(library (cdp)
  (export
    ;; Browsers
    browser-options browser-options? launch-browser browser? browser-close!
    call-with-browser with-browser browser-root-session browser-timeout browser-process
    browser-attach-target

    ;; Sessions and commands
    session? session-browser session-id cdp-call

    ;; Events
    with-events call-with-events events? events-wait event? event-method event-params
    event-session

    ;; Conditions
    &cdp-error cdp-error? cdp-error-code cdp-error-method
    &cdp-timeout make-cdp-timeout cdp-timeout? cdp-timeout-awaited cdp-timeout-last
    &browser-closed browser-closed? browser-closed-status
    &launch-error launch-error? launch-error-program)
  (import (chezscheme) (json) (process))

  (define-condition-type &cdp-error &error
    make-cdp-error cdp-error?
    (code cdp-error-code)
    (method cdp-error-method))

  (define-condition-type &cdp-timeout &error
    make-cdp-timeout cdp-timeout?
    (awaited cdp-timeout-awaited)
    (last cdp-timeout-last))

  (define-condition-type &browser-closed &error
    make-browser-closed browser-closed?
    (status browser-closed-status))

  (define-condition-type &launch-error &error
    make-launch-error launch-error?
    (program launch-error-program))

  (define (raise-timeout who awaited last)
    (raise
      (condition
        (make-cdp-timeout awaited last)
        (make-who-condition who)
        (make-message-condition (format "timed out waiting for ~a" awaited))
        (make-irritants-condition (if last (list last) '())))))

  (define-syntax check
    (syntax-rules ()
      [(_ who ok? x what)
       (let ([v x]) (unless (ok? v) (assertion-violationf who "~s is not ~a" v what)))]))

  (define (duration? x)
    (and (time? x) (eq? (time-type x) 'time-duration) (>= (time-second x) 0)))

  (define (deadline-after timeout)
    (add-duration (current-time 'time-utc) timeout))

  (define (past? deadline)
    (time>=? (current-time 'time-utc) deadline))

  (define (field message key)
    (cond [(assoc key message) => cdr] [else #f]))

  (define-record-type (options make-browser-options browser-options?)
    (nongenerative)
    (sealed #t)
    (fields headless profile log arguments timeout)
    (protocol
      (lambda (new)
        (lambda (headless profile log arguments timeout)
          (define who 'browser-options)
          (check who boolean? headless "a boolean")
          (check who (lambda (x) (or (not x) (string? x))) profile "a directory name or #f")
          (check who (lambda (x) (or (not x) (string? x))) log "a file name or #f")
          (check who (lambda (x) (and (list? x) (for-all string? x))) arguments
            "a list of strings")
          (check who duration? timeout "a nonnegative time-duration")
          (new headless profile log arguments timeout)))))

  (define omitted (list 'omitted))

  (define (option value default) (if (eq? value omitted) default value))

  (define (options-from headless profile log arguments timeout)
    (make-browser-options (option headless #t) (option profile #f) (option log #f)
      (option arguments '()) (option timeout (make-time 'time-duration 0 30))))

  (define-syntax browser-options
    (lambda (x)
      (define names '(headless profile log arguments timeout))
      (define (check-names given)
        (let loop ([given given] [seen '()])
          (unless (null? given)
            (let ([name (syntax->datum (car given))])
              (unless (memq name names)
                (syntax-violation 'browser-options "unknown option" x (car given)))
              (when (memq name seen)
                (syntax-violation 'browser-options "option given twice" x (car given)))
              (loop (cdr given) (cons name seen))))))
      (syntax-case x ()
        [(_ [name value] ...)
         (for-all identifier? #'(name ...))
         (begin
           (check-names #'(name ...))
           (with-syntax ([(temporary ...) (generate-temporaries #'(name ...))])
             (with-syntax ([(argument ...)
                            (let ([given (map cons (syntax->datum #'(name ...)) #'(temporary ...))])
                              (map (lambda (name)
                                     (cond [(assq name given) => cdr] [else #'omitted]))
                                   names))])
               #'(let* ([temporary value] ...) (options-from argument ...)))))]
        [_ (syntax-violation 'browser-options "expected (browser-options [name value] ...)" x)])))

  ;; The lock guards every mutable field of the browser, its pending calls
  ;; and its subscriptions. The writer mutex is separate so that a large
  ;; write never holds up the reader's deliveries.
  (define-record-type browser
    (nongenerative)
    (sealed #t)
    (fields process to from lock writer ended timeout owned-profile
      (mutable reader) (mutable next-id) pending (mutable subscriptions)
      (mutable closed) (mutable root) close-lock (mutable close-status))
    (protocol
      (lambda (new)
        (lambda (process timeout owned-profile)
          (new process (process-stdin process) (process-stdout process) (make-mutex) (make-mutex)
            (make-condition) timeout owned-profile #f 0 (make-eqv-hashtable) '() #f #f
            (make-mutex) #f)))))

  (define-record-type session
    (nongenerative)
    (sealed #t)
    (fields browser id))

  (define-record-type pending-call
    (nongenerative)
    (sealed #t)
    (fields ready (mutable reply))
    (protocol (lambda (new) (lambda () (new (make-condition) #f)))))

  (define-record-type pipe-end
    (nongenerative)
    (sealed #t)
    (fields reason))

  (define-record-type event
    (nongenerative)
    (sealed #t)
    (fields method params session))

  ;; Received is the reader's, under the lock; kept is touched only by the
  ;; thread that waits, outside it.
  (define-record-type (events make-events events?)
    (nongenerative)
    (sealed #t)
    (fields session methods ready (mutable received) (mutable kept) (mutable last)))

  (define (check-session who s) (check who session? s "a session"))
  (define (check-browser who b) (check who browser? b "a browser"))

  (define (browser-root-session b)
    (check-browser 'browser-root-session b)
    (browser-root b))

  (define (send! b message)
    (let ([bytes (json->bytevector message)] [port (browser-to b)])
      (with-mutex (browser-writer b)
        (put-bytevector port bytes)
        (put-u8 port 0)
        (flush-output-port port))))

  (define (read-messages port deliver!)
    (let-values ([(out take) (open-bytevector-output-port)])
      (let loop ()
        (let ([chunk (get-bytevector-some port)])
          (unless (eof-object? chunk)
            (let scan ([start 0])
              (let ([end (let find ([i start])
                           (cond
                             [(fx= i (bytevector-length chunk)) #f]
                             [(fx= (bytevector-u8-ref chunk i) 0) i]
                             [else (find (fx+ i 1))]))])
                (cond
                  [end
                   (put-bytevector out chunk start (fx- end start))
                   (deliver! (bytevector->json (take)))
                   (scan (fx+ end 1))]
                  [else
                   (put-bytevector out chunk start (fx- (bytevector-length chunk) start))
                   (loop)]))))))))

  (define (deliver! b message)
    (let ([id (field message "id")])
      (with-mutex (browser-lock b)
        (if id
            (let ([c (hashtable-ref (browser-pending b) id #f)])
              (when c
                (hashtable-delete! (browser-pending b) id)
                (pending-call-reply-set! c message)
                (condition-signal (pending-call-ready c))))
            (let ([method (field message "method")]
                  [target-session (field message "sessionId")])
              (for-each
                (lambda (s)
                  (when (and (equal? (session-id (events-session s)) target-session)
                             (member method (events-methods s)))
                    (events-received-set! s
                      (cons (make-event method (or (field message "params") '()) (events-session s))
                            (events-received s)))
                    (condition-signal (events-ready s))))
                (browser-subscriptions b)))))))

  ;; A failure here must reach the callers waiting on the browser: a
  ;; thread that ends on an unhandled error only prints it.
  (define (start-reader! b)
    (browser-reader-set! b
      (fork-thread
        (lambda ()
          (let ([reason (guard (c [#t c])
                          (read-messages (browser-from b) (lambda (m) (deliver! b m)))
                          "the browser closed its pipe")])
            (with-mutex (browser-lock b)
              (browser-closed-set! b (make-pipe-end reason))
              (condition-broadcast (browser-ended b))
              (vector-for-each (lambda (c) (condition-signal (pending-call-ready c)))
                (hashtable-values (browser-pending b)))
              (for-each (lambda (s) (condition-signal (events-ready s)))
                (browser-subscriptions b))))))))

  ;; The pipe ends a moment before the process does, so the status is
  ;; waited for briefly. The caller holds no lock.
  (define (raise-closed who b)
    (let ([reason (with-mutex (browser-lock b) (pipe-end-reason (browser-closed b)))]
          [status (process-wait (browser-process b) (make-time 'time-duration 0 1))])
      (raise
        (condition
          (make-browser-closed status)
          (make-who-condition who)
          (make-message-condition
            (if (string? reason)
                reason
                (format "reading from the browser failed: ~a"
                  (call-with-string-output-port (lambda (p) (display-condition reason p))))))
          (make-irritants-condition (if status (list status) '()))))))

  (define (next-id! b)
    (with-mutex (browser-lock b)
      (let ([id (+ (browser-next-id b) 1)])
        (browser-next-id-set! b id)
        id)))

  (define (command id session method params)
    (let ([base `(("id" . ,id) ("method" . ,method) ("params" . ,params))])
      (if (session-id session)
          (append base `(("sessionId" . ,(session-id session))))
          base)))

  (define (call-command who session method params timeout)
    (let* ([b (session-browser session)]
           [c (make-pending-call)]
           [id (with-mutex (browser-lock b)
                 (and (not (browser-closed b))
                      (let ([id (+ (browser-next-id b) 1)])
                        (browser-next-id-set! b id)
                        (hashtable-set! (browser-pending b) id c)
                        id)))]
           [deadline (deadline-after timeout)])
      (unless id (raise-closed who b))
      (guard (e [(i/o-error? e)
                 (with-mutex (browser-lock b) (hashtable-delete! (browser-pending b) id))
                 (unless (wait-for-pipe-end b (make-time 'time-duration 0 5))
                   (with-mutex (browser-lock b)
                     (browser-closed-set! b (make-pipe-end "the browser's pipe refused a write"))))
                 (raise-closed who b)])
        (send! b (command id session method params)))
      (let ([reply
             (with-mutex (browser-lock b)
               (let loop ()
                 (cond
                   [(pending-call-reply c)]
                   [(or (browser-closed b) (past? deadline))
                    (hashtable-delete! (browser-pending b) id)
                    (if (browser-closed b) 'closed 'timeout)]
                   [else (condition-wait (pending-call-ready c) (browser-lock b) deadline) (loop)])))])
        (cond
          [(eq? reply 'closed) (raise-closed who b)]
          [(eq? reply 'timeout) (raise-timeout who method #f)]
          [(field reply "error")
           => (lambda (e)
                (raise
                  (condition
                    (make-cdp-error (field e "code") method)
                    (make-who-condition who)
                    (make-message-condition (or (field e "message") "the browser refused the command"))
                    (make-irritants-condition
                      (cons method (cond [(field e "data") => list] [else '()]))))))]
          [else (or (field reply "result") '())]))))

  ;; A write fails when the browser has gone; the reader learns it from the
  ;; pipe's end shortly after, and its reason is the one to report.
  (define (wait-for-pipe-end b timeout)
    (let ([deadline (deadline-after timeout)])
      (with-mutex (browser-lock b)
        (let loop ()
          (unless (or (browser-closed b) (past? deadline))
            (condition-wait (browser-ended b) (browser-lock b) deadline)
            (loop)))
        (browser-closed b))))

  (define cdp-call
    (case-lambda
      [(session method) (cdp-call session method '())]
      [(session method params)
       (check-session 'cdp-call session)
       (cdp-call session method params (browser-timeout (session-browser session)))]
      [(session method params timeout)
       (check-session 'cdp-call session)
       (check 'cdp-call string? method "a method name")
       (check 'cdp-call list? params "an association list of parameters")
       (check 'cdp-call duration? timeout "a nonnegative time-duration")
       (call-command 'cdp-call session method params timeout)]))

  (define (browser-attach-target b target-id)
    (check-browser 'browser-attach-target b)
    (check 'browser-attach-target string? target-id "a target id")
    (let ([result (call-command 'browser-attach-target (browser-root b)
                    "Target.attachToTarget" `(("targetId" . ,target-id) ("flatten" . #t))
                    (browser-timeout b))])
      (make-session b (field result "sessionId"))))

  (define (call-with-events session methods proc)
    (check-session 'call-with-events session)
    (check 'call-with-events (lambda (x) (and (list? x) (for-all string? x))) methods
      "a list of event names")
    (check 'call-with-events procedure? proc "a procedure")
    (let ([b (session-browser session)]
          [s (make-events session methods (make-condition) '() '() #f)])
      (dynamic-wind
        (lambda ()
          (with-mutex (browser-lock b)
            (browser-subscriptions-set! b (cons s (browser-subscriptions b)))))
        (lambda () (proc s))
        (lambda ()
          (with-mutex (browser-lock b)
            (browser-subscriptions-set! b (remq s (browser-subscriptions b))))))))

  (define-syntax with-events
    (syntax-rules ()
      [(_ (name session method ...) body1 body2 ...)
       (identifier? #'name)
       (call-with-events session (list method ...) (lambda (name) body1 body2 ...))]))

  ;; The predicate runs outside the lock: it may itself call the browser.
  (define events-wait
    (case-lambda
      [(s predicate)
       (check 'events-wait events? s "an events subscription")
       (events-wait s predicate (browser-timeout (session-browser (events-session s))))]
      [(s predicate timeout)
       (check 'events-wait events? s "an events subscription")
       (check 'events-wait procedure? predicate "a procedure")
       (check 'events-wait duration? timeout "a nonnegative time-duration")
       (let ([b (session-browser (events-session s))]
             [deadline (deadline-after timeout)])
         (define (take-match! kept candidates)
           (let loop ([rest candidates] [skipped '()])
             (cond
               [(null? rest) (events-kept-set! s (append kept (reverse skipped))) #f]
               [else
                (events-last-set! s (car rest))
                (if (predicate (car rest))
                    (begin
                      (events-kept-set! s (append kept (reverse skipped) (cdr rest)))
                      (car rest))
                    (loop (cdr rest) (cons (car rest) skipped)))])))
         (or (take-match! '() (events-kept s))
             (let loop ()
               (let ([fresh
                      (with-mutex (browser-lock b)
                        (let wait ()
                          (cond
                            [(pair? (events-received s))
                             (let ([fresh (reverse (events-received s))])
                               (events-received-set! s '())
                               fresh)]
                            [(browser-closed b) 'closed]
                            [(past? deadline) 'timeout]
                            [else (condition-wait (events-ready s) (browser-lock b) deadline) (wait)])))])
                 (case fresh
                   [(closed) (raise-closed 'events-wait b)]
                   [(timeout)
                    (raise-timeout 'events-wait
                      (format "an event of ~{~a~^, ~}" (events-methods s)) (events-last s))]
                   [else (or (take-match! (events-kept s) fresh) (loop))])))))]))

  (define (temporary-directory)
    (let ([parent (or (getenv "TMPDIR") "/tmp")])
      (let retry ([n 0])
        (let* ([t (current-time 'time-utc)]
               [dir (format "~a/chez-cdp-~a-~a-~a" parent (time-second t) (time-nanosecond t) n)])
          (if (and (not (file-exists? dir #f)) (begin (mkdir dir #o700) #t))
              dir
              (if (< n 100)
                  (retry (+ n 1))
                  (errorf 'launch-browser "cannot make a profile directory in ~a" parent)))))))

  (define (delete-tree path)
    (if (file-directory? path #f)
        (begin
          (for-each (lambda (name) (delete-tree (path-build path name))) (directory-list path))
          (delete-directory path))
        (delete-file path)))

  (define (chromium-arguments options profile)
    (append
      (list "--remote-debugging-pipe"
            (string-append "--user-data-dir=" profile)
            ;; Without it Chromium on Linux asks the desktop keyring for a key
            ;; before its first network request, and waits forever where no
            ;; keyring answers.
            "--password-store=basic"
            "--no-first-run" "--no-default-browser-check" "--disable-extensions"
            "--disable-background-networking" "--disable-component-update"
            "--no-startup-window"
            ;; Several pages are in use at once, and a page in the background
            ;; would otherwise have its timers and frames throttled.
            "--disable-background-timer-throttling" "--disable-backgrounding-occluded-windows"
            "--disable-renderer-backgrounding")
      (if (options-headless options)
          (list "--headless" "--hide-scrollbars" "--mute-audio" "--force-color-profile=srgb"
                "--blink-settings=primaryHoverType=2,availableHoverTypes=2,primaryPointerType=4,availablePointerTypes=4")
          '())
      (options-arguments options)))

  (define default-options (browser-options))

  (define launch-browser
    (case-lambda
      [(program) (launch-browser program default-options)]
      [(program options)
       (check 'launch-browser string? program "a program name")
       (check 'launch-browser browser-options? options "browser options")
       (launch program options)]))

  (define (launch program options)
    (let* ([owned (and (not (options-profile options)) (temporary-directory))]
           [profile (or (options-profile options) owned)]
           [log (and (options-log options)
                     (open-file-output-port (options-log options) (file-options no-fail)))]
           [process
            (guard (e [(spawn-error? e)
                       (when log (close-port log))
                       (when owned (delete-tree owned))
                       (raise
                         (condition
                           (make-launch-error program)
                           (make-who-condition 'launch-browser)
                           (make-message-condition (condition-message e))
                           (make-irritants-condition (list program))))])
              ;; Chromium reads the protocol on descriptor 3 and writes it on
              ;; 4, and a spawned child gets only 0 to 2; the shell moves the
              ;; pipes there and execs Chromium, which keeps the shell's pid.
              (spawn-process "/bin/sh"
                (cons* "-c" "exec \"$0\" \"$@\" 3<&0 4>&1 0</dev/null 1>&2" program
                  (chromium-arguments options profile))
                (process-options
                  [stderr (or log (stdio null))]
                  [process-group 'new])))]
           [b (make-browser process (options-timeout options) owned)])
      (when log (close-port log))
      (browser-root-set! b (make-session b #f))
      (start-reader! b)
      (guard (e [(or (browser-closed? e) (cdp-timeout? e) (cdp-error? e))
                 (browser-close! b)
                 (raise
                   (condition
                     (make-launch-error program)
                     (make-who-condition 'launch-browser)
                     (make-message-condition
                       (format "~a did not answer over the pipe: ~a" program
                         (if (message-condition? e) (condition-message e) e)))
                     (make-irritants-condition
                       (append (list program)
                         (if (browser-closed? e) (list (browser-closed-status e)) '())))))])
        (call-command 'launch-browser (browser-root b) "Browser.getVersion" '()
          (options-timeout options)))
      b))

  ;; Chromium ends when its pipe closes, so a driver that dies leaves no
  ;; browser behind; the group is killed as well, for its helpers.
  (define (browser-close! b)
    (check-browser 'browser-close! b)
    (with-mutex (browser-close-lock b)
      (unless (browser-close-status b)
        (let ([p (browser-process b)])
          (unless (with-mutex (browser-lock b) (browser-closed b))
            (guard (e [(i/o-error? e) #f])
              (send! b `(("id" . ,(next-id! b)) ("method" . "Browser.close") ("params")))))
          (wait-for-pipe-end b (make-time 'time-duration 0 10))
          (process-group-signal! p 'kill)
          (browser-close-status-set! b (process-wait p))
          (guard (e [(i/o-error? e) #f]) (close-port (browser-to b)))
          (thread-join (browser-reader b))
          (close-port (browser-from b))
          (when (browser-owned-profile b)
            (delete-tree (browser-owned-profile b)))))))

  (define call-with-browser
    (case-lambda
      [(program proc) (call-with-browser program default-options proc)]
      [(program options proc)
       (check 'call-with-browser procedure? proc "a procedure")
       (let ([b (launch-browser program options)])
         (dynamic-wind
           void
           (lambda () (proc b))
           (lambda () (browser-close! b))))]))

  (define-syntax with-browser
    (syntax-rules ()
      [(_ (name program) body1 body2 ...)
       (identifier? #'name)
       (call-with-browser program (lambda (name) body1 body2 ...))]
      [(_ (name program options) body1 body2 ...)
       (identifier? #'name)
       (call-with-browser program options (lambda (name) body1 body2 ...))])))
