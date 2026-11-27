(import (chezscheme) (process) (cdp) (test) (support))

(define (field alist key) (cdr (assoc key alist)))

(define (milliseconds n) (make-time 'time-duration (* n 1000000) 0))

(define (open-target b)
  (let ([target (field (cdp-call (browser-root-session b) "Target.createTarget"
                         '(("url" . "about:blank")))
                       "targetId")])
    (browser-attach-target b target)))

;; Chromium keeps a directory of its own beside the profile while it runs.
(define (profiles)
  (filter (lambda (name) (and (>= (string-length name) 9) (string=? (substring name 0 9) "chez-cdp-")))
    (directory-list (getenv "TMPDIR"))))

(test "launch-browser starts Chromium, and browser-close! ends it and its profile"
  (let ([b (launch-browser chromium)])
    (let ([product (field (cdp-call (browser-root-session b) "Browser.getVersion") "product")])
      (check-equal? "Chrome/" (substring product 0 7)))
    (check-equal? 1 (length (profiles)))
    (browser-close! b)
    (check (exit-status? (process-status (browser-process b))))
    (check (not (group-running? (process-group-id (browser-process b)))))
    (check-equal? '() (profiles))
    (browser-close! b)
    (check-raises browser-closed? (cdp-call (browser-root-session b) "Browser.getVersion"))))

(test "a program that is not Chromium raises a launch error"
  (check-raises launch-error? (launch-browser "/nonexistent/chromium"))
  (check-raises launch-error?
    (launch-browser "/bin/true" (browser-options [timeout (make-time 'time-duration 0 5)])))
  (check-equal? '() (profiles)))

(test "options are checked when they are made"
  (check-raises assertion-violation? (browser-options [headless 'yes]))
  (check-raises assertion-violation? (browser-options [timeout 30]))
  (check-raises assertion-violation? (launch-browser 'chromium)))

(with-browser (b chromium (browser-options [log (string-append (getenv "TMPDIR") "/log")]))
  (define root (browser-root-session b))

  (test "a command the browser refuses raises a CDP error with its code and method"
    (guard (e [(cdp-error? e)
               (check-equal? -32601 (cdp-error-code e))
               (check-equal? "No.such" (cdp-error-method e))])
      (cdp-call root "No.such")
      (check #f)))

  (test "a call raises a timeout when the reply is late, and the browser goes on"
    (let ([s (open-target b)])
      (guard (e [(cdp-timeout? e) (check-equal? "Runtime.evaluate" (cdp-timeout-awaited e))])
        (cdp-call s "Runtime.evaluate"
          '(("expression" . "new Promise(r => setTimeout(r, 2000))") ("awaitPromise" . #t))
          (milliseconds 200))
        (check #f))
      (check-equal? 3
        (field (field (cdp-call s "Runtime.evaluate" '(("expression" . "1 + 2"))) "result")
               "value"))))

  (test "events reach a waiting thread as they happen, from their own session only"
    (let ([s (open-target b)] [other (open-target b)])
      (cdp-call s "Runtime.enable")
      (cdp-call other "Runtime.enable")
      (with-events (logged s "Runtime.consoleAPICalled")
        (cdp-call s "Runtime.evaluate"
          '(("expression" . "setTimeout(() => console.log('late'), 300)")))
        (cdp-call other "Runtime.evaluate" '(("expression" . "console.log('other')")))
        (let* ([e (events-wait logged (lambda (e) #t))]
               [received (current-time 'time-utc)]
               [stamp (field (event-params e) "timestamp")]
               [lag (- (+ (* 1000 (time-second received)) (/ (time-nanosecond received) 1e6)) stamp)])
          (check-equal? "Runtime.consoleAPICalled" (event-method e))
          (check (eq? s (event-session e)))
          (check-equal? "late"
            (field (vector-ref (field (event-params e) "args") 0) "value"))
          (check (< lag 100)))
        (check-raises cdp-timeout? (events-wait logged (lambda (e) #t) (milliseconds 200))))))

  (test "an event a wait passes over stays for the next wait"
    (let ([s (open-target b)])
      (cdp-call s "Runtime.enable")
      (with-events (logged s "Runtime.consoleAPICalled")
        (cdp-call s "Runtime.evaluate" '(("expression" . "console.log('one'); console.log('two')")))
        (let ([text (lambda (e) (field (vector-ref (field (event-params e) "args") 0) "value"))])
          (check-equal? "two" (text (events-wait logged (lambda (e) (equal? (text e) "two")))))
          (check-equal? "one" (text (events-wait logged (lambda (e) #t))))))))

  (test "calls from several threads at once each get their own reply"
    (let* ([results (make-vector 4 #f)]
           [threads
            (map (lambda (i)
                   (fork-thread
                     (lambda ()
                       (vector-set! results i
                         (guard (e [#t e])
                           (let loop ([n 0])
                             (if (= n 50)
                                 'done
                                 (let ([r (cdp-call root "Target.getTargets")])
                                   (if (vector? (field r "targetInfos")) (loop (+ n 1)) r)))))))))
                 '(0 1 2 3))])
      (for-each thread-join threads)
      (check-equal? '#(done done done done) results))))

(test "with-browser closes the browser when its body raises"
  (let ([kept #f])
    (guard (e [(string? e) #f])
      (with-browser (b chromium)
        (set! kept b)
        (raise "leaving")))
    (check-raises browser-closed? (cdp-call (browser-root-session kept) "Browser.getVersion"))
    (check (not (group-running? (process-group-id (browser-process kept)))))))

(test "a browser that dies fails the calls waiting on it"
  (let* ([b (launch-browser chromium)] [s (open-target b)] [outcome #f])
    (let ([waiter (fork-thread
                    (lambda ()
                      (set! outcome
                        (guard (e [(browser-closed? e) 'closed])
                          (cdp-call s "Runtime.evaluate"
                            '(("expression" . "new Promise(() => {})") ("awaitPromise" . #t)))))))])
      (sleep (milliseconds 200))
      (process-group-signal! (browser-process b) 'kill)
      (thread-join waiter)
      (check-equal? 'closed outcome)
      (check-raises browser-closed? (cdp-call (browser-root-session b) "Browser.getVersion"))
      (browser-close! b)
      (check-equal? 9 (exit-status-signal (process-status (browser-process b)))))))
