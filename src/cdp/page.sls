(library (cdp page)
  (export
    page-options page-options? color-scheme open-page page? page-browser page-session
    page-close! page-navigate!
    &navigation-error navigation-error? navigation-error-url

    page-query element? element-page element-selector element-index element-backend-node-id
    element-text element-attribute element-style element-box
    &element-not-found element-not-found? element-not-found-selector element-not-found-index
    element-state page-wait-for
    page-snapshot snapshot-node? snapshot-node-backend-node-id snapshot-node-name
    snapshot-node-value snapshot-node-parent snapshot-node-attributes snapshot-node-bounds
    snapshot-node-text snapshot-node-styles
    page-screenshot
    make-rectangle rectangle? rectangle-x rectangle-y rectangle-width rectangle-height

    modifiers page-press! page-type!
    make-point point? point-x point-y element-click! element-hover! page-wheel! page-drag!)
  (import (chezscheme) (cdp))

  (define-condition-type &navigation-error &error
    make-navigation-error navigation-error?
    (url navigation-error-url))

  (define-condition-type &element-not-found &error
    make-element-not-found element-not-found?
    (selector element-not-found-selector)
    (index element-not-found-index))

  (define-syntax check
    (syntax-rules ()
      [(_ who ok? x what)
       (let ([v x]) (unless (ok? v) (assertion-violationf who "~s is not ~a" v what)))]))

  (define (duration? x)
    (and (time? x) (eq? (time-type x) 'time-duration) (>= (time-second x) 0)))

  (define (field message key)
    (cond [(assoc key message) => cdr] [else #f]))

  (define (raise-timeout who awaited last)
    (raise
      (condition
        (make-cdp-timeout awaited last)
        (make-who-condition who)
        (make-message-condition (format "timed out waiting for ~a" awaited))
        (make-irritants-condition (if last (list last) '())))))

  (define poll-interval (make-time 'time-duration 50000000 0))

  (define-record-type rectangle
    (nongenerative)
    (sealed #t)
    (fields x y width height)
    (protocol
      (lambda (new)
        (lambda (x y width height)
          (for-each (lambda (n) (check 'make-rectangle real? n "a real number")) (list x y))
          (for-each (lambda (n) (check 'make-rectangle (lambda (n) (and (real? n) (>= n 0))) n
                                  "a nonnegative real number"))
            (list width height))
          (new x y width height)))))

  (define-record-type point
    (nongenerative)
    (sealed #t)
    (fields x y)
    (protocol
      (lambda (new)
        (lambda (x y)
          (check 'make-point real? x "a real number")
          (check 'make-point real? y "a real number")
          (new x y)))))

  (define-enumeration color-scheme (light dark) color-schemes)

  (define-record-type (options make-page-options page-options?)
    (nongenerative)
    (sealed #t)
    (fields width height shared-storage color-scheme)
    (protocol
      (lambda (new)
        (lambda (width height shared-storage color-scheme)
          (define who 'page-options)
          (define (size? x) (and (fixnum? x) (fx> x 0)))
          (check who size? width "a positive width")
          (check who size? height "a positive height")
          (check who boolean? shared-storage "a boolean")
          (check who (lambda (x) (or (not x) (enum-set-member? x (enum-set-universe (color-schemes))))) color-scheme
            "a color scheme or #f")
          (new width height shared-storage color-scheme)))))

  (define omitted (list 'omitted))

  (define (option value default) (if (eq? value omitted) default value))

  (define (options-from width height shared-storage color-scheme)
    (make-page-options (option width 1280) (option height 800) (option shared-storage #f)
      (option color-scheme #f)))

  (define-syntax page-options
    (lambda (x)
      (define names '(width height shared-storage color-scheme))
      (define (check-names given)
        (let loop ([given given] [seen '()])
          (unless (null? given)
            (let ([name (syntax->datum (car given))])
              (unless (memq name names)
                (syntax-violation 'page-options "unknown option" x (car given)))
              (when (memq name seen)
                (syntax-violation 'page-options "option given twice" x (car given)))
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
        [_ (syntax-violation 'page-options "expected (page-options [name value] ...)" x)])))

  (define default-options (page-options))

  (define-record-type page
    (nongenerative)
    (sealed #t)
    (fields browser session target context (mutable closed?)))

  (define (check-page who p) (check who page? p "a page"))

  (define open-page
    (case-lambda
      [(browser url) (open-page browser url default-options)]
      [(browser url options)
       (check 'open-page browser? browser "a browser")
       (check 'open-page string? url "a URL")
       (check 'open-page page-options? options "page options")
       (let* ([root (browser-root-session browser)]
              [context (and (not (options-shared-storage options))
                            (field (cdp-call root "Target.createBrowserContext")
                                   "browserContextId"))]
              [target (field (cdp-call root "Target.createTarget"
                               (cons '("url" . "about:blank")
                                     (if context `(("browserContextId" . ,context)) '())))
                             "targetId")]
              [p (make-page browser (browser-attach-target browser target) target context #f)]
              [s (page-session p)])
         (guard (e [#t (page-close! p) (raise e)])
           (cdp-call s "Page.enable")
           (cdp-call s "Page.setLifecycleEventsEnabled" '(("enabled" . #t)))
           (cdp-call s "DOM.enable")
           (cdp-call s "CSS.enable")
           (cdp-call s "Emulation.setDeviceMetricsOverride"
             `(("width" . ,(options-width options)) ("height" . ,(options-height options))
               ("deviceScaleFactor" . 1) ("mobile" . #f)))
           (let ([scheme (options-color-scheme options)])
             (when scheme
               (cdp-call s "Emulation.setEmulatedMedia"
                 `(("features" . #((("name" . "prefers-color-scheme")
                                    ("value" . ,(symbol->string scheme)))))))))
           (navigate 'open-page p url))
         p)]))

  ;; The load is matched by its loader, so the initial blank page's own
  ;; load, which can arrive late, is not taken for this one.
  (define (navigate who p url)
    (let ([s (page-session p)])
      (with-events (lifecycle s "Page.lifecycleEvent")
        (let* ([result (cdp-call s "Page.navigate" `(("url" . ,url)))]
               [loader (field result "loaderId")])
          (cond
            [(field result "errorText")
             => (lambda (text)
                  (raise
                    (condition
                      (make-navigation-error url)
                      (make-who-condition who)
                      (make-message-condition (format "cannot load ~a: ~a" url text))
                      (make-irritants-condition (list url)))))]
            [loader
             (events-wait lifecycle
               (lambda (e)
                 (let ([params (event-params e)])
                   (and (equal? (field params "name") "load")
                        (equal? (field params "loaderId") loader)))))])))))

  (define (page-navigate! p url)
    (check-page 'page-navigate! p)
    (check 'page-navigate! string? url "a URL")
    (navigate 'page-navigate! p url)
    (void))

  (define (page-close! p)
    (check-page 'page-close! p)
    (unless (page-closed? p)
      (page-closed?-set! p #t)
      (guard (e [(browser-closed? e) #f])
        ;; Disposing a context closes its targets. Closing the target first
        ;; and disposing its context straight after crashed Chromium 146
        ;; when the machine was busy.
        (let ([root (browser-root-session (page-browser p))])
          (if (page-context p)
              (cdp-call root "Target.disposeBrowserContext"
                `(("browserContextId" . ,(page-context p))))
              (cdp-call root "Target.closeTarget" `(("targetId" . ,(page-target p)))))))))

  ;; An element is its selector and index, looked up again on every use:
  ;; a node the page has replaced stays valid in the protocol, detached,
  ;; and would still answer with its old content.
  (define-record-type element
    (nongenerative)
    (sealed #t)
    (fields page selector index))

  (define (check-element who e) (check who element? e "an element"))

  (define (query-node-ids p selector)
    (let* ([s (page-session p)]
           [root (field (field (cdp-call s "DOM.getDocument" '(("depth" . 0))) "root") "nodeId")])
      (field (cdp-call s "DOM.querySelectorAll" `(("nodeId" . ,root) ("selector" . ,selector)))
             "nodeIds")))

  (define (page-query p selector)
    (check-page 'page-query p)
    (check 'page-query string? selector "a selector")
    (let ([count (vector-length (query-node-ids p selector))])
      (let loop ([i 0])
        (if (fx= i count)
            '()
            (cons (make-element p selector i) (loop (fx+ i 1)))))))

  (define (resolve who e)
    (let ([ids (query-node-ids (element-page e) (element-selector e))])
      (unless (fx< (element-index e) (vector-length ids))
        (raise
          (condition
            (make-element-not-found (element-selector e) (element-index e))
            (make-who-condition who)
            (make-message-condition
              (format "no element ~a matches ~a" (element-index e) (element-selector e)))
            (make-irritants-condition (list (element-selector e) (element-index e))))))
      (field (field (cdp-call (page-session (element-page e)) "DOM.describeNode"
                      `(("nodeId" . ,(vector-ref ids (element-index e)))))
                    "node")
             "backendNodeId")))

  ;; The page may replace the node between the lookup and its use; one more
  ;; lookup settles that, and a second refusal is a real one.
  (define (with-node who e proc)
    (guard (c [(cdp-error? c) (proc (resolve who e))])
      (proc (resolve who e))))

  (define (element-backend-node-id e)
    (check-element 'element-backend-node-id e)
    (resolve 'element-backend-node-id e))

  (define (describe e id depth)
    (field (cdp-call (page-session (element-page e)) "DOM.describeNode"
             `(("backendNodeId" . ,id) ("depth" . ,depth)))
           "node"))

  (define (children node) (vector->list (or (field node "children") '#())))

  (define (element-text e)
    (check-element 'element-text e)
    (with-node 'element-text e
      (lambda (id)
        (call-with-string-output-port
          (lambda (out)
            (let walk ([node (describe e id -1)])
              (if (eqv? (field node "nodeType") 3)
                  (put-string out (field node "nodeValue"))
                  (for-each walk (children node)))))))))

  (define (element-attribute e name)
    (check-element 'element-attribute e)
    (check 'element-attribute string? name "an attribute name")
    (with-node 'element-attribute e
      (lambda (id)
        (let loop ([attributes (vector->list (or (field (describe e id 0) "attributes") '#()))])
          (cond
            [(null? attributes) #f]
            [(string=? (car attributes) name) (cadr attributes)]
            [else (loop (cddr attributes))])))))

  (define (computed-style e id)
    (let ([s (page-session (element-page e))])
      (cdp-call s "DOM.getDocument" '(("depth" . 0)))
      (let ([node-id (vector-ref (field (cdp-call s "DOM.pushNodesByBackendIdsToFrontend"
                                         `(("backendNodeIds" . ,(vector id))))
                                       "nodeIds")
                                0)])
        (vector->list
          (field (cdp-call s "CSS.getComputedStyleForNode" `(("nodeId" . ,node-id)))
                 "computedStyle")))))

  (define (element-style e property)
    (check-element 'element-style e)
    (check 'element-style string? property "a property name")
    (with-node 'element-style e
      (lambda (id)
        (cond
          [(find (lambda (p) (string=? (field p "name") property)) (computed-style e id))
           => (lambda (p) (field p "value"))]
          [else #f]))))

  (define (quad->rectangle quad)
    (let* ([xs (list (vector-ref quad 0) (vector-ref quad 2) (vector-ref quad 4) (vector-ref quad 6))]
           [ys (list (vector-ref quad 1) (vector-ref quad 3) (vector-ref quad 5) (vector-ref quad 7))]
           [x (apply min xs)]
           [y (apply min ys)])
      (make-rectangle x y (- (apply max xs) x) (- (apply max ys) y))))

  (define (border-box e id)
    (quad->rectangle
      (field (field (cdp-call (page-session (element-page e)) "DOM.getBoxModel"
                      `(("backendNodeId" . ,id)))
                    "model")
             "border")))

  (define (element-box e)
    (check-element 'element-box e)
    (with-node 'element-box e (lambda (id) (border-box e id))))

  (define-enumeration element-state (attached visible hidden) element-states)

  (define (visible? e id)
    (guard (c [(cdp-error? c) #f])
      (let ([box (border-box e id)])
        (and (> (rectangle-width box) 0) (> (rectangle-height box) 0)
             (not (member (cond
                            [(find (lambda (p) (string=? (field p "name") "visibility"))
                               (computed-style e id))
                             => (lambda (p) (field p "value"))]
                            [else "visible"])
                          '("hidden" "collapse")))))))

  ;; Nodes can vanish between the query and the reads that follow it, so a
  ;; refused read counts as "not yet" and is reported if the wait times out.
  (define page-wait-for
    (case-lambda
      [(p selector state)
       (check-page 'page-wait-for p)
       (page-wait-for p selector state (browser-timeout (page-browser p)))]
      [(p selector state timeout)
       (check-page 'page-wait-for p)
       (check 'page-wait-for string? selector "a selector")
       (check 'page-wait-for (lambda (x) (enum-set-member? x (enum-set-universe (element-states)))) state
         "an element state")
       (check 'page-wait-for duration? timeout "a nonnegative time-duration")
       (let ([deadline (add-duration (current-time 'time-utc) timeout)])
         (let loop ()
           (let-values ([(found last)
                         (guard (c [(or (cdp-error? c) (element-not-found? c))
                                    (values #f (condition-message c))])
                           (let* ([elements (page-query p selector)]
                                  [shown (lambda ()
                                           (find (lambda (e) (visible? e (resolve 'page-wait-for e)))
                                             elements))])
                             (case state
                               [(attached)
                                (values (and (pair? elements) (car elements)) "no element matched")]
                               [(visible)
                                (values (shown)
                                  (format "~a matching elements, none visible" (length elements)))]
                               [else
                                (values (not (shown))
                                  (format "~a matching elements, some visible" (length elements)))])))])
             (cond
               [found (if (element? found) found (void))]
               [(time>=? (current-time 'time-utc) deadline)
                (raise-timeout 'page-wait-for (format "~a to be ~a" selector state) last)]
               [else (sleep poll-interval) (loop)]))))]))

  (define-record-type snapshot-node
    (nongenerative)
    (sealed #t)
    (fields backend-node-id name value parent attributes bounds text styles))

  (define (page-snapshot p properties)
    (check-page 'page-snapshot p)
    (check 'page-snapshot (lambda (x) (and (list? x) (for-all string? x))) properties
      "a list of property names")
    (let* ([result (cdp-call (page-session p) "DOMSnapshot.captureSnapshot"
                     `(("computedStyles" . ,(list->vector properties))))]
           [strings (field result "strings")]
           [document (vector-ref (field result "documents") 0)]
           [nodes (field document "nodes")]
           [layout (field document "layout")]
           [layout-of (make-eqv-hashtable)])
      (define (text-at i) (and (fx>= i 0) (vector-ref strings i)))
      (let ([indexes (field layout "nodeIndex")])
        (do ([k (fx- (vector-length indexes) 1) (fx- k 1)]) ((fx< k 0))
          (hashtable-set! layout-of (vector-ref indexes k) k)))
      (let ([parents (field nodes "parentIndex")]
            [names (field nodes "nodeName")]
            [node-values (field nodes "nodeValue")]
            [backend-ids (field nodes "backendNodeId")]
            [attributes (field nodes "attributes")]
            [bounds (field layout "bounds")]
            [texts (field layout "text")]
            [styles (field layout "styles")])
        (let ([snapshot (make-vector (vector-length parents))])
          (do ([i 0 (fx+ i 1)]) ((fx= i (vector-length parents)) snapshot)
            (let ([k (hashtable-ref layout-of i #f)])
              (vector-set! snapshot i
                (make-snapshot-node
                  (vector-ref backend-ids i)
                  (vector-ref strings (vector-ref names i))
                  (text-at (vector-ref node-values i))
                  (let ([parent (vector-ref parents i)]) (and (fx>= parent 0) parent))
                  (let loop ([pairs (vector->list (vector-ref attributes i))])
                    (if (null? pairs)
                        '()
                        (cons (cons (vector-ref strings (car pairs)) (vector-ref strings (cadr pairs)))
                              (loop (cddr pairs)))))
                  (and k (let ([b (vector-ref bounds k)])
                           (make-rectangle (vector-ref b 0) (vector-ref b 1) (vector-ref b 2)
                             (vector-ref b 3))))
                  (and k (text-at (vector-ref texts k)))
                  (let ([style (if k (vector->list (vector-ref styles k)) '())])
                    ;; The document node has layout but no styles.
                    (if (fx= (length style) (length properties))
                        (map (lambda (name value) (cons name (vector-ref strings value)))
                             properties style)
                        '()))))))))))

  (define base64-values
    (let ([table (make-bytevector 256 255)]
          [alphabet "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/"])
      (do ([i 0 (fx+ i 1)]) ((fx= i 64) table)
        (bytevector-u8-set! table (char->integer (string-ref alphabet i)) i))))

  (define (base64->bytevector s)
    (let* ([n (let trim ([n (string-length s)])
                (if (and (fx> n 0) (char=? (string-ref s (fx- n 1)) #\=)) (trim (fx- n 1)) n))]
           [out (make-bytevector (fxquotient (fx* n 3) 4))])
      (define (digit i) (bytevector-u8-ref base64-values (char->integer (string-ref s i))))
      (let loop ([i 0] [o 0] [bits 0] [count 0])
        (cond
          [(fx= i n) out]
          [else
           (let ([bits (fxior (fxsll (fxand bits #xffff) 6) (digit i))]
                 [count (fx+ count 6)])
             (if (fx>= count 8)
                 (begin
                   (bytevector-u8-set! out o (fxand (fxsrl bits (fx- count 8)) 255))
                   (loop (fx+ i 1) (fx+ o 1) bits (fx- count 8)))
                 (loop (fx+ i 1) o bits count)))]))))

  (define page-screenshot
    (case-lambda
      [(p)
       (check-page 'page-screenshot p)
       (screenshot p '(("format" . "png")))]
      [(p clip)
       (check-page 'page-screenshot p)
       (check 'page-screenshot rectangle? clip "a rectangle")
       (screenshot p
         `(("format" . "png")
           ("clip" . (("x" . ,(inexact (rectangle-x clip))) ("y" . ,(inexact (rectangle-y clip)))
                      ("width" . ,(inexact (rectangle-width clip)))
                      ("height" . ,(inexact (rectangle-height clip)))
                      ("scale" . 1)))))]))

  (define (screenshot p params)
    (base64->bytevector (field (cdp-call (page-session p) "Page.captureScreenshot" params) "data")))

  (define-enumeration modifier (alt control meta shift) modifiers)

  ;; Codes and key codes are a US keyboard's.
  (define-record-type key
    (nongenerative)
    (sealed #t)
    (fields name code key-code text shifted-name needs-shift?))

  (define named-keys
    (map (lambda (entry)
           (let-values ([(name code key-code text) (apply values entry)])
             (make-key name code key-code text #f #f)))
      '(("Enter" "Enter" 13 "\r") ("Tab" "Tab" 9 #f) ("Escape" "Escape" 27 #f)
        ("Backspace" "Backspace" 8 #f) ("Delete" "Delete" 46 #f) ("Insert" "Insert" 45 #f)
        (" " "Space" 32 " ") ("Home" "Home" 36 #f) ("End" "End" 35 #f)
        ("PageUp" "PageUp" 33 #f) ("PageDown" "PageDown" 34 #f)
        ("ArrowLeft" "ArrowLeft" 37 #f) ("ArrowUp" "ArrowUp" 38 #f)
        ("ArrowRight" "ArrowRight" 39 #f) ("ArrowDown" "ArrowDown" 40 #f)
        ("F1" "F1" 112 #f) ("F2" "F2" 113 #f) ("F3" "F3" 114 #f) ("F4" "F4" 115 #f)
        ("F5" "F5" 116 #f) ("F6" "F6" 117 #f) ("F7" "F7" 118 #f) ("F8" "F8" 119 #f)
        ("F9" "F9" 120 #f) ("F10" "F10" 121 #f) ("F11" "F11" 122 #f) ("F12" "F12" 123 #f))))

  (define (character-keys code key-code plain shifted)
    (list (make-key plain code key-code plain shifted #f)
          (make-key shifted code key-code shifted #f #t)))

  (define typing-keys
    (apply append
      (map (lambda (entry) (apply character-keys entry))
        (append
          (map (lambda (c)
                 (let ([upper (char-upcase c)])
                   (list (string #\K #\e #\y upper) (char->integer upper) (string c) (string upper))))
               (string->list "abcdefghijklmnopqrstuvwxyz"))
          '(("Backquote" 192 "`" "~") ("Minus" 189 "-" "_") ("Equal" 187 "=" "+")
            ("BracketLeft" 219 "[" "{") ("BracketRight" 221 "]" "}") ("Backslash" 220 "\\" "|")
            ("Semicolon" 186 ";" ":") ("Quote" 222 "'" "\"") ("Comma" 188 "," "<")
            ("Period" 190 "." ">") ("Slash" 191 "/" "?")
            ("Digit0" 48 "0" ")") ("Digit1" 49 "1" "!") ("Digit2" 50 "2" "@") ("Digit3" 51 "3" "#")
            ("Digit4" 52 "4" "$") ("Digit5" 53 "5" "%") ("Digit6" 54 "6" "^") ("Digit7" 55 "7" "&")
            ("Digit8" 56 "8" "*") ("Digit9" 57 "9" "("))))))

  (define-record-type modifier-key
    (nongenerative)
    (sealed #t)
    (fields modifier bit key))

  (define modifier-keys
    (list (make-modifier-key 'alt 1 (make-key "Alt" "AltLeft" 18 #f #f #f))
          (make-modifier-key 'control 2 (make-key "Control" "ControlLeft" 17 #f #f #f))
          (make-modifier-key 'meta 4 (make-key "Meta" "MetaLeft" 91 #f #f #f))
          (make-modifier-key 'shift 8 (make-key "Shift" "ShiftLeft" 16 #f #f #f))))

  (define (find-key name)
    (find (lambda (k) (string=? (key-name k) name))
      (append named-keys (map modifier-key-key modifier-keys) typing-keys)))

  (define (key-event! p type k mask text)
    (cdp-call (page-session p) "Input.dispatchKeyEvent"
      (append
        `(("type" . ,type) ("key" . ,(key-name k)) ("code" . ,(key-code k))
          ("windowsVirtualKeyCode" . ,(key-key-code k)) ("modifiers" . ,mask))
        (if text `(("text" . ,text) ("unmodifiedText" . ,text)) '()))))

  ;; Each modifier is its own key, pressed before and released after, as a
  ;; page that tracks AltLeft or ShiftLeft expects; a shifted character
  ;; holds Shift, as a hand does. Only a key that types sends text, and none
  ;; does while a modifier other than Shift is held.
  (define (press! p k held)
    (let* ([held (if (key-needs-shift? k) (cons 'shift held) held)]
           [down (filter (lambda (m) (memq (modifier-key-modifier m) held)) modifier-keys)]
           [mask (fold-left (lambda (bits m) (fxior bits (modifier-key-bit m))) 0 down)]
           [own (find (lambda (m) (eq? (modifier-key-key m) k)) modifier-keys)]
           [k (if (and (memq 'shift held) (key-shifted-name k)) (find-key (key-shifted-name k)) k)]
           [text (and (fx= (fxand mask (fxnot 8)) 0) (key-text k))])
      (let press ([ms down] [bits 0])
        (unless (null? ms)
          (let ([bits (fxior bits (modifier-key-bit (car ms)))])
            (key-event! p "rawKeyDown" (modifier-key-key (car ms)) bits #f)
            (press (cdr ms) bits))))
      (key-event! p (if text "keyDown" "rawKeyDown") k
        (if own (fxior mask (modifier-key-bit own)) mask) text)
      (key-event! p "keyUp" k mask #f)
      (let release ([ms (reverse down)] [bits mask])
        (unless (null? ms)
          (let ([bits (fxand bits (fxnot (modifier-key-bit (car ms))))])
            (key-event! p "keyUp" (modifier-key-key (car ms)) bits #f)
            (release (cdr ms) bits))))))

  (define page-press!
    (case-lambda
      [(p name) (page-press! p name (modifiers))]
      [(p name held)
       (check-page 'page-press! p)
       (check 'page-press! string? name "a key name")
       (check 'page-press!
         (lambda (x) (and (enum-set? x) (enum-set-subset? x (modifiers alt control meta shift))))
         held "a set of modifiers")
       (press! p (or (find-key name) (assertion-violationf 'page-press! "~s is not a key" name))
         (enum-set->list held))]))

  ;; A character no key types arrives as inserted text, as from an input
  ;; method.
  (define (page-type! p text)
    (check-page 'page-type! p)
    (check 'page-type! string? text "a string")
    (string-for-each
      (lambda (c)
        (let ([k (case c
                   [(#\newline) (find-key "Enter")]
                   [(#\tab) (find-key "Tab")]
                   [else (find-key (string c))])])
          (if k
              (press! p k '())
              (cdp-call (page-session p) "Input.insertText" `(("text" . ,(string c)))))))
      text))

  (define (mouse! p type at button buttons)
    (cdp-call (page-session p) "Input.dispatchMouseEvent"
      `(("type" . ,type) ("x" . ,(inexact (point-x at))) ("y" . ,(inexact (point-y at)))
        ("button" . ,button)
        ("buttons" . ,buttons) ("clickCount" . 1))))

  (define (subtree-ids e id)
    (let walk ([node (describe e id -1)] [acc '()])
      (fold-left (lambda (acc child) (walk child acc))
        (cons (field node "backendNodeId") acc)
        (children node))))

  ;; The element's centre once it is in view and a pointer there would hit
  ;; it or something inside it, rather than whatever covers it; tried
  ;; again until the deadline, since pages move and replace elements.
  (define (actionable-point who e)
    (let* ([p (element-page e)]
           [s (page-session p)]
           [deadline (add-duration (current-time 'time-utc) (browser-timeout (page-browser p)))])
      (define (attempt id)
        (cdp-call s "DOM.scrollIntoViewIfNeeded" `(("backendNodeId" . ,id)))
        (let ([box (border-box e id)])
          (if (or (zero? (rectangle-width box)) (zero? (rectangle-height box)))
              "the element has no size"
              (let* ([x (+ (rectangle-x box) (/ (rectangle-width box) 2))]
                     [y (+ (rectangle-y box) (/ (rectangle-height box) 2))]
                     [hit (field (cdp-call s "DOM.getNodeForLocation"
                                   `(("x" . ,(exact (floor x))) ("y" . ,(exact (floor y)))))
                                 "backendNodeId")])
                (if (memv hit (subtree-ids e id))
                    (make-point (inexact x) (inexact y))
                    "another element covers it")))))
      (let loop ()
        (let ([outcome (guard (c [(or (cdp-error? c) (element-not-found? c)) (condition-message c)])
                         (with-node who e attempt))])
          (cond
            [(point? outcome) outcome]
            [(time>=? (current-time 'time-utc) deadline)
             (raise-timeout who (format "element ~a of ~a to take the pointer" (element-index e)
                                  (element-selector e))
               outcome)]
            [else (sleep poll-interval) (loop)])))))

  (define (element-click! e)
    (check-element 'element-click! e)
    (let ([p (element-page e)] [at (actionable-point 'element-click! e)])
      (mouse! p "mouseMoved" at "none" 0)
      (mouse! p "mousePressed" at "left" 1)
      (mouse! p "mouseReleased" at "left" 0)
      (void)))

  (define (element-hover! e)
    (check-element 'element-hover! e)
    (mouse! (element-page e) "mouseMoved" (actionable-point 'element-hover! e) "none" 0)
    (void))

  (define (page-wheel! p at dx dy)
    (check-page 'page-wheel! p)
    (check 'page-wheel! point? at "a point")
    (check 'page-wheel! real? dx "a real number")
    (check 'page-wheel! real? dy "a real number")
    (cdp-call (page-session p) "Input.dispatchMouseEvent"
      `(("type" . "mouseWheel") ("x" . ,(inexact (point-x at))) ("y" . ,(inexact (point-y at)))
        ("deltaX" . ,(inexact dx)) ("deltaY" . ,(inexact dy))))
    (void))

  ;; Moves in steps with the button held, as a hand does: a page that
  ;; tracks the drag sees intermediate positions, and a scroll bar follows.
  (define (page-drag! p from to)
    (check-page 'page-drag! p)
    (check 'page-drag! point? from "a point")
    (check 'page-drag! point? to "a point")
    (let ([steps 10])
      (mouse! p "mouseMoved" from "none" 0)
      (mouse! p "mousePressed" from "left" 1)
      (do ([i 1 (fx+ i 1)]) ((fx> i steps))
        (mouse! p "mouseMoved"
          (make-point (+ (point-x from) (/ (* i (- (point-x to) (point-x from))) steps))
                      (+ (point-y from) (/ (* i (- (point-y to) (point-y from))) steps)))
          "left" 1))
      (mouse! p "mouseReleased" to "left" 0)
      (void))))
