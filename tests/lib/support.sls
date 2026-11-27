(library (support)
  (export chromium page-url evaluate group-running? until)
  (import (chezscheme) (process) (cdp) (cdp page))

  (define chromium (or (getenv "CHROMIUM") "chromium"))

  (define (page-url name)
    (string-append "file://" (current-directory) "/tests/pages/" name))

  (define (evaluate p expression)
    (let ([result (cdp-call (page-session p) "Runtime.evaluate"
                    `(("expression" . ,expression) ("returnByValue" . #t) ("awaitPromise" . #t)))])
      (when (assoc "exceptionDetails" result)
        (error 'evaluate "the page raised" expression (cdr (assoc "exceptionDetails" result))))
      (let ([value (assoc "value" (cdr (assoc "result" result)))])
        (and value (cdr value)))))

  (define (group-running? group)
    (exit-status-success? (process-result-status (run-process "pgrep" (list "-g" (number->string group))))))

  ;; The page applies some input on its compositor, after the call returns.
  (define (until thunk)
    (let loop ([tries 0])
      (or (thunk)
          (and (< tries 100)
               (begin (sleep (make-time 'time-duration 20000000 0)) (loop (+ tries 1))))))))
