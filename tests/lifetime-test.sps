(import (chezscheme) (process) (cdp) (test) (support))

(define (gone-within? seconds group)
  (let ([deadline (add-duration (current-time 'time-utc) (make-time 'time-duration 0 seconds))])
    (let loop ()
      (cond
        [(not (group-running? group)) #t]
        [(time>=? (current-time 'time-utc) deadline) #f]
        [else (sleep (make-time 'time-duration 20000000 0)) (loop)]))))

(test "the browser ends when the process that launched it is killed"
  (let* ([profile (string-append (getenv "TMPDIR") "/held")]
         [driver (spawn-process (getenv "SCHEME")
                   (list "--libdirs" (getenv "LIBDIRS") "--script" "tests/fixtures/hold-browser.ss"
                         profile))]
         [line (get-line (transcoded-port (process-stdout driver) (native-transcoder)))]
         [group (let ([words (open-string-input-port line)]) (read words) (read words))])
    (check (number? group))
    (check (group-running? group))
    (process-kill! driver)
    (check-equal? 9 (exit-status-signal (process-wait driver)))
    (check (gone-within? 10 group))))

(test "browser-close! leaves none of the browser's processes"
  (let ([b (launch-browser chromium)])
    (let ([group (process-group-id (browser-process b))])
      (check (group-running? group))
      (browser-close! b)
      (check (not (group-running? group))))))
