(import (chezscheme) (process) (cdp))

(let ([b (launch-browser (getenv "CHROMIUM")
           (browser-options [profile (car (command-line-arguments))]))])
  (printf "~a ~a\n" (process-id (browser-process b)) (process-group-id (browser-process b)))
  (flush-output-port (current-output-port))
  (get-line (current-input-port)))
