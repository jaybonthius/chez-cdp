(import (chezscheme) (cdp) (cdp page) (test) (support))

(define (field alist key) (cdr (assoc key alist)))

(define (target-ids b)
  (map (lambda (info) (field info "targetId"))
    (vector->list (field (cdp-call (browser-root-session b) "Target.getTargets") "targetInfos"))))

(with-browser (b chromium)
  (test "open-page loads the page before it returns"
    (let ([p (open-page b (page-url "storage.html"))])
      (check-equal? "storage" (evaluate p "document.title"))
      (check-equal? "complete" (evaluate p "document.readyState"))
      (page-close! p)))

  (test "pages have separate storage unless told to share it"
    (let ([first (open-page b (page-url "storage.html"))]
          [second (open-page b (page-url "storage.html"))]
          [shared (open-page b (page-url "storage.html") (page-options [shared-storage #t]))]
          [also-shared (open-page b (page-url "storage.html") (page-options [shared-storage #t]))])
      (evaluate first "localStorage.setItem('k', 'first')")
      (evaluate shared "localStorage.setItem('k', 'shared')")
      (check-equal? 'null (evaluate second "localStorage.getItem('k')"))
      (check-equal? "first" (evaluate first "localStorage.getItem('k')"))
      (check-equal? "shared" (evaluate also-shared "localStorage.getItem('k')"))
      (for-each page-close! (list first second shared also-shared))))

  (test "two threads drive two pages at once"
    (let* ([pages (list (open-page b (page-url "reads.html")) (open-page b (page-url "storage.html")))]
           [results (make-vector 2 #f)]
           [threads
            (map (lambda (p i)
                   (fork-thread
                     (lambda ()
                       (vector-set! results i
                         (guard (e [#t e])
                           (let loop ([n 0] [titles '()])
                             (if (= n 10)
                                 titles
                                 (begin
                                   (page-navigate! p (if (= i 0) (page-url "reads.html") (page-url "storage.html")))
                                   (loop (+ n 1)
                                     (cons (element-text (car (page-query p "title"))) titles))))))))))
                 pages '(0 1))])
      (for-each thread-join threads)
      (check-equal? (make-list 10 "reads") (vector-ref results 0))
      (check-equal? (make-list 10 "storage") (vector-ref results 1))
      (for-each page-close! pages)))

  (test "a page that cannot load raises a navigation error"
    (let ([p (open-page b (page-url "storage.html"))])
      (guard (e [(navigation-error? e)
                 (check-equal? (page-url "missing.html") (navigation-error-url e))])
        (page-navigate! p (page-url "missing.html"))
        (check #f))
      (page-close! p))
    (check-raises navigation-error? (open-page b (page-url "missing.html"))))

  (test "page-close! closes the page's target, once"
    (let* ([p (open-page b (page-url "storage.html"))] [before (length (target-ids b))])
      (page-close! p)
      (page-close! p)
      (check (until (lambda () (= (- before 1) (length (target-ids b))))))))

  (test "a page can ask for a light or dark color scheme"
    (let ([light (open-page b (page-url "scheme.html") (page-options [color-scheme (color-scheme light)]))]
          [dark (open-page b (page-url "scheme.html") (page-options [color-scheme (color-scheme dark)]))])
      (check-equal? "rgb(0, 0, 0)" (element-style (car (page-query light "#text")) "color"))
      (check-equal? "rgb(255, 255, 255)" (element-style (car (page-query dark "#text")) "color"))
      (page-close! light)
      (page-close! dark)))

  (test "page options are checked when they are made"
    (check-raises assertion-violation? (page-options [width 0]))
    (check-raises assertion-violation? (page-options [color-scheme 'dim]))
    (check-raises assertion-violation? (open-page b 'nowhere))))
