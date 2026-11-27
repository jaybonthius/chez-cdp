(import (chezscheme) (cdp) (cdp page) (test) (support))

(define (milliseconds n) (make-time 'time-duration (* n 1000000) 0))

(define (png-size png)
  (values (bytevector-u32-ref png 16 (endianness big)) (bytevector-u32-ref png 20 (endianness big))))

(define png-signature #vu8(137 80 78 71 13 10 26 10))

(define (bytevector-prefix bv n)
  (let ([out (make-bytevector n)])
    (bytevector-copy! bv 0 out 0 n)
    out))

(with-browser (b chromium)
  (define p (open-page b (page-url "reads.html") (page-options [width 400] [height 300])))

  (test "page-query finds every match in document order"
    (let ([items (page-query p ".item")])
      (check-equal? 2 (length items))
      (check-equal? "greeting" (element-attribute (car items) "id"))
      (check-equal? "second" (element-text (cadr items))))
    (check-equal? '() (page-query p ".nothing"))
    (check-raises cdp-error? (page-query p "[[")))

  (test "an element's text, attributes, style and box are read without JavaScript"
    (let ([greeting (car (page-query p "#greeting"))] [box (car (page-query p "#box"))])
      (check-equal? "Hello world" (element-text greeting))
      (check-equal? "hello" (element-attribute greeting "data-kind"))
      (check-equal? #f (element-attribute greeting "data-none"))
      (check-equal? "rgb(1, 2, 3)" (element-style greeting "color"))
      (check-equal? "700" (element-style (car (page-query p "#greeting b")) "font-weight"))
      (let ([r (element-box box)])
        (check-equal? '(20 30 100 50)
          (map (lambda (x) (exact (round x)))
            (list (rectangle-x r) (rectangle-y r) (rectangle-width r) (rectangle-height r)))))))

  (test "an element replaced by the page is found again by its selector"
    (let ([greeting (car (page-query p "#greeting"))])
      (evaluate p "document.getElementById('greeting').outerHTML = '<p id=\"greeting\" class=\"item\">Hello again</p>'")
      (check-equal? "Hello again" (element-text greeting))
      (evaluate p "document.getElementById('greeting').remove()")
      (check-raises element-not-found? (element-text greeting))
      (page-navigate! p (page-url "reads.html"))))

  (test "page-wait-for waits until an element is attached, visible or hidden"
    (evaluate p "setTimeout(() => document.getElementById('later').innerHTML = '<span id=\"new\">new</span>', 200)")
    (check-equal? "new" (element-text (page-wait-for p "#new" (element-state attached))))
    (evaluate p "setTimeout(() => document.getElementById('gone').style.display = 'block', 200)")
    (check-equal? "gone" (element-text (page-wait-for p "#gone" (element-state visible))))
    (page-wait-for p "#invisible" (element-state hidden))
    (evaluate p "setTimeout(() => document.getElementById('box').remove(), 200)")
    (page-wait-for p "#box" (element-state hidden))
    (guard (e [(cdp-timeout? e) (check-equal? "1 matching elements, none visible" (cdp-timeout-last e))])
      (page-wait-for p "#invisible" (element-state visible) (milliseconds 300))
      (check #f))
    (page-navigate! p (page-url "reads.html")))

  (test "a snapshot gives every node with its layout and the styles asked for"
    (let* ([nodes (page-snapshot p '("font-weight" "color"))]
           [world (find (lambda (n) (equal? (snapshot-node-value n) "world")) (vector->list nodes))]
           [parent (vector-ref nodes (snapshot-node-parent world))])
      (check-equal? "#text" (snapshot-node-name world))
      (check-equal? "world" (snapshot-node-text world))
      (check-equal? '(("font-weight" . "700") ("color" . "rgb(1, 2, 3)")) (snapshot-node-styles world))
      (check-equal? "B" (snapshot-node-name parent))
      (check (rectangle? (snapshot-node-bounds world)))
      (check-equal? (element-backend-node-id (car (page-query p "#greeting b")))
        (snapshot-node-backend-node-id parent))
      (let ([greeting (vector-ref nodes (snapshot-node-parent parent))])
        (check-equal? "hello" (cdr (assoc "data-kind" (snapshot-node-attributes greeting)))))
      (let ([gone (find (lambda (n) (equal? (assoc "id" (snapshot-node-attributes n)) '("id" . "gone")))
                    (vector->list nodes))])
        (check-equal? #f (snapshot-node-bounds gone))
        (check-equal? '() (snapshot-node-styles gone)))))

  (test "a screenshot is a PNG of the viewport, or of a clip"
    (let ([whole (page-screenshot p)] [clip (page-screenshot p (make-rectangle 20 30 100 50))])
      (check-equal? png-signature (bytevector-prefix whole 8))
      (check-equal? '(400 300) (call-with-values (lambda () (png-size whole)) list))
      (check-equal? '(100 50) (call-with-values (lambda () (png-size clip)) list))))

  (page-close! p))
