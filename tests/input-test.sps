(import (chezscheme) (cdp) (cdp page) (test) (support))

(define (keys-since p n)
  (let ([keys (vector->list (evaluate p "keys"))])
    (list-tail keys n)))

(define (key-count p) (evaluate p "keys.length"))

(define (field-value p) (evaluate p "document.getElementById('field').value"))

(with-browser (b chromium (browser-options [timeout (make-time 'time-duration 0 5)]))
  (define p (open-page b (page-url "input.html")))

  (test "page-type! types text into the focused field, inserting what no key types"
    (element-click! (car (page-query p "#field")))
    (page-type! p "Hi, wörld!")
    (check-equal? "Hi, wörld!" (field-value p))
    (page-press! p "Backspace")
    (check-equal? "Hi, wörld" (field-value p)))

  (test "a key with modifiers sends each modifier as its own key, and no text"
    (let ([n (key-count p)])
      (page-press! p "x" (modifiers alt))
      (check-equal? '(#("Alt" "AltLeft" #t #f #f #f) #("x" "KeyX" #t #f #f #f)) (keys-since p n))
      (check-equal? "Hi, wörld" (field-value p)))
    (let ([n (key-count p)])
      (page-press! p "g" (modifiers control meta))
      (check-equal?
        '(#("Control" "ControlLeft" #f #t #f #f) #("Meta" "MetaLeft" #f #t #t #f)
          #("g" "KeyG" #f #t #t #f))
        (keys-since p n))))

  (test "a shifted character holds Shift, and Shift shifts a character"
    (let ([n (key-count p)])
      (page-press! p "A")
      (page-press! p "a" (modifiers shift))
      (page-press! p "!")
      (check-equal?
        '(#("Shift" "ShiftLeft" #f #f #f #t) #("A" "KeyA" #f #f #f #t)
          #("Shift" "ShiftLeft" #f #f #f #t) #("A" "KeyA" #f #f #f #t)
          #("Shift" "ShiftLeft" #f #f #f #t) #("!" "Digit1" #f #f #f #t))
        (keys-since p n))
      (check-equal? "Hi, wörldAA!" (field-value p))))

  (test "named keys"
    (let ([n (key-count p)])
      (page-press! p "Enter")
      (page-press! p "ArrowLeft")
      (page-press! p "Escape")
      (check-equal? '("Enter" "ArrowLeft" "Escape")
        (map (lambda (k) (vector-ref k 0)) (keys-since p n))))
    (check-raises assertion-violation? (page-press! p "NoSuchKey"))
    (check-raises assertion-violation? (page-press! p "x" '(alt))))

  (test "element-click! clicks an element once nothing covers it"
    (element-click! (car (page-query p "#button")))
    (check-equal? 1 (evaluate p "clicks"))
    (evaluate p "setTimeout(() => document.getElementById('cover').remove(), 300)")
    (element-click! (car (page-query p "#covered")))
    (check-equal? 1 (evaluate p "coveredClicks")))

  (test "element-click! gives up when something keeps covering the element"
    (evaluate p "document.body.insertAdjacentHTML('beforeend', '<div id=\"cover\"></div>')")
    (guard (e [(cdp-timeout? e) (check-equal? "another element covers it" (cdp-timeout-last e))])
      (element-click! (car (page-query p "#covered")))
      (check #f))
    (check-equal? 1 (evaluate p "coveredClicks")))

  (test "element-hover! moves the pointer onto the element"
    (let ([hovered (car (page-query p "#hovered"))])
      (check-equal? "rgb(0, 0, 0)" (element-style hovered "color"))
      (element-hover! hovered)
      (check-equal? "rgb(255, 0, 0)" (element-style hovered "color"))))

  (test "page-drag! presses, moves in steps with the button held, and releases"
    (page-drag! p (make-point 10 210) (make-point 110 260))
    (let ([drag (member '#("mousedown" 10 210 1) (vector->list (evaluate p "drag")))])
      (check drag)
      (check-equal? '#("mouseup" 110 260 0) (list-ref drag (- (length drag) 1)))
      (check (<= 10 (length (filter (lambda (e) (and (equal? (vector-ref e 0) "mousemove")
                                                     (= (vector-ref e 3) 1)))
                              drag))))))

  (test "page-wheel! scrolls what is under the point"
    (page-wheel! p (make-point 450 50) 0 300)
    (check (until (lambda () (= 300 (evaluate p "document.getElementById('scroller').scrollTop"))))))

  (page-close! p))
