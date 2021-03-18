(define out (current-output-port))

(define out (current-output-port))
(define (bool->string b) (if b "TRUE" "FALSE"))
(define (compose a b) (lambda (x) (a (b x))))
(define (my-show str) (write-string str out))
(define show-bool (compose my-show bool->string))
(define my-newline (lambda () (newline out)))

; Test to generate a number that is much larger than 32-bit C99 integers
(show-bool (boolean? 429496729643443434235234543543225437697238645723984562983578412349346571028347695813092387412937490165))
(my-newline)

(define elem-at (lambda (x xs)
  (if (= x 0) 
    (car xs)
    (elem-at 
      (- x 1) 
      (cdr xs)))))

(define my-list (cons 1 (cons 2 (cons 64345 ()))))

(show-bool (= 1 (elem-at 0 my-list)))
(my-newline)

(show-bool (= 2 (elem-at 1 my-list)))
(my-newline)

(show-bool (= 64345 (elem-at 2 my-list)))
(my-newline)

(show-bool (= 553 (elem-at 2 my-list)))
(my-newline)