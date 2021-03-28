(define out (current-output-port))
(define (bool->string b) (if b "TRUE" "FALSE"))
(define (compose a b) (lambda (x) (a (b x))))
(define (my-show str) (write-string str out))
(define show-bool (compose my-show bool->string))
(define my-newline (lambda () (newline out)))
(define (number-to-string n) (number->string n))
(define show-number (compose my-show number-to-string))

(define a '((1 4) 2 3))
(set-car! (car a) 10)

(show-bool (= (car (car a)) 10))