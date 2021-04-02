(define (plus x y) (+ x y))
(define (myMap f l1 l2) (if (or  (null? l1) (null? l2)) '() (cons (f (car l1) (car l2)) ( myMap f (cdr l1) (cdr l2)))))
(define g (myMap plus '(1 2 3 4) '(1 2 3 4)))
(write-string  (number->string (car g)))
(newline)

(define (print-numbers ls) (if (null? ls) #f ((lambda () (write-string (number->string (car ls)))  (print-numbers (cdr ls))))))

(print-numbers g)
(newline)