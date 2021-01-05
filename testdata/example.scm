(define (fac n) 
    (if (> n 0) 
        (* n (fac (- n 1)))
        1))

(define (test x y . z) (cons (+ x y) z))
(test 1 4 3 5 1 "String")
(set! test fac)
(test 2)