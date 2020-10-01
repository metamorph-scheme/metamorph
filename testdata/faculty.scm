(define (fac n) 
    (if (> n 0) 
        (* n (fac (- n 1)))
        1))