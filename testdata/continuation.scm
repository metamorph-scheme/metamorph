(define cont)
(define x 0)
(define (func) (call/cc (lambda (k) (set! cont  k))) (if (= x 100)  ))