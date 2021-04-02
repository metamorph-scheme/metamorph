(define continuation)
(define counter 0)

(define (n-times times func x)
    (set! counter (+ counter 1))
    (if (> times 0)
        (n-times (- times 1) func (func x))
        x))

(define (mul2-store8 x)
    (let ((maybe-num
            (call/cc (lambda (c) (if (= x 8) (set! continuation c))))))
        (* 2 (if (number? maybe-num) maybe-num x))))

(write-string (number->string (n-times 4 mul2-store8 1)))
(newline)
(if (= counter 5) (write-string (number->string (continuation 5))))