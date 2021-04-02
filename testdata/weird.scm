(define outer #t)
(define (od? n) #t)
(let-syntax
  ((hansi (syntax-rules ()
      ((_ x y z) (and outer (let-syntax ((z (syntax-rules () ((_ a) #t))) (y (z x)))))))) (hansi 4 not outer))
