(define outer #t)
(let-syntax
  ((hansi (syntax-rules ()
      ((_ x y z) (and outer (let-syntax ((z (syntax-rules () ((_ a) (odd? a))))) (y (z x)))))))) (hansi 4 not outer))