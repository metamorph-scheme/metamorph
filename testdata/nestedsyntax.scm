(let-syntax
  ((hansi (syntax-rules ()
      ((_ x y) (let-syntax ((syn (syntax-rules () ((_ a) (odd? a))))) (y (syn x)) ))))) (hansi 4 not))