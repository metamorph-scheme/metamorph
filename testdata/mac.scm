(let-syntax ((hansi (syntax-rules () ((_ x y) (y x))))) 
 (hansi 4 even?))