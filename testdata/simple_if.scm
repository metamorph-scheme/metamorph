(define my-cond #t)
(define out (current-output-port))

(let ((other-cond #t))
  (if (and my-cond other-cond (and (or #t #f #f #f #f other-cond) #t #t (if #t my-cond 8) #t)) 
    (write-string out "YES")
    (write-string out "NO")))