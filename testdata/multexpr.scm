(define (fac n) 
    (if (> n 0) 
        (* n (fac (- n 1)))
        1))

(define (lookup key-1 key-2 table)
  (let ((subtable (assoc key-1 (cdr table))))
    (if subtable
      (let ((record (assoc key-2 (cdr subtable))))
        (if record
          (cdr record)
          false))
    false)))

(fac 4)

