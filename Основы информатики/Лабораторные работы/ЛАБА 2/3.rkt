(define (iterate f x n)
  (if (< n 1)
      '()
      (cons x (iterate f (f x) (- n 1)))))
