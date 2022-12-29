(define (count x xs)
  (if (< (length xs) 1)
      0
      (if (EQUAL? (car xs) x)
          (+ 1 (count x (cdr xs)))
          (count x (cdr xs)))))
