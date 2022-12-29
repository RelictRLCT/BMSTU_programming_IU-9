(define (intersperse e xs)
  (if (< (length xs) 2)
      xs
      (cons (car xs) (cons e (intersperse e (cdr xs))))))
