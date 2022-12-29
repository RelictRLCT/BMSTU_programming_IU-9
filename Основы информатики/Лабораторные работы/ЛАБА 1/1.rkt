(define (my-abs x)
  (if (< x 0)
      (- x)
      x))

(define (my-odd? x)
  (= (remainder x 2) 1))

(define (my-even? x)
  (= (remainder x 2) 0))

(define (power b e)
  (if (= e 0)
      1
      (* b (power b (- e 1)))))
