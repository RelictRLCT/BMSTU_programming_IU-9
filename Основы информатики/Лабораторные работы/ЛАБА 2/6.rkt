(define (f x) (+ x 2))
(define (g x) (* x 3))
(define (h x) (- x))

(define (o . xs)
  (define (compos-x x)
    (define (compos x xs)
      (if (null? xs)
          x
          (compos ((car xs) x) (cdr xs))))
     (compos x (reverse xs)))
  compos-x)
