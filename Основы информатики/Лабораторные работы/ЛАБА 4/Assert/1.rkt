(load "assert.rkt")
(use-assertations)

(define (1/x x)
  (assert (not (zero? x)))
  (/ 1 x))
