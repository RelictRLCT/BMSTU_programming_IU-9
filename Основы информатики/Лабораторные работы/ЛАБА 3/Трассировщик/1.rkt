(load "trace.rkt")

(define counter 0)
(define (next)
  (set! counter
        (+ counter 1))
  counter)

(define (zip . xss)
  (if (or (null? xss)
          (null? (trace-ex (car xss)))) ; Здесь...
      '()
      (cons (map car xss)
            (apply zip (map cdr (trace-ex xss)))))) ; ... и здесь
