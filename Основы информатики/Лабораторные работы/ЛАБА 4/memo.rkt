(define (tri n)
  (cond ((<= n 1) 0)
        ((= n 2) 1)
        (else (+ (tri (- n 1))
                 (tri (- n 2))
                 (tri (- n 3))))))

(define (trib n)
  (let ((known-results (make-vector (+ n 1))))
    (let loop ((n n))
      (cond
        ((<= n 1) 0)
        ((= n 2) 1)
        (else (if (= (vector-ref known-results n) 0)
                  (vector-set! known-results n
                               (+ (loop (- n 1))
                                  (loop (- n 2))
                                  (loop (- n 3)))))
              (vector-ref known-results n))))))
