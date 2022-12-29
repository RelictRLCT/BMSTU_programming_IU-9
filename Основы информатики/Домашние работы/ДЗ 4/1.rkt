(define (memoized-factorial n)
  (let ((spis '()))
    (let* ((args (list n))
           (res (assoc args spis)))
      (if res
          (cadr res)
          (if (= n 0)
              1
              (let ((res (* n (memoized-factorial (- n 1)))))
                (set! spis (cons (list args res) spis))
                res))))))
