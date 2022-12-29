(define-syntax my-if
  (syntax-rules ()
    ((_ usl true false)
     (force (or (and usl (delay true))
                (delay false))))))
