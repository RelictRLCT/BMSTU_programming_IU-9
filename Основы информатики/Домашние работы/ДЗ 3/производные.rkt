(define (list! expr)
  (if (not (list? expr))
      (list expr)
      expr))

(define (derivative expr)
  (if (not (list? expr))
      (set! expr (list expr)))
  
  (cond
    ((null? expr) '())
    ((equal? (car expr) 'x) 1)
    ((equal? (car expr) '-x) -1)
    ((number? (car expr)) 0)
    ((equal? (car expr) '+) (append '(+) (map derivative (cdr expr))));(list '+ (derivative (cadr expr)) (derivative (caddr expr))))
    ((equal? (car expr) '*)
     (cond
       ((> (length expr) 3)
        (list '+ (list '* (derivative (list! (cadr expr))) (append '(*) (cddr expr)))
              (list '* (derivative (append '(*) (list! (cddr expr)))) (cadr expr))))
       ((= (length expr) 3)
        (list '+ (list '* (derivative (list! (cadr expr))) (caddr expr))
              (list '* (derivative (list! (caddr expr))) (cadr expr))))))
     ((equal? (car expr) '-) (if (not (null? (cddr expr)))
                                 (list '- (derivative (cadr expr)) (derivative (caddr expr)))
                                 (list '- (derivative (cadr expr)))))
     ((equal? (car expr) '/)
      (list '/ (list '- (list '* (derivative (cadr expr)) (caddr expr)) (list '* (derivative (caddr expr)) (cadr expr)))
            (list 'expt (caddr expr) 2)))
     ((equal? (car expr) 'sin) (list '* (list 'cos (cadr expr)) (derivative (cadr expr))))
     ((equal? (car expr) 'cos) (list '* (list '- (list 'sin (cadr expr))) (derivative (cadr expr))))
     ((equal? (car expr) 'log) (list '* (list '/ 1 (cadr expr)) (derivative (cadr expr)))) 
     ((and (equal? (car expr) 'expt) (not (member 'x (list (caddr expr)))))
      (list '* (list '* (caddr expr) (list 'expt (cadr expr) (- (caddr expr) 1))) (derivative (cadr expr))))
     ((equal? (car expr) 'expt) (list '* (list '* (list 'expt (cadr expr) (caddr expr)) (list 'log (cadr expr))) (derivative (caddr expr))))
     ((equal? (car expr) 'exp) (list '* (list 'exp (cadr expr)) (derivative (cadr expr))))))
