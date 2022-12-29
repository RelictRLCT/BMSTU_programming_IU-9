(define-syntax when                                 
  (syntax-rules ()
    ((_ cond exprs ...)
     (if cond
         (begin exprs ...)))))

(define-syntax unless                                
  (syntax-rules ()
    ((_ cond exprs ...)
     (if (not cond)
         (begin exprs ...)))))

(define-syntax for                                  
  (syntax-rules (in as)
    ((for perem in listt exprs ...)
     (LET loop ((listtt listt))
          (if (not (null? listtt))
              (let ((perem (car listtt)))
                exprs ...
                (loop (cdr listtt))))))
    ((for listt as perem exprs ...)
     (LET loop ((listtt listt))
          (if (not (null? listtt))
              (let ((perem (car listtt)))
                exprs ...
                (loop (cdr listtt))))))))


(define-syntax while                                 
  (syntax-rules ()
    ((_ cond exprs ...)
     (let loop ()
       (if cond
           (begin exprs ... (loop)))))))

(define-syntax repeat                        
  (syntax-rules (until)
    ((_ (exprs ...) until cond)
     (let loop ()
       (begin exprs ...)
       (if (not cond) (loop))))))


(define-syntax cout                                   
  (syntax-rules (<< endl)
    ((cout . exprs)
     (let loop ((expr 'exprs))
       (if (not (null? expr))
           (if (equal? (car expr) '<<)
               (loop (cdr expr))
               (if (equal? (car expr) 'endl)
                   (begin (newline)
                          (loop (cdr expr)))
                   (begin
                     (if (or (string? (car expr))
                             (number? (car expr)))
                         (display (car expr))
                         (if (list? (car expr))
                             (display (eval
                                       (car expr)
                                       (interaction-environment)))))
                     (loop (cdr expr))))))))))
