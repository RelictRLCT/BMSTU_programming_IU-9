(define (s->s s)
  (if (symbol? s)
      (symbol->string s)
      (string->symbol s)))

(define (my-eval exprs)
  (eval exprs (interaction-environment)))

(define-syntax define-data
  (syntax-rules ()
    ((define-data data-name ((name field1 ...) ...))
     (begin
       (my-eval (list 'define
                      'name
                      (lambda (field1 ...)
                        (list (list 'd-name 'data-name) (list 't-name 'name)
                              (list 'field1 field1) ...)))) ...
       (my-eval (list 'define
                      (s->s (string-append (s->s 'data-name) "?"))
                      (lambda (x)
                        (and (list? x) (>= (length x) 2)
                             (let ((d-nameres (assoc 'd-name x)))
                               (and d-nameres (equal? (cadr d-nameres) 'data-name)))))))))))

(define-syntax match
  (syntax-rules ()
    ((match x ((name field1 ...) expr) ...)
       (cond
         ((equal? (cadadr x) 'name)
           (let ((field1 (cadr (assoc 'field1 x))) ...)
             expr))
          ...
          (else x)))))

(define (s->s s)
  (if (symbol? s)
      (symbol->string s)
      (string->symbol s)))

(define (my-eval exprs)
  (eval exprs (interaction-environment)))


(define-syntax define-struct
  (syntax-rules ()
    ((define-struct name (field1 ...))
     (begin
       (my-eval (list 'define
                      (s->s (string-append "make-" (s->s 'name)))
                      (lambda (field1 ...)
                        (list (list 'type 'name) (list 'field1 field1) ...))))
       (my-eval (list 'define
                      (s->s (string-append (s->s 'name) "?"))
                      (lambda (x)
                        (and (list? x) (not (null? x))
                             (let ((ares (assoc 'type x)))
                               (and ares (equal? (cadr ares) 'name)))))))
       (my-eval (list 'define
                       (s->s (string-append (s->s 'name) "-" (s->s 'field1)))
                       (lambda (x)
                         (cadr (assoc 'field1 (cdr x)))))) ...
       (my-eval (list 'define
                       (s->s (string-append "set-" (s->s 'name) "-" (s->s 'field1) "!"))
                       (lambda (x val)
                         (set-car! (cdr (assoc 'field1 (cdr x))) val)))) ... ))))
