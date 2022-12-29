(define backtrack #t)

(define (error) (backtrack #f))

(define (last-body program)
  (if (and (not (member 'define program))
           (not (member 'end program)))
      program
      (if (not (null? (car program)))
          (last-body (cdr program))
          (error))))

(define (search-endif program)
  (let loop ((program program)
             (count 0))
    (and (not (null? program))
         (cond
           ((equal? (car program) 'if)
            (loop (cdr program) (+ count 1)))
           ((and (equal? (car program) 'endif) (= count 1))
            (cdr program))
           ((equal? (car program) 'endif)
            (loop (cdr program) (- count 1)))
           (else (loop (cdr program) count))))))
            
(define (body program)
  (let loop ((program program)
             (list-done '()))
    (if (not (null? program))
        (cond
          ((member (car program) '(define end)) (error))
          ((equal? (car program) 'if)
           (if (not (equal? #f (search-endif program)))
               (begin
                 (set! list-done
                       (append list-done
                               (list (list 'if
                                           (loop (cdr program) '())))))
                 (loop (search-endif program) list-done))
               (error)))
          ((equal? (car program) 'endif)
           list-done)
          (else
           (begin
             (set! list-done (append list-done (list (car program))))
             (loop (cdr program) list-done))))
        list-done)))

(define (деф-энд program)
  (let loop ((program program)
             (def 0)
             (end 0))
    (if (null? program)
        (= def end)
        (if (equal? (car program) 'define)
            (loop (cdr program) (+ def 1) end)
            (if (equal? (car program) 'end)
                (loop (cdr program) def (+ 1 end))
                (loop (cdr program) def end))))))

(define (defines program)
  (if (or (not (member 'end program)) (equal? (деф-энд program) #f))
      (error)
      (let loop ((program program))
        (if (not (null? program))
            (if (member 'end program)
                (if (equal? (car program) 'define)
                    (if (member (cadr program) '(if endif))
                        (error)
                        (loop (cdr program)))
                    (if (not (equal? (car program) 'end))
                        (cons
                         (cons
                          (car program)
                          (list
                           (body
                            (let loop2 ((programmm (cdr program)))
                              (if (equal? (car programmm) 'end)
                                  '()
                                  (cons (car programmm)
                                        (loop2 (cdr programmm))))))))
                         (loop (let loop3 ((programm program))
                                 (if (equal? (car programm) 'end)
                                     (cdr programm)
                                     (loop3 (cdr programm))))))
                        (error)))
                '())
            program))))

(define (parse program)
  (call-with-current-continuation
   (lambda (cc)
     (set! backtrack cc)
     (let ((prog (vector->list program)))
       (if (and (not (null? prog))
                (equal? (car prog) 'define))
           (cons (defines prog)
                 (list (body (last-body prog))))
           (cons '() (list (body prog))))))))
