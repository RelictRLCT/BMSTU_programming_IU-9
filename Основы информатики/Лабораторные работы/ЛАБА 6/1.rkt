;<цифры>::=1|2|3|4|5|6|7|8|9|0
;<знаки>::=+|-
;<числитель>::=<цифры>|<числитель><цифры>|<знаки><числитель>
;<знаменатель>::=<цифры>|<знаменатель><цифры>
;<символ>::=/
;<дробь>::=<числитель><символ><знаменатель>|E
(define была-дробь? #f)

(define numbers '(#\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9 #\0))

(define znaki '(#\+ #\-))

(define backtrack #f)

(define числитель 0)

(define знаменатель 0)

(define (error) (backtrack #f))

(define (check-frac drob)
  (call-with-current-continuation
   (lambda (cc)
     (set! backtrack cc)
     (if (equal? drob "")
         (if (and (equal? была-дробь? #t) (> числитель 0) (> знаменатель 0))
             (begin (set! была-дробь? #f)
                    (set! знаменатель 0)
                    (set! числитель 0))
             (begin (set! была-дробь? #f)
                    (set! знаменатель 0)
                    (set! числитель 0)
                    (error))))
     (or (equal? drob "")
         (if (equal? была-дробь? #f)
             (cond
               ((equal? (car (string->list drob)) #\/)
                (begin (set! была-дробь? #t)
                       (check-frac (list->string (cdr (string->list drob))))))
               ((not (or (member (car (string->list drob)) numbers)
                         (member (car (string->list drob)) znaki)))
                (begin (set! была-дробь? #f) (error)))
               (else
                (begin (set! числитель (+ числитель 1))
                       (check-frac (list->string (cdr (string->list drob)))))))
             (cond
               ((not (member (car (string->list drob)) numbers))
                (begin (set! была-дробь? #f) (error)))
               (else
                (begin (set! знаменатель (+ знаменатель 1))
                       (check-frac (list->string
                                    (cdr (string->list drob))))))))))))

(define (scan-frac drob)
  (and (check-frac drob)
       (eval (string->number drob) (interaction-environment))))

(define (make-spisok str)
  (let loop ((str str)
             (listd '())
             (drob ""))
    (if (equal? str "")
        (append listd (list drob))
        (if (or (equal? (car (string->list str)) #\space)
                (equal? (car (string->list str)) #\tab)
                (equal? (car (string->list str)) #\newline))
            (if (not (equal? drob ""))
                (begin (if (null? listd)
                           (set! listd (list drob))
                           (set! listd (append listd (list drob))))
                       (loop (list->string
                              (cdr (string->list str))) listd ""))
                (loop (list->string (cdr (string->list str))) listd drob))
            (loop (list->string (cdr (string->list str)))
                  listd
                  (string-append drob
                                 (list->string
                                  (list (car (string->list str))))))))))

(define (scan-many-fracs str)
  (let loop ((drobi (make-spisok str))
             (evals '()))
    (if (null? drobi)
        evals
        (and (scan-frac (car drobi))
             (loop (cdr drobi)
                   (append evals
                           (list (scan-frac (car drobi)))))))))
        
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

;Если допустить вложенные статьи, то придется
;разрешить define...end внутри body.
