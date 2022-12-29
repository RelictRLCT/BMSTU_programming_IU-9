(define-syntax use-assertations
  (syntax-rules ()
    ((use-assertations)
     (call-with-current-continuation
      (lambda (cc))))))

(define backtrack #f)

(define-syntax assert
  (syntax-rules ()
    ((assert expr)
     (if (not expr)
         (begin
           (display "Ошибка: ")
           (display 'expr)
           (backtrack))))))
