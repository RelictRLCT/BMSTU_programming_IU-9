(define-syntax trace-ex
  (syntax-rules ()
    ((trace-ex proc)
     
     (begin (write 'proc)
            (display "=>")
            (let ((a proc))
              (write a)
              (newline)
              a)))))

(display "Модуль трассировки подключён")
(newline)
