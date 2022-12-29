(define (save-data данные путь)
  (with-output-to-file путь
    (lambda ()
      (write данные))))

(define (load-data путь)
  (with-input-from-file путь read))

(define (count путь)
  (define (c путь перв-стр)
    (let ((port (open-input-file путь)))
      (let loop ((строки 0) (симв #f))
        (if (not (eof-object? (peek-char port)))
            (begin (set! симв (read-char port))
                   (if (not (and (equal? #\newline симв)
                                 (equal? #\return
                                         (peek-char port))))
                       (if (and (equal? симв #\newline)
                                (not (eof-object?
                                      (peek-char port))))
                           (loop (+ строки 1)
                                 (set! симв #f))
                           (loop строки (set! симв #f)))                     
                       (loop строки (set! симв #f))))
            (if (equal? перв-стр #t)
                строки
                (+ 1 строки))))))
  (if (equal? (with-input-from-file путь
                (lambda ()
                  (peek-char)))
              #\return)
      (c путь #t)
      (c путь #f)))
