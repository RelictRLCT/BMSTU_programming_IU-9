(define (member! xs)
  (define (mem xs count len)
    (if (null? xs)
        (= count len)
        (and (or (equal? (car xs) #\space)
                 (equal? (car xs) #\newline)
                 (equal? (car xs) #\tab)
                 (equal? (car xs) #\return))
             (mem (cdr xs) (+ count 1) len))))
  (mem xs 0 (length xs)))
        

(define (list-trim-right xs)
  (define (trim xs done)
    (if (member! xs)
        done
        (trim (cdr xs) (append done (list (car xs))))))
  (trim xs '()))
