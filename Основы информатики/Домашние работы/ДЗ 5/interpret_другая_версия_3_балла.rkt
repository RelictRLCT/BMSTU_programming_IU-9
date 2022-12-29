;Соавтор Нащекин Никита
;Соавтор Никитин Александр
(define feature-if-else #t)
(define feature-while-loop #t)
(define feature-break-continue #t)
(define (статья-есть-а-если-найду? словарь слово)
  (if (not (null? словарь))
      (if (equal? слово (caar словарь))
          (cadar словарь) ;индекс статьи
          (статья-есть-а-если-найду? (cdr словарь) слово))
      -1))

(define (wend_find program index)
  (let loop ((index index))
    (if (equal? (vector-ref program index) 'wend)
        (+ index 1)
        (loop (+ index 1)))))

(define (interpret program stack)
  (let interpretator ((index 0)
                      (stack stack)
                      (slovar '())
                      (возвраты '()))
    (cond
      ((>= index (length (vector->list program))) stack)
      ((and (not (null? slovar)) (not (= -1 (статья-есть-а-если-найду? slovar (vector-ref program index)))))
       (begin (set! возвраты (cons (list (+ 1 index)) возвраты))
              (interpretator (статья-есть-а-если-найду? slovar (vector-ref program index)) stack slovar возвраты)))
      ((number? (vector-ref program index)) (interpretator (+ index 1) (cons (vector-ref program index) stack) slovar возвраты))
    
      ((equal? (vector-ref program index) '*) (begin (let ((a (* (car stack) (cadr stack))))
                                                       (set! stack (cdr stack))
                                                       (set-car! stack a))
                                                     (interpretator (+ index 1) stack slovar возвраты)))
    
      ((equal? (vector-ref program index) '+) (begin (let ((a (+ (car stack) (cadr stack))))
                                                       (set! stack (cdr stack))
                                                       (set-car! stack a))
                                                     (interpretator (+ index 1) stack slovar возвраты)))

      ((equal? (vector-ref program index) '-) (begin (let ((a (- (cadr stack) (car stack))))
                                                       (set! stack (cdr stack))
                                                       (set-car! stack a))
                                                     (interpretator (+ index 1) stack slovar возвраты)))

      ((equal? (vector-ref program index) '/) (begin (let ((a (quotient (cadr stack) (car stack))))
                                                       (set! stack (cdr stack))
                                                       (set-car! stack a))
                                                     (interpretator (+ index 1) stack slovar возвраты)))

      ((equal? (vector-ref program index) 'mod) (begin (let ((a (remainder (cadr stack) (car stack))))
                                                         (set! stack (cdr stack))
                                                         (set-car! stack a))
                                                       (interpretator (+ index 1) stack slovar возвраты)))

      ((equal? (vector-ref program index) 'neg) (begin (let ((a (- (car stack))))
                                                         (set-car! stack a))
                                                       (interpretator (+ index 1) stack slovar возвраты)))

      ((equal? (vector-ref program index) '=) (begin (if (= (car stack) (cadr stack))
                                                         (begin (set! stack (cdr stack))                                                         
                                                                (set-car! stack -1))
                                                         (begin (set! stack (cdr stack))                                                         
                                                                (set-car! stack 0)))
                                                     (interpretator (+ index 1) stack slovar возвраты)))

      ((equal? (vector-ref program index) '>) (begin (if (> (cadr stack) (car stack))
                                                         (begin (set! stack (cdr stack))                                                         
                                                                (set-car! stack -1))
                                                         (begin (set! stack (cdr stack))                                                         
                                                                (set-car! stack 0)))
                                                     (interpretator (+ index 1) stack slovar возвраты)))

      ((equal? (vector-ref program index) '<) (begin (if (< (cadr stack) (car stack))
                                                         (begin (set! stack (cdr stack))                                                         
                                                                (set-car! stack -1))
                                                         (begin (set! stack (cdr stack))                                                         
                                                                (set-car! stack 0)))
                                                     (interpretator (+ index 1) stack slovar возвраты)))

      ((equal? (vector-ref program index) 'not) (begin (if (not (equal? (car stack) 0))                                                         
                                                           (set-car! stack 0)                                                         
                                                           (set-car! stack -1))
                                                       (interpretator (+ index 1) stack slovar возвраты)))

      ((equal? (vector-ref program index) 'and) (begin (if (and (not (equal? (car stack) 0)) (not (equal? (cadr stack) 0)))                                                         
                                                           (begin (set! stack (cdr stack))
                                                                  (set-car! stack -1))
                                                           (begin (set! stack (cdr stack))
                                                                  (set-car! stack 0)))
                                                       (interpretator (+ index 1) stack slovar возвраты)))

      ((equal? (vector-ref program index) 'or) (begin (if (or (not (equal? (car stack) 0)) (not (equal? (cadr stack) 0)))                                                         
                                                          (begin (set! stack (cdr stack))
                                                                 (set-car! stack -1))
                                                          (begin (set! stack (cdr stack))
                                                                 (set-car! stack 0)))
                                                      (interpretator (+ index 1) stack slovar возвраты)))

      ((equal? (vector-ref program index) 'drop) (begin (set! stack (cdr stack))
                                                        (interpretator (+ index 1) stack slovar возвраты)))

      ((equal? (vector-ref program index) 'swap) (begin (let ((a (car stack))
                                                              (b (cadr stack)))
                                                          (set! stack (cdr stack))
                                                          (set-car! stack a)
                                                          (set! stack (cons b stack))) 
                                                        (interpretator (+ index 1) stack slovar возвраты)))

      ((equal? (vector-ref program index) 'dup) (begin (set! stack (cons (car stack) stack))
                                                       (interpretator (+ index 1) stack slovar возвраты)))

      ((equal? (vector-ref program index) 'over) (begin (set! stack (cons (cadr stack) stack))
                                                        (interpretator (+ index 1) stack slovar возвраты)))

      ((equal? (vector-ref program index) 'depth) (begin (set! stack (cons (length stack) stack))
                                                         (interpretator (+ index 1) stack slovar возвраты)))

      ((equal? (vector-ref program index) 'rot) (begin (let ((a (car stack))
                                                             (b (cadr stack))
                                                             (c (caddr stack)))
                                                         (set! stack (cons c (cons b (cons a (cdddr stack))))))
                                                       (interpretator (+ index 1) stack slovar возвраты)))

      ((equal? (vector-ref program index) 'define)
       (interpretator (let loop ((index index)
                                 (stack stack))
                        (if (equal? (vector-ref program index) 'end)
                            (+ index 1)
                            (loop (+ index 1) stack)))
                      stack (cons (list (vector-ref program (+ 1 index)) (+ 2 index)) slovar) возвраты))
                                                                  

      ((and (or (equal? (vector-ref program index) 'exit) (equal? (vector-ref program index) 'end)) (null? возвраты)) stack)

      ((and (or (equal? (vector-ref program index) 'exit) (equal? (vector-ref program index) 'end)) (not (null? возвраты)))
       (interpretator (caar возвраты) stack slovar (cdr возвраты)))

      ((equal? (vector-ref program index) 'endif) (interpretator (+ 1 index) stack slovar возвраты))

      ((equal? (vector-ref program index) 'if) (if (= (car stack) 0)
                                                   (interpretator (let loop ((index index))
                                                                    (if (or (equal? (vector-ref program index) 'else)
                                                                            (equal? (vector-ref program index) 'endif))
                                                                        (+ 1 index)
                                                                        (loop (+ 1 index))))
                                                                  (cdr stack) slovar возвраты)
                                                   (interpretator (+ 1 index) (cdr stack) slovar возвраты)))

      ((equal? (vector-ref program index) 'else) (interpretator (let loop ((index index))
                                                                  (if (equal? (vector-ref program index) 'endif)
                                                                      (+ 1 index)
                                                                      (loop (+ 1 index))))
                                                                stack slovar возвраты))

      ((equal? (vector-ref program index) 'while)
       (if (= (car stack) 0)
           (interpretator (wend_find program index) (cdr stack) slovar возвраты)
           (interpretator (+ 1 index) (cdr stack) slovar (cons index возвраты))))

      ((equal? (vector-ref program index) 'wend)
       (interpretator (car возвраты) stack slovar (cdr возвраты)))

      ((equal? (vector-ref program index) 'break) (interpretator (+ (wend_find program index) 1)
                                                                 (cdr stack) slovar (cdr возвраты)))
      
      ((equal? (vector-ref program index) 'continue) (interpretator (car возвраты) stack slovar возвраты)))))
