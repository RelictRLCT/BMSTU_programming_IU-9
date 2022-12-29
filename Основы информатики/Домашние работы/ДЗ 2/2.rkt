(define (list->set xs)
  (if (> (length xs) 0)
      (if (not (member (car xs) (cdr xs)))
          (cons (car xs) (list->set (cdr xs)))
          (list->set (cdr xs)))
      '()))

(define (set? xs)
  (define (set xs)
    (if (> (length xs) 0)
        (cons (if (member (car xs) (cdr xs))
                  #f
                  #t) (set (cdr xs)))
        '()))
  (if (and (member #f (set xs)))
      #f
      #t))

(define (union xs ys)
  (list->set (append xs ys)))

(define (intersection xs ys)
  (if (> (length xs) 0)
      (if (member (car xs) ys)
          (cons (car xs) (intersection (cdr xs) ys))
          (intersection (cdr xs) ys))
      '()))

(define (difference xs ys)
  (if (> (length xs) 0)
      (if (not (member (car xs) ys))
          (cons (car xs) (difference (cdr xs) ys))
          (difference (cdr xs) ys))
      '()))

(define (symmetric-difference xs ys)
  (union (difference xs ys) (difference ys xs)))

(define (set-eq? xs ys)
  (define (set xs ys)
    (if (> (length xs) 0)
        (cons (if (member (car xs) ys)
                  #t
                  #f)
              (set (cdr xs) ys))
        '()))
  (if (= (length xs) (length ys))
      (if (not (member #f (set xs ys)))
          #t
          #f)
      #f))
