(load "unit-test2.rkt")

(define (signum x)
  (cond
    ((< x 0) -1)
    ((= x 0)  1) ; Ошибка здесь!
    (else     1)))

(define the-test
  (test (signum 2) 1))

(define the-tests
  (list (test (signum -2) -1)
        (test (signum  0)  0)
        (test (signum  2)  1)))


(define counter 0)

(define (next)
  (set! counter
        (+ counter 1))
  counter)

(set! counter 5)

(define next-test
  (test (next) 6))

(define next-test1
  (test (next) 7))

(define next-tests
  (list (test (next) 6)
        (test (next) 10)
        (test (next) 8)))

(define tsa
  (test (string-append "штука" "турка") "штукатурка"))

(define divizion
  (test (/ 2 0) 0))
