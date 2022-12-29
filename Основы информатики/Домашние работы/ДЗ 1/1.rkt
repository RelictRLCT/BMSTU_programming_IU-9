(define (day-of-week day month year)
  (if (> month 2)
      (remainder (+ day (quotient (- (* 13 (- month 2)) 1) 5) year (quotient year 4) (quotient year 400) (- (quotient year 100))) 7)
      (remainder (+ day (quotient (- (* 13 (+ month 10)) 1) 5) (- year 1) (quotient (- year 1) 4) (quotient (- year 1) 400) (- (quotient (- year 1) 100))) 7)))
      
