#lang racket

(require threading
         rackunit)

(define inputs
  (~>> "./input.txt"
       (file->lines)
       (map string->number)))

(define (calc-fuel mass)
  (~> mass (/ 3) (floor) (- 2)))

(module+ test
  (check-equal? (calc-fuel 12) 2)
  (check-equal? (calc-fuel 14) 2)
  (check-equal? (calc-fuel 1969) 654)
  (check-equal? (calc-fuel 100756) 33583))

; Calculate fuel recursively
; Part 1 didn't account for mass of fuel so we add
; fuel until we get to 0
(define (calc-fuel* mass)
  (let ([fuel (calc-fuel mass)])
    (cond
      [(negative? fuel) 0]
      [else (+ fuel (calc-fuel* fuel))])))

(module+ test
  (check-equal? (calc-fuel* 1969) 966))

; Answers

(define part1
  (~>> inputs
       (map calc-fuel)
       (apply +)))

(define part2
  (~>> inputs
       (map calc-fuel*)
       (apply +)))
