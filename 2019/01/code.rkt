#lang racket

(require threading
         rackunit)

(define (calc-fuel m)
  (~> m (/ 3) (floor) (- 2)))

(module+ test
  (check-equal? (calc-fuel 12) 2)
  (check-equal? (calc-fuel 14) 2)
  (check-equal? (calc-fuel 1969) 654)
  (check-equal? (calc-fuel 100756) 33583))

(define (calc-module-fuel m)
  (let ([f (calc-fuel m)])
    (cond
      [(negative? f) 0]
      [else (+ f (calc-module-fuel f))])))

(module+ test
  (check-equal? (calc-module-fuel 1969) 966))

(define inputs
  (~>> "./input.txt"
       (file->lines)
       (map string->number)))

(define part1
  (~>> inputs
       (map calc-fuel)
       (apply +)))

(define part2
  (~>> inputs
       (map calc-module-fuel)
       (apply +)))
