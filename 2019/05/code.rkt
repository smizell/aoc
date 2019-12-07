#lang racket

(require threading)

(module+ test (require rackunit))

(define (number->list n)
  (~>> n
       (number->string)
       (string->list)
       (map string)
       (map string->number)))

(define (parse-first n)
  (let ([l (number->list n)])
    (match l
      [(list m2 m1 0 op) (list 0 m2 m1 op)]
      [(list m3 m2 m1 0 op) (list m3 m2 m1 op)])))

(module+ test
  (check-equal? (parse-first 1002) '(0 1 0 2))
  (check-equal? (parse-first 11002) '(1 1 0 2)))
