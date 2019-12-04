#lang racket

(require threading)

(module+ test (require rackunit))

(define (has-repeating? n)
  (let* ([s (number->string n)]
         [m (regexp-match #px"(\\d+)\\1" s)])
    (cond
      [(eq? m #f) #f]
      [(not (empty? m)) #t])))

(module+ test
  (check-equal? (has-repeating? 111111) #t)
  (check-equal? (has-repeating? 122345) #t)
  (check-equal? (has-repeating? 111123) #t)
  (check-equal? (has-repeating? 123789) #f))

(define (number->list n)
  (~>> n
       (number->string)
       (string->list)
       (map string)
       (map string->number)))

(define (increasing? n)
  (let* ([l (number->list n)]
         [s (sort l <)])
    (eq? l s)))

(module+ test
  (check-equal? (increasing? 123456) #t))

(define (find-matches)
  (for/fold ([acc '()])
            ([i (in-range 130254 678276)])
    (cond
      [(and (has-repeating? i) (increasing? i)) (cons i acc)]
      [else acc])))

(define (part1)
  (length (find-matches)))
