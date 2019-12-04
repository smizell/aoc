#lang racket

(require threading)

(module+ test (require rackunit))

(define (repeating-regex s)
  (regexp-match* #px"(\\d+)\\1+" s))

(define (has-repeating? n)
  (let* ([s (number->string n)]
         [m (repeating-regex s)])
    (not (empty? m))))

(module+ test
  (check-equal? (has-repeating? 111111) #t)
  (check-equal? (has-repeating? 122345) #t)
  (check-equal? (has-repeating? 111123) #t)
  (check-equal? (has-repeating? 123789) #f))

; This is ugly :)
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

(define (any? fn l)
  (cond
    [(empty? l) #f]
    [else (or (fn (first l)) (any? fn (rest l)))]))

(module+ test
  (check-equal? (any? (lambda (v) (eq? v #t)) '(#t #f)) #t)
  (check-equal? (any? (lambda (v) (eq? v #t)) '(#f #f)) #f))

(define (only-two? s)
  (eq? (string-length s) 2))

(module+ test
  (check-equal? (only-two? "aa") #t))

(define (any-only-two? l)
  (any? only-two? l))

(module+ test
  (check-equal? (any-only-two? '("rr" "ee")) #t)
  (check-equal? (any-only-two? '("rre" "ee")) #t)
  (check-equal? (any-only-two? '("rre" "eee")) #f))

(define (part2)
  (~>> (find-matches)
       (map number->string)
       (map repeating-regex)
       (filter any-only-two?)
       (length)))

