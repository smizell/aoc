#lang racket

(require threading
         rackunit)

(define input
  (~> "./input.txt"
       (file->string)
       (string-trim)
       (string-split ",")
       (map string->number _)))

(define (run-program v)
  (run-step v 0))

(define (run-step result curr-pos)
  (define values (list-tail result curr-pos))
  (match values
    [(list 1 a b pos rest ...)
     (let* ([value (+ (list-ref result a) (list-ref result b))]
            [new-result (list-set result pos value)])
       (run-step new-result (+ curr-pos 4)))]
    [(list 2 a b pos rest ...)
     (let* ([value (* (list-ref result a) (list-ref result b))]
            [new-result (list-set result pos value)])
       (run-step new-result (+ curr-pos 4)))]
    [(list 99 rest ...) result]))

(module+ test
  (check-equal? (run-program '(1 0 0 0 99)) '(2 0 0 0 99))
  (check-equal? (run-program '(2 3 0 3 99)) '(2 3 0 6 99))
  (check-equal? (run-program '(2 4 4 5 99 0)) '(2 4 4 5 99 9801))
  (check-equal? (run-program '(1 1 1 4 99 5 6 0 99)) '(30 1 1 4 2 5 6 0 99)))

(define (gravity-assist initial noun verb)
  (~> initial
      (list-set 1 noun)
      (list-set 2 verb)
      (run-program)
      (list-ref 0)))

(define (find-noun-verb value)
  (for*/fold ([n null]
              [v null])
             ([i (in-range 100)]
              [j (in-range 100)])
    (let ([r (gravity-assist input i j)])
      (cond
        [(eq? r value) (values i j)]
        [else (values n v)]))))

(define (final-calc n v)
  (+ (* n 100) v))

(module+ test
  (check-equal? (final-calc 12 2) 1202))

(define part1
  (gravity-assist input 12 2))

(define part2
  (let-values ([(n v) (find-noun-verb 19690720)])
    (final-calc n v)))