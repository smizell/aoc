#lang racket

(require threading)

(module+ test (require rackunit))

(define (count-orbits os t [r (make-hasheq)] [acc 0])
  (cond
    [(eq? t 'COM) acc]
    [else
     (unless (hash-has-key? r t)
       (hash-set! r t (count-orbits os (hash-ref os t) r (add1 acc))))
     (hash-ref r t)]))

(module+ test
  (define o1 (hasheq
              'B 'COM
              'C 'B
              'D 'C
              'E 'D
              'F 'E
              'G 'B
              'H 'G
              'I 'D
              'J 'E
              'K 'J
              'L 'K))
  (check-equal? (count-orbits o1 'D) 3)
  (check-equal? (count-orbits o1 'L) 7))

(define (total-orbits os)
  (let* ([ks (hash-keys os)]
         [t (map (lambda (k) (count-orbits os k)) ks)])
    (apply + t)))

(module+ test
  (check-equal? (total-orbits o1) 42))

(define (build-orbits ls)
  (let ([os (make-hasheq)])
    (for ([l ls])
      (hash-set! os (string->symbol (first l)) (string->symbol (second l))))
    os))

(define input
  (~> "./input.txt"
      (file->lines)
      (map (lambda (l) (reverse (string-split l ")"))) _)
      (build-orbits)))

