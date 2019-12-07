#lang racket

(require threading)

(module+ test (require rackunit))

; Used a hash so I didn't have to add the same numbers over and over
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

(define (part1)
  (total-orbits input))

(define (orbits os t [r '()])
  (match t
    ['COM r]
    [_ (let ([n (hash-ref os t)])
         (orbits os n (append r (list n))))]))

(module+ test
  (define o2 (hasheq
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
              'L 'K
              'YOU 'K
              'SAN 'I))
  (define you (orbits o2 'YOU))
  (define san (orbits o2 'SAN))
  (check-equal? san '(I D C B COM)))

(define (transfers os f t)
  (let* ([fos (orbits os f)]
         [tos (orbits os t)])
    (for/first ([o fos]
                #:when (index-of tos o))
      (+ (index-of fos o) (index-of tos o)))))

(module+ test
  (check-equal? (transfers o2 'YOU 'SAN) 4))

(define (part2)
  (transfers input 'YOU 'SAN))
