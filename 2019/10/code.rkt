#lang racket

(require threading)

(module+ test (require rackunit))

(define (load-file filename)
  (~>> filename
       (file->string)
       (string-split _ "\n")
       (map string->list)
       (map (lambda (l) (map string l)))))

(module+ test
  (define e1 (load-file "./example1.txt")))

; find asteroid coordinates

(define (asteroid-coordinates am)
  (for/fold ([acc '()])
            ([l (in-list am)]
             [i (range (length am))])
    (append acc (for/fold ([acc2 '()])
                          ([p (in-list l)]
                           [j (range (length l))])
                  (match p
                    ["#" (append acc2 (list (list j i)))]
                    ["." acc2])))))

(module+ test
  (define c1 '((1 0)
               (4 0)
               (0 2)
               (1 2)
               (2 2)
               (3 2)
               (4 2)
               (4 3)
               (3 4)
               (4 4)))
  (check-equal? (asteroid-coordinates e1) c1))

; map to number of visible coordinates

(define (coor-step a b)
  (define diff-a (- (first b) (first a)))
  (define diff-b (- (second b) (second a)))
  (define diff-gcd (gcd diff-a diff-b))
  (list (/ diff-a diff-gcd)
        (/ diff-b diff-gcd)))

(define (coor-sub a b)
  (list (- (first a) (first b))
        (- (second a) (second b))))

(define (next-step a b)
  (coor-sub b (coor-step a b)))

(define (all-steps o d)
  (define ns (next-step o d))
  (cond
    [(equal? o ns) '()]
    [else (cons ns (all-steps o ns))]))

; can origin be seen by destination
(define (visible? cs o d)
  (define steps (all-steps o d))
  (define common (set-intersect cs steps))
  (= 0 (length common)))

(module+ test
  (define c2 '((1 0) (3 0) (4 0)))
  (check-equal? (visible? c2 '(1 0) '(4 0)) #f))

(define (count-visible cs o)
  (define ocs (filter (lambda (n) (not (equal? o n))) cs))
  (define r (map (lambda (d) (visible? cs o d)) ocs))
  (count (curry eq? #t) r))

(module+ test
  (check-equal? (count-visible c1 (first c1)) 7))

; find coordinate for max number
(define (find-best cs)
  (define v (map (curry count-visible cs) cs))
  (argmax car (map list v cs)))

(module+ test
  (check-equal? (find-best c1) '(8 (3 4))))

(define (part1)
  (define c (asteroid-coordinates (load-file "./input.txt")))
  (println (find-best c)))
