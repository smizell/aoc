#lang racket

(require threading)

(module+ test (require rackunit))

(define (chop-up l n [acc '()])
  (cond
    [(eq? (length l) n) (append acc (list l))]
    [else (chop-up (drop l n) n (append acc (list (take l n))))]))

(define (build-layers s x y)
  (map (lambda (n) (chop-up n x)) (chop-up s (* x y))))

(module+ test
  (define n1 '(1 2 3 4 5 6 7 8 9 0 1 2))
  (check-equal? (build-layers n1 3 2)
                '(((1 2 3) (4 5 6)) ((7 8 9) (0 1 2)))))

(define (count-match fn l)
  (length (filter fn l)))

(module+ test
  (check-equal? (count-match (lambda (n) (eq? n 0)) n1) 1))

(define (count-match-layers fn ls)
  (map (lambda (l) (count-match fn (flatten l))) ls))

(module+ test
  (define layers (build-layers n1 3 2))
  (define match-count (count-match-layers (lambda (n) (eq? n 0)) layers))
  (check-equal? match-count '(0 1)))

(define (match-count-zero ls)
  (count-match-layers (lambda (n) (eq? n 0)) ls))

(define (largest-match-index mc)
  (foldl (lambda (n r) (if (> r n) r n)) 0 mc))

(module+ test
  (define l1 '(((0 2 0) (0 1 1)) ((0 0 1) (1 2 3)) ((1 1 1) (3 3 3))))
  (define mc1 (match-count-zero l1))
  (println mc1)
  (check-equal? (largest-match-index mc1) 0))

(define (part1-values ls)
  (let* ([mc (match-count-zero ls)]
         [idx (largest-match-index mc)]
         [l (list-ref ls idx)]
         [fl (flatten l)]
         [t1 (count-match (lambda (n) (eq? 1 n)) fl)]
         [t2 (count-match (lambda (n) (eq? 2 n)) fl)])
    (values idx t1 t2)))

(module+ test
  (let-values ([(idx t1 t2) (part1-values l1)])
    (check-equal? (list idx t1 t2) (list 0 2 1))))

(define input
  (string-trim (file->string "./input.txt")))

(define (input->layers i x y)
  (~>> i
       (string->list)
       (map string)
       (map string->number)
       (build-layers _ x y)))

(module+ test
  (check-equal? (input->layers "123456789012" 3 2) '(((1 2 3) (4 5 6)) ((7 8 9) (0 1 2)))))

(define ls (input->layers input 25 6))
(define mc (match-count-zero ls))
(define idx (largest-match-index mc))

(define (part1)
  (let-values ([(idx t1 t2) (part1-values (input->layers input 25 6))])
    (* t1 t2)))
