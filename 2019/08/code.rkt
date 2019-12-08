#lang racket

(require threading
         math/matrix
         racket/draw)

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

(define (smallest-match-index mc)
  (foldl (lambda (n idx r)
           (if (> n r) r idx))
         0
         mc
         (range (length mc))))

(module+ test
  (define l1 '(((0 0 1) (1 2 3)) ((0 2 0) (0 1 1)) ((1 1 1) (3 3 3))))
  (define mc1 (match-count-zero l1))
  (check-equal? (smallest-match-index mc1) 2))

(define (part1-values ls)
  (let* ([mc (match-count-zero ls)]
         [idx (smallest-match-index mc)]
         [l (list-ref ls idx)]
         [fl (flatten l)]
         [t1 (count-match (lambda (n) (eq? 1 n)) fl)]
         [t2 (count-match (lambda (n) (eq? 2 n)) fl)])
    (values idx t1 t2)))

(module+ test
  (let-values ([(idx t1 t2) (part1-values l1)])
    (check-equal? (list idx t1 t2) (list 2 3 0))))

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
(define idx (smallest-match-index mc))

(define (part1)
  (let-values ([(idx t1 t2) (part1-values (input->layers input 25 6))])
    (* t1 t2)))


(define (number->color n)
  (match n
    [0 'black]
    [1 'white]))

(define (number->ascii n)
  (match n
    [0 " "]
    [1 "*"]))

(define (layers->pixels ls)
  (let* ([xl (length (flatten (first ls)))]
         [yl (length ls)]
         [fls (flatten ls)]
         [mls (list->matrix yl xl fls)]
         [tmls (matrix-transpose mls)]
         [tfls (matrix->list tmls)]
         [nls (chop-up tfls yl)])
    nls))

(define (find-color cs)
  (findf (lambda (n) (or (eq? n 1) (eq? n 0))) cs))

(define (pixels->colors pxs)
  (let* ([l (length pxs)]
         [ns (map find-color pxs)]
         [cs (map number->color ns)])
    cs))

(define (pixels->ascii pxs)
  (let* ([l (length pxs)]
         [ns (map find-color pxs)]
         [cs (map number->ascii ns)])
    cs))

(module+ test
  (define l2 '(((0 2)
                (2 2))
               ((1 1)
                (2 2))
               ((2 2)
                (1 2))
               ((0 0)
                (0 0))))
  (define pxs '((0 1 2 0) (2 1 2 0) (2 2 1 0) (2 2 2 0)))
  (check-equal? (layers->pixels l2) pxs)
  (check-equal? (pixels->colors (layers->pixels l2))
                '(black white white black)))

(define (part2)
  (let ([a (pixels->ascii (layers->pixels ls))])
    (for ([l (map string-join (chop-up a 25))])
      (println l))))

