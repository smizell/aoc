#lang racket

(require rackunit
         threading)

(define (parse-commands inputs)
  (~> inputs
      (string-split ",")
      (map parse-command _)))

(module+ test
  (check-equal? (parse-commands "R102,U50") '(("R" 102) ("U" 50))))

(define (parse-command input)
  (list (substring input 0 1)
        (string->number (substring input 1))))

(module+ test
  (check-equal? (parse-command "R102") '("R" 102)))

; Convert a command like R5 to a line with beginning and end
(define (command->line command cursor)
  (match command
    [(list "R" distance)
     (let ([x (first cursor)]
           [y (second cursor)])
       (list cursor (list (+ x distance) y)))]
    [(list "U" distance)
     (let ([x (first cursor)]
           [y (second cursor)])
       (list cursor (list x (+ y distance))))]
    [(list "L" distance)
     (let ([x (first cursor)]
           [y (second cursor)])
       (list cursor (list (- x distance) y)))]
    [(list "D" distance)
     (let ([x (first cursor)]
           [y (second cursor)])
       (list cursor (list x (- y distance))))]))

(module+ test
  (check-equal? (command->line '("R" 3) '(0 0)) '((0 0) (3 0)))
  (check-equal? (command->line '("U" 3) '(0 0)) '((0 0) (0 3)))
  (check-equal? (command->line '("L" 3) '(0 0)) '((0 0) (-3 0)))
  (check-equal? (command->line '("D" 3) '(0 0)) '((0 0) (0 -3))))

; Look for crosses based on a horizontal and vertical line
(define (crosses? h v)
  (match (list h v)
    ; Horizontal on 0 vs vertical on 0
    ; Crossing at (0 0) doesn't count
    [(list (list (list 0 ay1) (list 0 ay2))
           (list (list bx1 0) (list bx2 0)))
     (not (crosses-point? (list ay1 ay2) 0))]
    ; Horizontal vs vertical
    [(list (list (list ax1 ay) (list ax2 ay))
           (list (list bx by1) (list bx by2)))
     (and (crosses-point? (list by1 by2) ay)
          (crosses-point? (list ax1 ax2) bx))]))

(define (crosses-point? p y)
  (let* ([p (sort p <)]
         [p1 (first p)]
         [p2 (second p)])
    (and (< p1 y)
         (> p2 y))))

(module+ test
  (check-equal? (crosses? '((3 6) (5 6)) '((4 5) (4 7))) #t)
  (check-equal? (crosses? '((0 -1) (0 3)) '((-2 0) (2 0))) #f))

(define (horizontal? line)
  (match line
    [(list (list a y) (list b y)) #t]
    [_ #f]))

(define (vertical? line) (not (horizontal? line)))

(module+ test
  (check-equal? (horizontal? '((3 6) (5 6))) #t)
  (check-equal? (horizontal? '((3 6) (5 7))) #f)
  (check-equal? (vertical? '((4 5) (4 7))) #t))

(define (find-crosses as bs)
  (for*/fold ([acc '()])
             ([a as]
              [b bs])
    (cond
      [(and (vertical? a) (horizontal? b) (crosses? b a))
       (append acc (list (crossing-point b a)))]
      [(and (horizontal? a) (vertical? b) (crosses? a b))
       (append acc (list (crossing-point a b)))]
      [else acc])))

(define (crossing-point a b)
  (list (first (first b)) (second (first a))))

(module+ test
  (check-equal? (find-crosses '(((3 6) (5 6))) '(((4 5) (4 7)))) '((4 6)))
  (check-equal? (find-crosses '(((4 5) (4 7))) '(((3 6) (5 6)))) '((4 6)))
  (check-equal? (find-crosses '(((2 5) (2 7))) '(((3 3) (5 3)))) '()))


(define (calc-shortest-distance crosses)
  (first (sort (map (lambda (c) (+ (abs (first c)) (abs (second c)))) crosses) <)))

(module+ test
  (check-equal? (calc-shortest-distance '((8 3) (4 6) (6 7))) 10))

(define (commands->lines cs cursor)
  (match cs
    [(list c) (list (command->line c cursor))]
    [(list c r ...)
     (let* ([l (command->line c cursor)]
            [nc (second l)])
       (cons l (commands->lines r nc)))]))

(module+ test
  (check-equal? (commands->lines '(("R" 8) ("U" 5) ("L" 5) ("D" 3)) '(0 0))
                '(((0 0) (8 0))
                  ((8 0) (8 5))
                  ((8 5) (3 5))
                  ((3 5) (3 2)))))

(define (shortest-distance i1 i2)
  (let* ([c1 (parse-commands i1)]
         [c2 (parse-commands i2)]
         [l1 (commands->lines c1 '(0 0))]
         [l2 (commands->lines c2 '(0 0))]
         [crosses (find-crosses l1 l2)])
    (calc-shortest-distance crosses)))

(module+ test
  (check-equal? (shortest-distance "R75,D30,R83,U83,L12,D49,R71,U7,L72"
                                   "U62,R66,U55,R34,D71,R55,D58,R83")
                159)
  (check-equal? (shortest-distance "R98,U47,R26,D63,R33,U87,L62,D20,R33,U53,R51"
                                   "U98,R91,D20,R16,D67,R40,U7,R15,U6,R7")
                135)
  (check-equal? (shortest-distance "R8,U5,L5,D3" "U7,R6,D4,L4") 6))

(define inputs
  (~>> "./input.txt"
       (file->string)
       (string-trim)
       (string-split _ "\n")))

(define (part1)
  (shortest-distance (first inputs) (second inputs)))