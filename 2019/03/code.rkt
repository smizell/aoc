#lang racket

(require threading)

(module+ test (require rackunit))

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
    [`(((,ax1 ,ay) (,ax2 ,ay)) ((,bx ,by1) (,bx ,by2)))
     (and (between? by1 by2 ay)
          (between? ax1 ax2 bx))]))

(module+ test
  (check-equal? (crosses? '((3 6) (5 6)) '((4 5) (4 7))) #t))

(define (horizontal? line)
  (match line
    [`((,a ,y) (,b ,y)) #t]
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

(define (crosses-from-inputs i1 i2)
  (let* ([c1 (parse-commands i1)]
         [c2 (parse-commands i2)]
         [l1 (commands->lines c1 '(0 0))]
         [l2 (commands->lines c2 '(0 0))])
    (remove '(0 0) (find-crosses l1 l2))))

(define (manhattan-distance i1 i2)
  (~> (crosses-from-inputs i1 i2)
      (calc-shortest-distance)))

(module+ test
  (check-equal? (manhattan-distance "R75,D30,R83,U83,L12,D49,R71,U7,L72"
                                   "U62,R66,U55,R34,D71,R55,D58,R83")
                159)
  (check-equal? (manhattan-distance "R98,U47,R26,D63,R33,U87,L62,D20,R33,U53,R51"
                                   "U98,R91,D20,R16,D67,R40,U7,R15,U6,R7")
                135)
  (check-equal? (manhattan-distance "R8,U5,L5,D3" "U7,R6,D4,L4") 6))

(define inputs
  (~>> "./input.txt"
       (file->string)
       (string-trim)
       (string-split _ "\n")))

(define (part1)
  (manhattan-distance (first inputs) (second inputs)))

(module+ test
  (check-equal? (part1) 870))

(define (point-on-line? line point)
  (match (list line point)
    ; Point on horizontal line where ys are the same
    [`(((,lx1 ,y) (,lx2 ,y)) (,px ,y))
     (between? lx1 lx2 px)]
    ; Point on vertical line where xs are the same
    [`(((,x ,ly1) (,x ,ly2)) (,x ,py))
     (between? ly1 ly2 py)]
    [_ #f]))

(define (between? a b n)
  (match-define (list c d) (sort (list a b) <))
  (and (> n c)
       (< n d)))

(module+ test
  (define y-line '((0 0) (10 0)))
  (define x-line '((5 0) (5 4)))
  (define p1 '(5 0))
  (define p2 '(5 1))
  (check-equal? (point-on-line? y-line p1) #t)
  (check-equal? (point-on-line? y-line p2) #f)
  (check-equal? (point-on-line? x-line p2) #t)
  (check-equal? (point-on-line? x-line p1) #f))

(define (distance-between a b)
  (match (list a b)
    [`((,x ,y1) (,x ,y2)) (abs (- y2 y1))]
    [`((,x1 ,y) (,x2, y)) (abs (- x1 x2))]))

(module+ test
  (define p3 '(5 1))
  (define p4 '(5 11))
  (define p5 '(8 11))
  (define p6 '(5 11))
  (check-equal? (distance-between p3 p4) 10)
  (check-equal? (distance-between p5 p6) 3))

; Takes a line and point and returns an integer
(define (distance-to-point ls p c)
  (let ([l (first ls)]
        [rls (rest ls)])
    (cond
      ; We're done if we make it to the point
      [(point-on-line? l p)
       (+ (distance-between (first l) p) c)]
      [else
       (+ (distance-between (first l) (second l))
          (distance-to-point rls p c))])))

(module+ test
  (define ls1 (commands->lines '(("R" 8) ("U" 5) ("L" 5) ("D" 3)) '(0 0)))
  (define ls2 (commands->lines '(("U" 7) ("R" 6) ("D" 4) ("L" 4)) '(0 0)))
  (check-equal? (distance-to-point ls1 '(6 5) 0) 15)
  (check-equal? (distance-to-point ls2 '(6 5) 0) 15))

(define (signal-delay i1 i2)
  (let* ([c1 (parse-commands i1)]
         [c2 (parse-commands i2)]
         [l1 (commands->lines c1 '(0 0))]
         [l2 (commands->lines c2 '(0 0))]
         [crosses (remove '(0 0) (find-crosses l1 l2))])
    (first (sort (map (lambda (c)
                        (let ([d1 (distance-to-point l1 c 0)]
                              [d2 (distance-to-point l2 c 0)])
                          (+ d1 d2)))
                      crosses)
                 <))))

(module+ test
  (check-equal? (signal-delay "R75,D30,R83,U83,L12,D49,R71,U7,L72"
                                   "U62,R66,U55,R34,D71,R55,D58,R83")
                610))

(define (part2)
  (signal-delay (first inputs) (second inputs)))

(module+ test
  (check-equal? (part2) 13698))
