; 2.17

(define (last-pair l)
  (let ((tail (cdr l)))
    (if (null? tail)
      (car l)
      (last-pair tail))))

(last-pair (list 23 72 149 34)) ; 34

; 2.18

(define (reverse l)
  (define (iter l1 l2)
    (if (null? l1)
      l2
      (iter (cdr l1) (cons (car l1) l2))))
  (iter (cdr l) (list (car l))))

(reverse (list 1 4 9 16 25)) ; (25 16 9 4 1)

; 2.19

(define (cc amount coin-values)
  (define (first-denomination cl) (car cl))
  (define (except-first-denomination cl) (cdr cl))
  (define (no-more? cl) (null? cl))
  (cond ((= amount 0) 1)
        ((or (< amount 0)
             (no-more? coin-values))
         0)
        (else
         (+ (cc amount
                (except-first-denomination coin-values))
            (cc (- amount
                   (first-denomination coin-values))
                coin-values)))))

(define us-coins
  (list 50 25 10 5 1))

(cc 100 us-coins) ; 292
(= (cc 100 us-coins) (cc 100 (reverse us-coins))) ; #t

; in reality, the number of ways to change is independent
; of the order of coins, because there is no order, it's
; a set, not a list. in this process, the order doesn't
; matter either, the reasoning about eliminating a coin
; from the list makes no assumptions of which coin you
; choose to eliminate

; 2.20

(define (same-parity head . tail)
  ; re-implementing filter, as I thought it might be
  ; cheating to use it since it hasn't been introduced
  (define (my-filter pred? l)
    (if (null? l)
      l
      (if (pred? (car l))
        (cons (car l) (my-filter pred? (cdr l)))
        (my-filter pred? (cdr l)))))
  (let ((same-parity? (if (even? head) even? odd?)))
    (cons head (my-filter same-parity? tail))))

(same-parity 1 2 3 4 5 6 7) ; (1 3 5 7)
(same-parity 2 3 4 5 6 7)   ; (2 4 6)

; 2.21

(define (stupid-square-list items)
  (if (null? items)
    items
    (cons (square (car items))
          (stupid-square-list (cdr items)))))

(define (square-list items)
  (map (lambda (x) (square x)) items))

(stupid-square-list (list 1 2 3 4)) ; (1 4 9 16)
(square-list        (list 1 2 3 4)) ; (1 4 9 16)

; 2.22

; interchanging the arguments to cons, doesn't reverse a
; list, instead it makes a fully unbalanced tree:
; (1 2 3 4) -> ((((() 4) 3) 2) 1)

(cons 1 (cons 2 (cons 3 (cons 4 '())))) ; (1 2 3 4)
(cons (cons (cons (cons '() 4) 3) 2) 1) ; ((((() . 4) . 3) . 2) . 1)

; 2.23

(define(my-for-each f l)
  (f (car l)) ; doesn't handle empty lists
  (if (null? (cdr l))
    (cdr l)
    (my-for-each f (cdr l))))

(for-each
 (lambda (x) (newline) (display x))
 (list 57 321 88))
; (out) 57
; (out) 321
; (out) 88
; (out) ;Unspecified return value

(my-for-each
 (lambda (x) (newline) (display x))
 (list 57 321 88))
; (out) 57
; (out) 321
; (out) 88
; ()

; 2.24

(list 1 (list 2 (list 3 4))) ; (1 (2 (3 4)))

; (1 (2 (3 4))) (2 (3 4)) (3 4)
;       v          v        v
;     [ | ] ---> [ | ] -> [ | ] > [ | x]
;      v          v        v       v
;    [ |x]      [ |x]    [ |x]     4
;     v          v        v
;     1          2        3

;  (1 (2 (3 4)))
;  /      \
; 1    (2 (3 4))
;      /     \
;     2     (3 4)
;           /   \
;          3     4

; 2.25

(car (cdr (car (cdr (cdr (list 1 3 (list 5 7) 9))))))
(car (car (list (list 7)))) ; 7
(car (cdr (car (cdr (car (cdr (car (cdr (car (cdr (car (cdr (list 1 (list 2 (list 3 (list 4 (list 5 (list 6 7)))))))))))))))))) ; 7

; 2.26

(define my-x (list 1 2 3))
(define my-y (list 4 5 6))

(append my-x my-y) ; (1 2 3 4 5 6)
(cons my-x my-y) ; ((1 2 3) 4 5 6)
(list my-x my-y) ; ((1 2 3) (4 5 6))

; 2.27

(define (deep-reverse l)
  (define (iter l1 l2)
    (if (null? l1)
      l2
      (iter (cdr l1)
            (cons (deep-reverse (car l1)) l2))))
  (if (pair? l)
    (iter (cdr l) (list (deep-reverse (car l))))
    l))

(define my-x (list (list 1 2) (list 3 4)))
(reverse my-x) ; ((3 4) (1 2))
(deep-reverse my-x) ; ((4 3) (2 1))

; 2.28

(define (fringe tree)
  (define (iter t l)
    (if (pair? t)
      (iter (car t) (iter (cdr t) l))
      (if (null? t)
        l
        (cons t l))))
  (iter tree '()))

(fringe my-x) ; (1 2 3 4)
(fringe (list my-x my-x)) ; (1 2 3 4 1 2 3 4)

; 2.29

; (define (make-mobile left right)
;   (list left right))
(define (make-mobile left right)
  (cons left right))
(define (left-branch mobile) (car mobile))
; (define (right-branch mobile) (car (cdr mobile)))
(define (right-branch mobile) (cdr mobile))

; (define (make-branch length structure)
;   (list length structure))
(define (make-branch length structure)
  (cons length structure))
(define (branch-length branch) (car branch))
; (define (branch-structure branch) (car (cdr branch)))
(define (branch-structure branch) (cdr branch))

(define (total-weight mobile)
  (if (pair? mobile)
    (+ (total-weight
         (branch-structure
           (left-branch mobile)))
       (total-weight
         (branch-structure
           (right-branch mobile))))
    mobile))

(define my-mobile
  (make-mobile
    (make-branch 1 2)
    (make-branch
      1
      (make-mobile
        (make-branch 1 3)
        (make-branch 1 4)))))

(total-weight my-mobile) ; 9

(define (balanced? mobile)
  (define (branch-torque branch)
    (* (branch-length branch)
       (total-weight (branch-structure branch))))
  (if (pair? mobile)
    (and (= (branch-torque (left-branch mobile))
            (branch-torque (right-branch mobile)))
         (balanced?
           (branch-structure (left-branch mobile)))
         (balanced?
           (branch-structure (right-branch mobile))))
    #t))

(define my-balanced-mobile
  (make-mobile
    (make-branch 2 3)
    (make-branch
      1
      (make-mobile
        (make-branch 1 3)
        (make-branch 1 3)))))

(balanced? my-mobile) ; #f
(balanced? my-balanced-mobile) ; #t

; barely, just remove a call to car in right selectors

; 2.30

(define (square-tree-stupid tree)
  (if (null? tree)
    tree
    (cons
      (if (pair? (car tree))
        (square-tree-stupid (car tree))
        (square (car tree)))
      (square-tree-stupid (cdr tree)))))

(define (square-tree-map tree)
  (map (lambda (subtree)
         (if (pair? subtree)
           (square-tree-map subtree)
           (square subtree)))
       tree))

(square-tree-stupid
  (list 1
        (list 2 (list 3 4) 5)
        (list 6 7))) ; (1 (4 (9 16) 25) (36 49))

(square-tree-map
  (list 1
        (list 2 (list 3 4) 5)
        (list 6 7))) ; (1 (4 (9 16) 25) (36 49))

; 2.31

(define (tree-map f tree)
  (map (lambda (subtree)
         (if (pair? subtree)
           (tree-map f subtree)
           (f subtree)))
       tree))

(define (square-tree tree)
  (tree-map square tree))

(square-tree
  (list 1
        (list 2 (list 3 4) 5)
        (list 6 7))) ; (1 (4 (9 16) 25) (36 49))

; 2.32

(define (subsets s)
  (if (null? s)
    (list '())
    (let ((rest (subsets (cdr s))))
      (append rest (map
                     (lambda (elem)
                       (cons (car s) elem))
                     rest)))))

(subsets (list 1 2 3)) ; (() (3) (2) (2 3) (1) (1 3) (1 2) (1 2 3))

; (let ((head (car s))
;       (tail (cdr s)))
; the set of subsets of s, is the set of subsets of tail,
; plus the set of combinations of head with the subsets
; of tail. by exploiting this fact, we can recursively
; eliminate heads, until we only have to find the subsets
; of a unit set.)

; 2.33

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op
                      initial
                      (cdr sequence)))))

(define (my-map p seq)
  (accumulate (lambda (x y) (cons (p x) y))
              '() seq))

(my-map square (list 1 2 3 4)) ; (1 4 9 16)

(define (my-append seq1 seq2)
  (accumulate cons seq2 seq1))

(my-append (list 1 2 3) (list 4 5 6)) ; (1 2 3 4 5 6)

(define (my-length seq)
  (accumulate (lambda (x y) (+ 1 y)) 0 seq))

(my-length (list 1 2 3 4)) ; 4

; 2.34

(define (horner-eval x seq)
  (accumulate
    (lambda (ai higher)
      (+ ai (* x higher)))
    0
    seq))

(horner-eval 2 (list 1 3 0 5 0 1)) ; 79

; 2.35

(define (enumerate-tree tree)
  (cond ((null? tree) '())
        ((not (pair? tree)) (list tree))
        (else (append
               (enumerate-tree (car tree))
               (enumerate-tree (cdr tree))))))

(define (count-leaves t)
  (accumulate + 0 (map (lambda (node)
                         (if (pair? node)
                           (count-leaves node)
                           1))
                       t)))

(count-leaves (list (list 1 2) (list 3 4))) ; 4

; 2.36

(define (accumulate-n op init seqs)
  (if (null? (car seqs))
    '()
    (cons (accumulate op init (map car seqs))
          (accumulate-n op init (map cdr seqs)))))

(define my-seqs (list (list 1 2 3)
                      (list 4 5 6)
                      (list 7 8 9)
                      (list 10 11 12)))

(accumulate-n + 0 my-seqs) ; (22 26 30)

; 2.37

(define my-matrix (list (list 1 2 3 4)
                        (list 4 5 6 6)
                        (list 6 7 8 9)))

(define (dot-product v w)
  (accumulate + 0 (map * v w)))

(dot-product (car my-matrix)
             (car (cdr my-matrix))) ; 56

(define (matrix-*-vector m v)
  (map (lambda (mi) (dot-product mi v)) m))

(matrix-*-vector my-matrix (car my-matrix)) ; (30 56 80)

(define (transpose m)
  (accumulate-n cons '() m))

(transpose my-matrix) ; ((1 4 6) (2 5 7) (3 6 8) (4 6 9))

(define (matrix-*-matrix m n)
  (let ((cols (transpose n)))
    (map (lambda (mi)
           (map (lambda (nj)
                  (dot-product mi nj))
                cols))
         m)))

(matrix-*-matrix my-matrix (transpose my-matrix)) ; ((30 56 80) (56 113 161) (80 161 230))
(matrix-*-matrix (list (list 1 2)
                       (list 3 4))
                 (list (list 1 2)
                       (list 3 4))) ; ((7 10) (15 22))

; 2.38

(fold-right / 1 (list 1 2 3)) ; 3/2
(fold-left / 1 (list 1 2 3)) ; 1/6
(fold-right list '() (list 1 2 3)) ; (1 (2 (3 ())))
(fold-left list '() (list 1 2 3)) ; (((() 1) 2) 3)

; associativity. it should give the same result for the
;same arguments, regardless of their order

; 2.39

(define (reverse-fr seq)
  (fold-right (lambda (x y) (append y (list x))) '() seq))

(define (reverse-fl seq)
  (fold-left (lambda (x y) (cons y x)) '() seq))

(reverse-fr (list 1 2 3 4)) ; (4 3 2 1)
(reverse-fl (list 1 2 3 4)) ; (4 3 2 1)

; 2.40

(define (enumerate-interval low high)
  (if (> low high)
      '()
      (cons low
            (enumerate-interval
             (+ low 1)
             high))))

(define (flatmap proc seq)
  (accumulate append '() (map proc seq)))

(define (unique-pairs n)
  (flatmap (lambda (i)
             (map (lambda (j) (list i j))
                  (enumerate-interval 1 (- i 1))))
           (enumerate-interval 2 n)))

(unique-pairs 4) ; ((2 1) (3 1) (3 2) (4 1) (4 2) (4 3))

(define (prime? n)
  (define (smallest-divisor n)
    (define (divides? a b)
      (= (remainder b a) 0))
    (define (find-divisor n test-divisor)
      (cond ((> (square test-divisor) n)
             n)
            ((divides? test-divisor n)
             test-divisor)
            (else (find-divisor
                    n
                    (+ test-divisor 1)))))
    (find-divisor n 2))
  (= n (smallest-divisor n)))

(define (prime-sum-pairs n)
  (define (make-pair-sum pair)
    (list (car pair)
          (cadr pair)
          (+ (car pair) (cadr pair))))
  (define (prime-sum? pair)
    (prime? (+ (car pair) (cadr pair))))
  (map make-pair-sum
       (filter prime-sum? (unique-pairs n))))

(prime-sum-pairs 6) ; ((2 1 3) (3 2 5) (4 1 5) (4 3 7) (5 2 7) (6 1 7) (6 5 11))

; 2.41

(define (unique-triples n)
  (flatmap (lambda (i)
             (map (lambda (pair)
                    (append (list i) pair))
                  (unique-pairs (- i 1))))
           (enumerate-interval 3 n)))

(unique-triples 4) ; ((3 2 1) (4 2 1) (4 3 1) (4 3 2))

(define (s-sum-triples n s)
  (define (sum-s? triple)
    (= (accumulate + 0 triple)
       s))
  (filter sum-s? (unique-triples n)))

(s-sum-triples 5 8) ; ((4 3 1) (5 2 1))

; 2.42

(define (queens board-size)
  (define empty-board '())
  (define (safe? k positions)
    (define (safe-queen? queens diag-l row diag-h)
      (if (null? queens)
        #t
        (let ((queen-row (car (car queens)))
              (rest-of-queens (cdr queens)))
          (if (or (= queen-row row)
                  (= queen-row diag-l)
                  (= queen-row diag-h))
            #f
            (safe-queen?
                    rest-of-queens
                    (- diag-l 1)
                    row
                    (+ diag-h 1))))))
    (let ((new-queen-row (car (car positions)))
          (rest-of-queens (cdr positions)))
      (safe-queen?
        rest-of-queens
        (- new-queen-row 1)
        new-queen-row
        (+ new-queen-row 1))))
  (define (adjoin-position new-row k rest-of-queens)
    (append (list (list new-row k)) rest-of-queens))
  (define (queen-cols k)
    (if (= k 0)
      (list empty-board)
      (filter
        (lambda (positions)
          (safe? k positions))
        (flatmap
          (lambda (rest-of-queens)
            (map (lambda (new-row)
                   (adjoin-position
                     new-row k rest-of-queens))
                 (enumerate-interval 1 board-size)))
          (queen-cols (- k 1))))))
  (queen-cols board-size))

(null? (cdr (cdr (cdr (append (list (list 1 2)) (list (list 3 4) (list 5 6)))))))

(queens 3) ; ()
(queens 4) ; (((3 4) (1 3) (4 2) (2 1)) ((2 4) (4 3) (1 2) (3 1)))
(length (queens 8)) ; 92
;(length (queens 11)) ; 2680

; 2.43

; for simplicity, ignore the fact that calls to
; queen-cols get progressively more expensive for more
; cols.
;
; in 2.42, queen-cols is called recursively n times, for
; a time complexity of O(n)
;
; in Reasoner's version each recursion calls it n times,
; which in turn call it n times, this puts it on the
; order of O(n^n)
;
; for the case of an 8x8 board, this works out as ~2e6T,
; though because of the simplification we can probably
; treat this as an upper bound.

; 2.44

(define (up-split painter n)
  (if (= n 0)
    painter
    (let ((smaller (up-split painter (- n 1))))
      (below painter (beside smaller smaller)))))

; 2.45

(define (split dir1 dir2)
  (define (my-split painter n)
    (if (= n 0)
      painter
      (let ((smaller (my-split painter (- n 1))))
        (dir1 painter (dir2 smaller smaller))))))
  (lambda (painter n) (my-split painter n))

; 2.46

(define (make-vect x y) (cons x y))
(define (xcor-vect v) (car v))
(define (ycor-vect v) (cdr v))

(define (add-vect v1 v2)
  (let ((x1 (xcor-vect v1))
        (y1 (ycor-vect v1))
        (x2 (xcor-vect v2))
        (y2 (ycor-vect v2)))
    (make-vect (+ x1 x2) (+ y1 y2))))

(define (sub-vect v1 v2)
  (let ((x1 (xcor-vect v1))
        (y1 (ycor-vect v1))
        (x2 (xcor-vect v2))
        (y2 (ycor-vect v2)))
    (make-vect (- x1 x2) (- y1 y2))))

(define (scale-vect s v)
  (make-vect (* s (xcor-vect v))
             (* s (ycor-vect v))))

(define (vect->string v)
  (string-append "("
                 (number->string (xcor-vect v))
                 ","
                 (number->string (ycor-vect v))
                 ")"))

(define my-vect (make-vect 2 3))
(vect->string my-vect) ; "(2,3)"
(vect->string (add-vect my-vect my-vect)) ; "(4,6)"
(vect->string (sub-vect my-vect my-vect)) ; "(0,0)"
(vect->string (scale-vect 3 my-vect)) ; "(6,9)"

; 2.47

(define (make-frame origin edge1 edge2)
  (list origin edge1 edge2))
(define (origin-frame frame) (car frame))
(define (edge1-frame frame) (cadr frame))
(define (edge2-frame frame) (caddr frame))

(define (make-frame-cons origin edge1 edge2)
  (cons origin (cons edge1 edge2)))
(define (origin-frame-cons frame) (car frame))
(define (edge1-frame-cons frame) (cadr frame))
(define (edge2-frame-cons frame) (cddr frame))

(define my-frame (make-frame (make-vect 0 0)
                             (make-vect 1 0)
                             (make-vect 0 1)))
(define my-frame-cons (make-frame-cons (make-vect 0 0)
                                       (make-vect 1 0)
                                       (make-vect 0 1)))

(vect->string (origin-frame my-frame)) ; "(0,0)"
(vect->string (origin-frame-cons my-frame-cons)) ; "(0,0)"
(vect->string (edge1-frame my-frame)) ; "(1,0)"
(vect->string (edge1-frame-cons my-frame-cons)) ; "(1,0)"
(vect->string (edge2-frame my-frame)) ; "(0,1)"
(vect->string (edge2-frame-cons my-frame-cons)) ; "(0,1)"

; 2.48

(define (make-segment v1 v2) (cons v1 v2))
(define (start-segment s) (car s))
(define (end-segment s) (cdr s))

; 2.49

(define outline-painter
  (segments->painter
    (list (make-segment (make-vect 0 0)
                        (make-vect 0 1))
          (make-segment (make-vect 0 1)
                        (make-vect 1 1))
          (make-segment (make-vect 1 1)
                        (make-vect 1 0))
          (make-segment (make-vect 1 0)
                        (make-vect 0 0)))))

(define x-painter
  (segments->painter
    (list (make-segment (make-vect 0 0)
                        (make-vect 1 1))
          (make-segment (make-vect 0 1)
                        (make-vect 1 0)))))

(define diamond-painter
  (segments->painter
    (list (make-segment (make-vect 0.0 0.5)
                        (make-vect 0.5 1.0))
          (make-segment (make-vect 0.5 1.0)
                        (make-vect 1.0 0.5))
          (make-segment (make-vect 1.0 0.5)
                        (make-vect 0.5 0.0))
          (make-segment (make-vect 0.5 0.0)
                        (make-vect 0.0 0.5)))))

; not even going to try the wave painter, since I can't
; run and check this code

; 2.50

(define (flip-horiz painter)
  (transform-painter painter
                     (make-vect 1.0 0.0)
                     (make-vect 0.0 0.0)
                     (make-vect 1.0 1.0)))

(define (rotate180 painter)
  (transform-painter painter
                     (make-vect 1.0 1.0)
                     (make-vect 0.0 1.0)
                     (make-vect 1.0 0.0)))

(define (rotate270 painter)
  (transform-painter painter
                     (make-vect 0.0 1.0)
                     (make-vect 0.0 0.0)
                     (make-vect 1.0 1.0)))

; 2.51

(define (below painter1 painter2)
  (let ((split-point (make-vect 0.0 0.5)))
    (let ((paint-bottom
            (transform-painter
              painter1
              (make-vect 0.0 0.0)
              (make-vect 1.0 0.0)
              split-point))
          (paint-top
            (transform-painter
              painter2
              split-point
              (make-vect 1.0 0.5)
              (make-vect 0.0 1.0))))
      (lambda (frame)
        (paint-bottom frame)
        (paint-top frame)))))

(define (below painter1 painter2)
  (rotate90 (beside (rotate270 painter1)
                    (rotate270 painter2))))

; 2.52

; skipping, feels pointless to try without the graphics
; library
