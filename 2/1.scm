; 2.1

(define (numer x) (car x))
(define (denom x) (cdr x))

(define (print-rat x)
  (string-append (number->string (numer x)) "/" (number->string(denom x))))

(define (make-rat n d)
  (let ((g (gcd n d)))
    (cons (/ (* n (/ d (abs d))) g)
          (/ (abs d) g))))

(print-rat (make-rat 5 -10)) ; "-1/2"
(print-rat (make-rat -5 -10)) ; "1/2"
(print-rat (make-rat -5 10)) ; "-1/2"

; 2.2

(define (x-point p) (car p))
(define (y-point p) (cdr p))
(define (make-point x y) (cons x y))
(define (print-point p)
  (string-append
    "("
    (number->string (x-point p))
    ","
    (number->string (y-point p))
    ")"))

(define (start-segment s) (car s))
(define (end-segment s)   (cdr s))
(define (make-segment p1 p2) (cons p1 p2))

(define (average x y) (/ (+ x y) 2))

(define (midpoint-segment s)
  (let ((x1 (x-point (start-segment s)))
        (y1 (y-point (start-segment s)))
        (x2 (x-point (end-segment s)))
        (y2 (y-point (end-segment s))))
    (make-point (average x1 x2)
                (average y1 y2))))

(print-point
  (midpoint-segment
    (make-segment (make-point 1 1)
                  (make-point 3 3)))) ; "(2,2)"

; 2.3

(define (nw-point r) (car r))
(define (se-point r) (cdr r))
(define (make-rectangle nw se) (cons nw se))

(define (width-rect r)
  (- (x-point (se-point r))
     (x-point (nw-point r))))
(define (height-rect r)
  (- (y-point (nw-point r))
     (y-point (se-point r))))

(define (h-rect r) (car r))
(define (w-rect r) (cdr r))
(define (make-alt-rect w h) (cons w h))

(define (perimeter-rect w h r)
  (* 2 (+ (w r) (h r))))
(define (area-rect w h r)
  (* (w r) (h r)))

(perimeter-rect width-rect height-rect
                (make-rectangle (make-point 1 3)
                                (make-point 3 1))) ; 8
(area-rect width-rect height-rect
           (make-rectangle (make-point 1 3)
                           (make-point 3 1))) ; 4

(perimeter-rect w-rect h-rect (make-alt-rect 2 2)) ; 8
(area-rect w-rect h-rect (make-alt-rect 2 2)) ; 4

; 2.4

(define (weird-cons x y)
  (lambda (m) (m x y)))
(define (weird-car z)
  (z (lambda (p q) p)))
(define (weird-cdr z)
  (z (lambda (p q) q)))

(weird-car (weird-cons 2 3)) ; 2
(weird-cdr (weird-cons 2 3)) ; 3

; 2.5

(define (get-num-fac n factor)
  (define (iter comp i)
    (if (= (remainder comp factor) 0)
      (iter (/ comp factor) (+ i 1))
      i))
  (iter n 0))

(define (arith-cons a b)
  (* (expt 2 a) (expt 3 b)))
(define (arith-car c)
  (get-num-fac c 2))
(define (arith-cdr c)
  (get-num-fac c 3))

(arith-car (arith-cons 2 3)) ; 2
(arith-cdr (arith-cons 2 3)) ; 3

; 2.6

(define zero (lambda (f) (lambda (x) x)))
(define one (lambda (f) (lambda (x) (f x))))
(define two (lambda (f) (lambda (x) (f (f x)))))

(define (add-1 n)
  (lambda (f) (lambda (x) (f ((n f) x)))))
(define (add a b)
  (lambda (f) (lambda (x) ((a f) ((b f) x)))))

(define (inc x) (+ x 1))
(define (church->int church)
  ((church inc) 0))

(church->int (add two two)) ; 4

; 2.7

(define (lower-bound intv) (car intv))
(define (upper-bound intv) (cdr intv))

; 2.8

(define (make-interval a b) (cons a b))

(define (sub-interval x y)
  ;; lowest possible result is (small - big)
  ;; highest possible result is (big - small)
  (make-interval (- (lower-bound x)
                    (upper-bound y))
                 (- (upper-bound x)
                    (lower-bound y))))

; 2.9

(define (add-interval x y)
  (make-interval (+ (lower-bound x)
                    (lower-bound y))
                 (+ (upper-bound x)
                    (upper-bound y))))

(define (mul-interval x y)
  (let ((p1 (* (lower-bound x)
               (lower-bound y)))
        (p2 (* (lower-bound x)
               (upper-bound y)))
        (p3 (* (upper-bound x)
               (lower-bound y)))
        (p4 (* (upper-bound x)
               (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))

(define (div-interval x y)
  (mul-interval x
                (make-interval
                 (/ 1.0 (upper-bound y))
                 (/ 1.0 (lower-bound y)))))

(define (width-interval intv)
  (/ (- (upper-bound intv) (lower-bound intv)) 2.0))

;   (width-interval (add-interval x y))
;
; = (/ (- (upper-bound (add-interval x y))
;         (lower-bound (add-interval x y))) 2)
;
; = (/ (- (+ (upper-bound x)
;            (upper-bound y))
;         (+ (lower-bound x)
;            (lower-bound y)))
;      2)
;
; = (/ (+ (- (upper-bound x)
;            (lower-bound x))
;         (- (upper-bound y)
;            (lower-bound y)))
;      2)
;
; = (/ (+ (* 2 (width-interval x))
;         (* 2 (width-interval y)))
;      2)
;
; = (+ (width-interval x) (width-interval y))

(define interval1 (make-interval 2.0 2.2))
(define interval2 (make-interval 2.5 3.2))
(width-interval interval1) ; .10000000000000009
(width-interval interval2) ; .3500000000000001
(width-interval (add-interval interval1 interval2)) ; .4500000000000002
(width-interval (sub-interval interval1 interval2)) ; .4500000000000002
(width-interval (mul-interval interval1 interval2)) ; 1.0200000000000005
(width-interval (div-interval interval1 interval2)) ; .12750000000000006

; 2.10

(define (safe-div-interval x y)
  (if (and (negative? (lower-bound y))
           (positive? (upper-bound y)))
    (error "denom straddles zero")
    (mul-interval x
                  (make-interval
                    (/ 1.0 (upper-bound y))
                    (/ 1.0 (lower-bound y))))))

(define interval3 (make-interval -0.5 1.0))
(safe-div-interval interval1 interval2) ; (.625 . .8800000000000001)
(safe-div-interval interval1 interval3) ; denom straddles zero

; 2.11

(define (ben-mul-interval x y)
  (define (neg-intv? intv)
    (negative? (upper-bound intv)))
  (define (pos-intv? intv)
    (positive? (lower-bound intv)))
  (define (makemul s1 s2 s3 s4)
    (make-interval (* (s1 x) (s2 y))
                   (* (s3 x) (s4 y))))
  (define u upper-bound)
  (define l lower-bound)
  (cond ((neg-intv? x) (cond ((neg-intv? y) (makemul u u l l))
                             ((pos-intv? y) (makemul l u u l))
                             (else          (makemul l u l l))))
        ((pos-intv? x) (cond ((neg-intv? y) (makemul u l l u))
                             ((pos-intv? y) (makemul l l u u))
                             (else          (makemul u l u u))))
        (else (cond ((neg-intv? y) (makemul u l l l))
                    ((pos-intv? y) (makemul l u u u))
                    (else (make-interval
                            (min (* (lower-bound x) (upper-bound y))
                                 (* (upper-bound x) (lower-bound y)))
                            (max (* (lower-bound x) (lower-bound y))
                                 (* (upper-bound x) (upper-bound y)))))))))

(define (eq-intv? a b)
  (and (= (lower-bound a) (lower-bound b))
       (= (upper-bound a) (upper-bound b))))

(define intv-pos (make-interval 1.0 2.0))
(define intv-neg (make-interval -2.0 -1.0))
(define intv-strad (make-interval -1.0 1.0))
(eq-intv? (mul-interval intv-neg intv-neg)
          (ben-mul-interval intv-neg intv-neg)) ; #t
(eq-intv? (mul-interval intv-neg intv-pos)
          (ben-mul-interval intv-neg intv-pos)) ; #t
(eq-intv? (mul-interval intv-neg intv-strad)
          (ben-mul-interval intv-neg intv-strad)) ; #t
(eq-intv? (mul-interval intv-pos intv-neg)
          (ben-mul-interval intv-pos intv-neg)) ; #t
(eq-intv? (mul-interval intv-pos intv-pos)
          (ben-mul-interval intv-pos intv-pos)) ; #t
(eq-intv? (mul-interval intv-pos intv-strad)
          (ben-mul-interval intv-pos intv-strad)) ; #t
(eq-intv? (mul-interval intv-strad intv-neg)
          (ben-mul-interval intv-strad intv-neg)) ; #t
(eq-intv? (mul-interval intv-strad intv-pos)
          (ben-mul-interval intv-strad intv-pos)) ; #t
(eq-intv? (mul-interval intv-strad intv-strad)
          (ben-mul-interval intv-strad intv-strad)) ; #t

; 2.12

(define (make-center-width c w)
  (make-interval (- c w) (+ c w)))
(define (center i)
  (/ (+ (lower-bound i)
        (upper-bound i))
     2))
(define (width i)
  (/ (- (upper-bound i)
        (lower-bound i))
     2))

(define (make-center-percent c p)
  (let ((w (* c p)))
    (make-center-width c w)))
(define (percent i)
  (/ (width i) (center i)))

(eq-intv? (make-center-width 10.0 1.0)
          (make-center-percent 10.0 0.1)) ; #t
(= 0.1 (percent (make-center-width 10.0 1.0))) ; #t

; 2.14

(define (par1 r1 r2)
  (div-interval
   (mul-interval r1 r2)
   (add-interval r1 r2)))

(define (par2 r1 r2)
  (let ((one (make-interval 1 1)))
    (div-interval
     one
     (add-interval
      (div-interval one r1)
      (div-interval one r2)))))

(eq-intv? (par1 intv-pos intv-pos)
          (par2 intv-pos intv-pos)) ; #f

(define intv-a (make-center-percent 2.0 0.001))
(define intv-b (make-center-percent 3.0 0.002))

(center (div-interval intv-a intv-a)) ; 1.000002000002
(percent (div-interval intv-a intv-a)) ; 1.999998000001888e-3

(center (div-interval intv-a intv-b)) ; .6666706666826667
(percent (div-interval intv-a intv-b)) ; 2.9999940000118744e-3

; 2.15

; Eva is right, par2 introduces less error by avoiding
; directly multiplying uncertain intervals

; 2.16

; They propagate errors differently, some arrangements
; are worse. I believe the task is possible, you could
; write a program to rearrange any arithmetic expression
; to minimise error propagation. I'm not even going to
; try this though.
