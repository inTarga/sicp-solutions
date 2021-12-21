; 1.2

(define
  prefix
  (/
    (+
      5
      4
      (-
        2
        (-
          3
          (+
            6
            (/ 4 5)))))
    (*
      3
      (- 6 2)
      (- 2 7))))

; 1.3

(define (largest x y)
  (if (> x y)
    x
    y))

(define (smallest x y)
  (if (> x y)
    y
    x))

(define (sum-squares x y)
  (+ (* x x) (* y y)))

(define (larger-sum-squares x y z)
  (sum-squares
    (largest x y)
    (largest
      (smallest x y)
      z)))

; 1.6

(define (good-enough? guess x)
  (< (abs (- (square guess) x)) 0.001))

(define (average x y)
  (/ (+ x y) 2))

(define (improve guess x)
  (average guess (/ x guess)))

(define (sqrt-iter guess x)
  (if (good-enough? guess x)
    guess
    (sqrt-iter (improve guess x) x)))

(define (sqrt x)
  (sqrt-iter 1.0 x))

(define (new-if predicate then-clause else-clause)
  (cond (predicate then-clause)
        (else else-clause)))

(define (new-sqrt-iter guess x)
  (new-if (good-enough? guess x)
          guess
          (new-sqrt-iter (improve guess x) x)))

(define (new-sqrt x)
  (new-sqrt-iter 1.0 x))

; 1.7

(define (better-good-enough? guess last)
  (= guess last))

(define (better-sqrt-iter guess last x)
  (if (better-good-enough? guess last)
    guess
    (better-sqrt-iter (improve guess x) guess x)))

(define (better-sqrt x)
  (better-sqrt-iter 1.0 0.0 x))

(newline)
(display (better-sqrt 0.0001))
(newline)
(display (better-sqrt 10000000000000))
(newline)

; 1.8

(define (improve-curt guess x)
  (/ (+ (/ x (square guess)) (* 2 guess)) 3))

(define (curt-iter guess last x)
  (if (better-good-enough? guess last)
    guess
    (curt-iter (improve-curt guess x) guess x)))

(define (curt x)
  (curt-iter 1.0 0.0 x))

(display (curt 27))
(newline)
