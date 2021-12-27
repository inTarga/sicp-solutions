; 1.29

(define (simpsons f a b n)
  (define h (/ (- b a ) n))
  (define (iter i)
    (if (>= i n)
      0
      (+ (* (if (even? i) 2 4)
            (f (+ a (* i h))))
         (iter (+ i 1)))))
  (* (/ h 3) (+ (f a)
                (iter 1)
                (f (+ a (* n h))))))

(display "\n(simpsons cube 0 1 100): ")
(display (simpsons cube 0 1 100))
(display "\n(simpsons cube 0 1 1000): ")
(display (simpsons cube 0 1 1000))

; 1.30

(define (sum term a next b)
  (define (iter a result)
    (if (> a b)
      result
      (iter (next a) (+ (term a) result))))
  (iter a 0))

(define (inc n) (+ n 1))

(display "\n\n(sum cube 1 inc 10): ")
(display (sum cube 1 inc 10))

; 1.31

(define (product term a next b)
  (if (> a b)
    1
    (* (term a)
       (product term (next a) next b))))

(define (identity x) x)

(define (factorial n)
  (product identity 1 inc n))

(display "\n\n(factorial 5): ")
(display (factorial 5))

(define (pi-term a)
  (if (even? a)
    (/ (+ a 2) (+ a 1))
    (/ (+ a 1) (+ a 2))))

(define (pi-product n)
  (* 4 (product pi-term 1 inc n)))

(display "\n(pi-product 10): ")
(display (pi-product 10))

(define (product-iter term a next b)
  (define (iter a result)
    (if (> a b)
      result
      (iter (next a) (* (term a) result))))
  (iter a 1))

(define (pi-product-iter n)
  (* 4 (product-iter pi-term 1 inc n)))

(display "\n(pi-product-iter 10): ")
(display (pi-product-iter 10))

; 1.32

(define (acc comb null term a next b)
  (if (> a b)
    null
    (comb (term a)
          (acc comb null term (next a) next b))))

(define (pi-acc n)
  (* 4 (acc * 1 pi-term 1 inc n)))

(display "\n\n(pi-acc 10): ")
(display (pi-acc 10))

(define (acc-iter comb null term a next b)
  (define (iter a result)
    (if (> a b)
      result
      (iter (next a) (comb (term a) result))))
  (iter a null))

(define (pi-acc-iter n)
  (* 4 (acc-iter * 1 pi-term 1 inc n)))

(display "\n(pi-acc-iter 10): ")
(display (pi-acc-iter 10))

; 1.33

(define (filt-acc comb null term a next b pred?)
  (if (> a b)
    null
    (comb (if (pred? a) (term a) null)
          (filt-acc comb null term (next a) next b pred?))))

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

(define (sum-prime-squares a b)
  (filt-acc + 0 square a inc b prime?))

(display "\n\n(sum-prime-squares 1 5): ")
(display (sum-prime-squares 1 5))

(define (gcd a b)
  (if (= b 0) a (gcd b (remainder a b))))

(define (prod-rel-primes n)
  (define (rel-prime? i)
    (= (gcd i n) 1))
  (filt-acc * 1 identity 1 inc n rel-prime?))

(display "\n(prod-rel-primes 10): ")
(display (prod-rel-primes 10))

; 1.34 - (f f) -> (f 2) -> (2 2) -> error: 2 is not applicable

; 1.35

(define tolerance 0.00001)

(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2))
       tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (display "\n next guess: ")
      (display next)
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))

(display "\n\nGolden Ratio: ")
; (display (fixed-point (lambda (x) (+ 1 (/ 1 x))) 1.0))
(fixed-point (lambda (x) (+ 1 (/ 1 x))) 1.0)

; 1.36

(define (average x y) (/ (+ x y) 2))

(display "\n\nx^x = 1000: ")
(fixed-point (lambda (x) (/ (log 1000) (log x))) 2.0)
(display "\n\nx^x = 1000 (average-damped): ")
(fixed-point (lambda (x)
               (average x (/ (log 1000) (log x))))
             2.0)

; 1.37

(define (cont-frac n d k)
  (define (iter n d k i)
    (if (>= i k)
      (/ (n k) (d k))
      (/ (n i) (+ (d i) (iter n d k (+ i 1))))))
  (iter n d k 1))

(display "\n\n1/Golden ratio (cont-frac): ")
(display (cont-frac (lambda (i) 1.0)
                    (lambda (i) 1.0)
                    12))

(define (cont-frac-iter n d k)
  (define (iter i prev)
    (if (<= i 0)
      prev
      (iter (- i 1) (/ (n i) (+ (d i) prev)))))
  (iter (- k 1) (/ (n k) (d k))))

(display "\n\n1/Golden ratio (cont-frac-iter): ")
(display (cont-frac-iter (lambda (i) 1.0)
                         (lambda (i) 1.0)
                         12))

; 1.38

(display "\n\n e: ")
(display (+ (cont-frac (lambda (i) 1.0)
                       (lambda (i)
                         (if (= (remainder i 3) 2)
                           (* (/ (+ i 1) 3) 2)
                           1.0))
                       100)
            2))

; 1.39

(define (tan-cf x k)
  (cont-frac (lambda (i) (if (= i 1) x (square x)))
             (lambda (i) (- (* 2 i) 1))
             k))

; 1.40

(define dx 0.00001)

(define (deriv g)
  (lambda (x)
    (/ (- (g (+ x dx)) (g x))
       dx)))

(define (newton-transform g)
  (lambda (x)
    (- x (/ (g x)
            ((deriv g) x)))))

(define (newtons-method g guess)
  (fixed-point (newton-transform g)
               guess))

(define (cubic a b c)
  (lambda (x) (+ (cube x)
                 (* a (square x))
                 (* b x)
                 c)))

(display "\n\nCubic estimate: ")
(newtons-method (cubic 2 (- 0 5) (- 0 6)) (- 0 1.5))

; 1.41

(define (double f)
  (lambda (x) (f (f x))))

(display "\n\n((double inc) 2): ")
(display ((double (lambda (x) (+ x 1))) 2))

; 1.42

(define (compose f g)
  (lambda (x) (f (g x))))

(display "\n\n((compose inc dec) 2): ")
(display ((compose (lambda (x) (+ x 1))
                   (lambda (x) (- x 1)))
          2))

; 1.43

(define (repeated f n)
  (lambda (x) (if (<= n 0)
                x
                (f ((repeated f (- n 1)) x)))))

(display "\n\n((repeated square 2) 5): ")
(display ((repeated square 2) 5))

; 1.44

(define (smooth f)
  (lambda (x) (average (f (- x dx))
                       (f (+ x dx)))))

(define (smooth-nfold n)
  (lambda (f) (repeated f n)))

; 1.45

(define (average-damp f)
  (lambda (x)
    (average x (f x))))

(define (pow x y)
  (let ((times-x (lambda (z) (* x z))))
    ((repeated times-x (- y 1)) x)))

(define (log2 x) (/ (log x) (log 2)))

(define (n-root x n)
  (fixed-point
    ((repeated average-damp (floor (log2 n)))
     (lambda (y) (/ x (pow y (- n 1)))))
    1.0))

(display "\n\n(n-root x n): ")
(n-root 32 5)

;1.46

(define (iterative-improve good-enough? improve)
  (define (iter guess)
    (if (good-enough? guess)
      guess
      (iter (improve guess))))
  (lambda (guess) (iter guess)))

(define (abstract-sqrt x)
  ((iterative-improve
     (lambda (guess)
       (< (abs (- (square guess) x)) 0.00001))
     (lambda (guess)
       (average guess (/ x guess))))
   1.0))

(display "\n\n(abstract-sqrt 16): ")
(display (abstract-sqrt 16))

(define (abstract-fixed-point f first-guess)
  ((iterative-improve
     (lambda (guess)
       (< (abs (- (f guess) guess)) 0.00001))
     (lambda (guess)
       (f guess)))
   1.0))

(display "\n\n(abstract-fixed-point cos 1.0): ")
(display (abstract-fixed-point cos 1.0))
