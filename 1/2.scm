; 1.9

; (define (+ a b)
;   (if (= a 0) b (inc (+ (dec a) b))))

; (+ 4 5)
; (inc (+ (dec 4) 5))
; (inc (+ 3 5))
; (inc (inc (+ (dec 3) 5)))
; (inc (inc (+ 2 5)))
; (inc (inc (inc (+ (dec 2) 5))))
; (inc (inc (inc (+ 1 5))))
; (inc (inc (inc (inc (+ (dec 1) 5)))))
; (inc (inc (inc (inc (+ 0 5)))))
; (inc (inc (inc (inc 5))))
; (inc (inc (inc 6)))
; (inc (inc 7))
; (inc 8)
; 9

; (define (+ a b)
;   (if (= a 0) b (+ (dec a) (inc b))))

; (+ 4 5)
; (+ (dec 4) (inc 5))
; (+ 3 6)
; (+ (dec 3) (inc 6))
; (+ 2 7)
; (+ (dec 2) (inc 7))
; (+ 1 8)
; (+ (dec 1) (inc 8))
; (+ 0 9)
; 9

; 1.10

(define (A x y)
  (cond ((= y 0) 0)
        ((= x 0) (* 2 y))
        ((= y 1) 2)
        (else (A (- x 1)
                 (A x (- y 1))))))

(display "\n(A 1 10): ")
(display (A 1 10))
(display "\n(A 2 4): ")
(display (A 2 4))
(display "\n(A 3 3): ")
(display (A 3 3))

(define (f n) (A 0 n))
(define (g n) (A 1 n))
(define (h n) (A 2 n))

(display "\n\n(f n) === 2n")
(display "\n(g n) === 0 for n=0, 2^n for n > 0")
(display "\n(h n) === 0 for n=0 2^2^2^2... n times for n > 0")

; 1.11

(define (f-rec n)
  (if (< n 3) n (+ (f-rec (- n 1))
                   (* 2 (f-rec (- n 2)))
                   (* 3 (f-rec (- n 3))))))

(define (f-iter n)
  (define (step prev prev2 prev3)
    (+ prev (* 2 prev2) (* 3 prev3)))
  (define (iter cnt prev prev2 prev3 n)
    (if (= cnt n)
      (step prev prev2 prev3)
      (iter (+ cnt 1)
            (step prev prev2 prev3)
            prev prev2 n)))
  (if (< n 3) n (iter 3 2 1 0 n)))

(display "\n\n(f-rec 5) = ")
(display (f-rec 5))
(display "\n(f-iter 5) = ")
(display (f-iter 5))

; 1.12

(define (pascal r e)
  (if (or (= e 1) (= e r) (<= r 2))
    1
    (+ (pascal (- r 1) (- e 1))
       (pascal (- r 1) e))))

(display "\n\nElement 3 row 5 in pascal's triange: ")
(display (pascal 5 3))

; 1.16

(define (expt b n)
  (if (= n 0)
      1
      (* b (expt b (- n 1)))))

(define (fast-expt-iter b n)
  (define (iter b counter product)
    (cond ((= 0 counter) product)
          ((even? counter) (iter (square b) (/ counter 2) product))
          (else (iter b (- counter 1) (* b product)))))
  (iter b n 1))

(display "\n\n(expt 7 13)           = ")
(display (expt 7 13))
(display "\n(fast-expt-iter 7 13) = ")
(display (fast-expt-iter 7 13))

; 1.17

(define (double a) (* a 2))
(define (halve a) (/ a 2))

(define (fast-mult a b)
  (cond ((= b 0) 0)
        ((even? b) (double (fast-mult a (halve b))))
        (else (+ a (fast-mult a (- b 1))))))

(display "\n\n(* 7 13)              = ")
(display (* 7 13))
(display "\n(fast-mult 7 13)      = ")
(display (fast-mult 7 13))

; 1.18

(define (fast-mult-iter a b)
  (define (iter a counter product)
    (cond ((= counter 0) product)
          ((even? counter) (iter (double a) (halve counter) product))
          (else (iter a (- counter 1) (+ product a)))))
  (iter a b 0))

(display "\n(fast-mult-iter 7 13) = ")
(display (fast-mult-iter 7 13))

; 1.19

(define (fib n)
  (cond ((= n 0) 0)
        ((= n 1) 1)
        (else (+ (fib (- n 1)) (fib (- n 2))))))

(define (fast-fib n)
  (define (fib-iter a b p q count)
    (cond ((= count 0) b)
          ((even? count)
           (fib-iter a
                     b
                     (+ (square p) (square q))
                     (+ (* 2 p q) (square q))
                     (/ count 2)))
          (else
            (fib-iter (+ (* b q)
                         (* a q)
                         (* a p))
                      (+ (* b p)
                         (* a q))
                      p
                      q
                      (- count 1)))))
  (fib-iter 1 0 0 1 n))

(display "\n\n(fib 13) = ")
(display (fib 13))
(display "\n(fast-fib 13) = ")
(display (fast-fib 13))

; 1.20

; ; normal order

; (gcd 206 40)

; (if (= 40 0) 206 (gcd 40 (remainder 206 40)))

; (gcd 40 (remainder 206 40))

; (if (= (remainder 206 40) 0) a gcd((remainder 206 40) (remainder 40 (remainder 206 40))))

; (if (= 6 0) a gcd((remainder 206 40)
;                   (remainder 40 (remainder 206 40))))
; ; 1
; (gcd((remainder 206 40)
;     (remainder 40 (remainder 206 40))))
; ; 1
; (if (= (remainder 40 (remainder 206 40)) 0)
;   (remainder 206 40)
;   (gcd (remainder 40 (remainder 206 40))
;        (remainder
;          (remainder 206 40)
;          (remainder 40 (remainder 206 40))))) ; 1

; (if (= (remainder 40 6) 0)
;   (remainder 206 40)
;   (gcd (remainder 40 (remainder 206 40))
;        (remainder
;          (remainder 206 40)
;          (remainder 40 (remainder 206 40))))) ; 2

; (if (= 4 0)
;   (remainder 206 40)
;   (gcd (remainder 40 (remainder 206 40))
;        (remainder
;          (remainder 206 40)
;          (remainder 40 (remainder 206 40))))) ; 3

; (gcd (remainder 40 (remainder 206 40))
;      (remainder
;        (remainder 206 40)
;        (remainder 40 (remainder 206 40)))) ; 3

; (if (= (remainder
;          (remainder 206 40)
;          (remainder 40 (remainder 206 40))) 0)
;   (remainder 40 (remainder 206 40))
;   (gcd (remainder (remainder 206 40)
;                   (remainder 40 (remainder 206 40)))
;        (remainder (remainder 40 (remainder 206 40))
;                   (remainder (remainder 206 40)
;                              (remainder 40 (remainder 206 40))))))
; ; 3
; (if (= (remainder
;          (remainder 206 40)
;          (remainder 40 6)) 0)
;   (remainder 40 (remainder 206 40))
;   (gcd (remainder (remainder 206 40)
;                   (remainder 40 (remainder 206 40)))
;        (remainder (remainder 40 (remainder 206 40))
;                   (remainder (remainder 206 40)
;                              (remainder 40 (remainder 206 40))))))
; ; 4
; (if (= (remainder
;          6
;          (remainder 40 6)) 0)
;   (remainder 40 (remainder 206 40))
;   (gcd (remainder (remainder 206 40)
;                   (remainder 40 (remainder 206 40)))
;        (remainder (remainder 40 (remainder 206 40))
;                   (remainder (remainder 206 40)
;                              (remainder 40 (remainder 206 40))))))
; ; 5
; (if (= (remainder 6 4) 0)
;   (remainder 40 (remainder 206 40))
;   (gcd (remainder (remainder 206 40)
;                   (remainder 40 (remainder 206 40)))
;        (remainder (remainder 40 (remainder 206 40))
;                   (remainder (remainder 206 40)
;                              (remainder 40 (remainder 206 40))))))
; ; 6
; (if (= 2 0)
;   (remainder 40 (remainder 206 40))
;   (gcd (remainder (remainder 206 40)
;                   (remainder 40 (remainder 206 40)))
;        (remainder (remainder 40 (remainder 206 40))
;                   (remainder (remainder 206 40)
;                              (remainder 40 (remainder 206 40))))))
; ; 7
; (gcd (remainder (remainder 206 40)
;                 (remainder 40 (remainder 206 40)))
;      (remainder (remainder 40 (remainder 206 40))
;                 (remainder (remainder 206 40)
;                            (remainder 40 (remainder 206 40)))))
; ; 7
; (if (= (remainder (remainder 40 (remainder 206 40))
;                   (remainder (remainder 206 40)
;                              (remainder 40 (remainder 206 40))))
;        0)
;   (remainder (remainder 206 40)
;              (remainder 40 (remainder 206 40)))
;   (gcd (remainder (remainder 40 (remainder 206 40))
;                   (remainder (remainder 206 40)
;                              (remainder 40 (remainder 206 40))))
;        (remainder (remainder (remainder 206 40)
;                              (remainder 40 (remainder 206 40)))
;                   (remainder (remainder 40 (remainder 206 40))
;                              (remainder (remainder 206 40)
;                                         (remainder 40 (remainder 206 40)))))))
; ; 7
; (if (= (remainder (remainder 40 6)
;                   (remainder (remainder 206 40)
;                              (remainder 40 (remainder 206 40))))
;        0)
;   (remainder (remainder 206 40)
;              (remainder 40 (remainder 206 40)))
;   (gcd (remainder (remainder 40 (remainder 206 40))
;                   (remainder (remainder 206 40)
;                              (remainder 40 (remainder 206 40))))
;        (remainder (remainder (remainder 206 40)
;                              (remainder 40 (remainder 206 40)))
;                   (remainder (remainder 40 (remainder 206 40))
;                              (remainder (remainder 206 40)
;                                         (remainder 40 (remainder 206 40)))))))
; ; 8
; (if (= (remainder 4
;                   (remainder (remainder 206 40)
;                              (remainder 40 (remainder 206 40))))
;        0)
;   (remainder (remainder 206 40)
;              (remainder 40 (remainder 206 40)))
;   (gcd (remainder (remainder 40 (remainder 206 40))
;                   (remainder (remainder 206 40)
;                              (remainder 40 (remainder 206 40))))
;        (remainder (remainder (remainder 206 40)
;                              (remainder 40 (remainder 206 40)))
;                   (remainder (remainder 40 (remainder 206 40))
;                              (remainder (remainder 206 40)
;                                         (remainder 40 (remainder 206 40)))))))
; ; 9
; (if (= (remainder 4
;                   (remainder 6
;                              (remainder 40 (remainder 206 40))))
;        0)
;   (remainder (remainder 206 40)
;              (remainder 40 (remainder 206 40)))
;   (gcd (remainder (remainder 40 (remainder 206 40))
;                   (remainder (remainder 206 40)
;                              (remainder 40 (remainder 206 40))))
;        (remainder (remainder (remainder 206 40)
;                              (remainder 40 (remainder 206 40)))
;                   (remainder (remainder 40 (remainder 206 40))
;                              (remainder (remainder 206 40)
;                                         (remainder 40 (remainder 206 40)))))))
; ; 10
; (if (= (remainder 4
;                   (remainder 6
;                              (remainder 40 6)))
;        0)
;   (remainder (remainder 206 40)
;              (remainder 40 (remainder 206 40)))
;   (gcd (remainder (remainder 40 (remainder 206 40))
;                   (remainder (remainder 206 40)
;                              (remainder 40 (remainder 206 40))))
;        (remainder (remainder (remainder 206 40)
;                              (remainder 40 (remainder 206 40)))
;                   (remainder (remainder 40 (remainder 206 40))
;                              (remainder (remainder 206 40)
;                                         (remainder 40 (remainder 206 40)))))))
; ; 11
; (if (= (remainder 4
;                   (remainder 6 4))
;        0)
;   (remainder (remainder 206 40)
;              (remainder 40 (remainder 206 40)))
;   (gcd (remainder (remainder 40 (remainder 206 40))
;                   (remainder (remainder 206 40)
;                              (remainder 40 (remainder 206 40))))
;        (remainder (remainder (remainder 206 40)
;                              (remainder 40 (remainder 206 40)))
;                   (remainder (remainder 40 (remainder 206 40))
;                              (remainder (remainder 206 40)
;                                         (remainder 40 (remainder 206 40)))))))
; ; 12
; (if (= (remainder 4 2) 0)
;   (remainder (remainder 206 40)
;              (remainder 40 (remainder 206 40)))
;   (gcd (remainder (remainder 40 (remainder 206 40))
;                   (remainder (remainder 206 40)
;                              (remainder 40 (remainder 206 40))))
;        (remainder (remainder (remainder 206 40)
;                              (remainder 40 (remainder 206 40)))
;                   (remainder (remainder 40 (remainder 206 40))
;                              (remainder (remainder 206 40)
;                                         (remainder 40 (remainder 206 40)))))))
; ; 13
; (if (= 0 0)
;   (remainder (remainder 206 40)
;              (remainder 40 (remainder 206 40)))
;   (gcd (remainder (remainder 40 (remainder 206 40))
;                   (remainder (remainder 206 40)
;                              (remainder 40 (remainder 206 40))))
;        (remainder (remainder (remainder 206 40)
;                              (remainder 40 (remainder 206 40)))
;                   (remainder (remainder 40 (remainder 206 40))
;                              (remainder (remainder 206 40)
;                                         (remainder 40 (remainder 206 40)))))))
; ; 14 - reduction done
; (remainder (remainder 206 40)
;            (remainder 40 (remainder 206 40)))
; ; 14
; (remainder 6
;            (remainder 40 (remainder 206 40)))
; ; 14 + 1 = 15
; (remainder 6 (remainder 40 6))
; ; 14 + 2 = 16
; (remainder 6 4)
; ; 14 + 3 = 17
; 2
; ; 14 + 4 = 18 fuck this

; ; applicative order

; (gcd 206 40)
; (if (= 40 0) 206 (gcd 40 (remainder 206 40)))
; (gcd 40 (remainder 206 40))
; (gcd 40 6) ; 1
; (if (= 6 0) 40 (gcd 6 (remainder 40 6))) ; 1
; (gcd 6 (remainder 40 6)) ; 1
; (gcd 6 4) ; 2
; (if (= 4 0) 6 (gcd 4 (remainder 6 4))) ; 2
; (gcd 4 (remainder 6 4)) ; 2
; (gcd 4 2) ; 3
; (if (= 2 0) 4 (gcd 2 (remainder 4 2))) ; 3
; (gcd 2 (remainder 4 2)) ; 3
; (gcd 2 0) ; 4
; (if (= 0 0) 2 (gcd 0 (remainder 2 0))) ; 4
; 2 ; 4

; 1.21

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

(display "\n\n(smallest-divisor 199) = ")
(display (smallest-divisor 199))
(display "\n(smallest-divisor 1999) = ")
(display (smallest-divisor 1999))
(display "\n(smallest-divisor 19999) = ")
(display (smallest-divisor 19999))

; 1.22

(define (prime? n)
  (= n (smallest-divisor n)))

(define (timed-prime-test n)
  (define (report-prime elapsed-time)
    (display " *** ")
    (display elapsed-time))
  (define (start-prime-test n start-time)
    (if (prime? n)
      (report-prime (- (runtime)
                       start-time))))
  (newline)
  (display n)
  (start-prime-test n (runtime)))

(define (search-for-primes b n)
  (cond ((even? b) (search-for-primes (+ b 1) n))
        ((> n 0) (timed-prime-test b)
                 (search-for-primes
                   (+ b 2)
                   (if (prime? b) (- n 1) n)))))

(display "\n\n Searching for lowest 3 primes over 1000")
; (search-for-primes 1000 3)
(display "\n Searching for lowest 3 primes over 10000")
; (search-for-primes 10000 3)
(display "\n Searching for lowest 3 primes over 100000")
; (search-for-primes 100000 3)

; 1.23

(define (smallest-divisor-smart n)
  (define (next test-divisor)
    (if (= test-divisor 2) 3 (+ test-divisor 2)))
  (define (divides? a b)
    (= (remainder b a) 0))
  (define (find-divisor n test-divisor)
    (cond ((> (square test-divisor) n)
           n)
          ((divides? test-divisor n)
           test-divisor)
          (else (find-divisor
                  n
                  (next test-divisor)))))
  (find-divisor n 2))

(display "\n\n(smallest-divisor 49) = ")
(display (smallest-divisor 49))
(display "\n(smallest-divisor-smart 49) = ")
(display (smallest-divisor-smart 49))

;
