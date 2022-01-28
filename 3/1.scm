; 3.1

(define (make-accumulator sum)
  (define (accumulate amt)
    (begin (set! sum (+ sum amt))
           sum))
  accumulate)

(define A (make-accumulator 5))
(A 10) ; 15
(A 10) ; 25

; 3.2

(define (make-monitored f)
  (let ((calls 0))
    (define (apply x)
      (if (eq? x 'how-many-calls?)
        calls
        (begin (set! calls (+ calls 1))
               (f x))))
    apply))

(define s (make-monitored sqrt))
(s 100) ; 10
(s 'how-many-calls?) ; 1

; 3.3 and 3.4

(define (make-account balance password)
  (let ((tries-left 7))

    (define (call-the-cops)
      "NeeNooNeeNooNeeNoo")

    (define (withdraw amount)
      (if (>= balance amount)
        (begin (set! balance
                 (- balance amount))
               balance)
        "Insufficient funds"))

    (define (deposit amount)
      (set! balance (+ balance amount))
      balance)

    (define (dispatch password-in m)
      (if (eq? password-in password)
        (begin
          (set! tries-left 7)
          (cond ((eq? m 'withdraw) withdraw)
                ((eq? m 'deposit) deposit)
                (else
                  (error "Unknown request: MAKE-ACCOUNT"
                         m))))
        (if (< tries-left 1)
          (call-the-cops)
          (begin (set! tries-left (- tries-left 1))
                 "Incorrect password"))))

    dispatch))

(define acc  (make-account 100 'secret-password))
((acc 'secret-password 'deposit) 40) ; 140
(acc 'some-other-password 'withdraw) ; "Incorrect password"
(acc 'some-other-password 'withdraw) ; "Incorrect password"
(acc 'some-other-password 'withdraw) ; "Incorrect password"
(acc 'some-other-password 'withdraw) ; "Incorrect password"
(acc 'some-other-password 'withdraw) ; "Incorrect password"
(acc 'some-other-password 'withdraw) ; "Incorrect password"
(acc 'some-other-password 'withdraw) ; "Incorrect password"
(acc 'some-other-password 'withdraw) ; "NeeNooNeeNooNeeNoo"

; 3.5

(define (monte-carlo trials experiment)
  (define (iter trials-remaining trials-passed)
    (cond ((= trials-remaining 0)
           (/ trials-passed trials))
          ((experiment)
           (iter (- trials-remaining 1)
                 (+ trials-passed 1)))
          (else
           (iter (- trials-remaining 1)
                 trials-passed))))
  (iter trials 0))

(define (random-in-range low high)
  (let ((range (- high low)))
    (+ low (random range))))

(define (estimate-integral p x1 x2 y1 y2 trials)
  (let ((rect-area (* (- x2 x1) (- y2 y1))))
    (* (monte-carlo
         trials
         (lambda () (p (random-in-range x1 x2)
                       (random-in-range y1 y2))))
       rect-area)))

(/ (estimate-integral
     (lambda (x y)
       (<= (+ (square (- x 5))
              (square (- y 7)))
           (square 3)))
     2.0 8.0 4.0 10.0
     10000)
   9) ; 3.1248

; 3.6

(define (rand-update x)
   (let ((a 27) (b 26) (m 127))
     (modulo (+ (* a x) b) m)))

(define (resettable-rand)
  (let ((prev 1))
    (define (dispatch m)
      (cond ((eq? m 'generate)
             (begin (set! prev (rand-update prev))
                    prev))
            ((eq? m 'reset)
             (lambda (x) (set! prev x)))
            (else (error "unknown method"))))
    dispatch))

(define rand (resettable-rand))
(rand 'generate) ; 53
(rand 'generate) ; 60
((rand 'reset) 1)
(rand 'generate) ; 53
(rand 'generate) ; 60

; 3.7

(define (make-joint acc pass new-pass)
  (lambda (pass-in m)
    (if (eq? pass-in new-pass)
      (acc pass m))))

(define peter-acc (make-account 100 'open-sesame))
(define paul-acc
  (make-joint peter-acc
              'open-sesame
              'rosebud))

((peter-acc 'open-sesame 'withdraw) 10) ; 90
((paul-acc 'rosebud 'deposit) 20) ; 110

; 3.8

(define (make-mul)
  (let ((i 1))
    (lambda (j) (begin (set! i (* i j))
                       i))))

(define f (make-mul))
(+ (f 0) (f 1)) ; 1
(define f (make-mul))
(+ (f 1) (f 0)) ; 0
