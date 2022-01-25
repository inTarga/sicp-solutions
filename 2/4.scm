; 2.73

; 1: replaced the cond branches for tagged expressions
; with data-directed dispatch via a table. We cand do
; this for number? and variable? because those aren't
; tagged

; 2 and 3:

(define (install-deriv-package)
  (define (=number? exp num)
    (and (number? exp) (= exp num)))
  (define (addend s) (cadr s))
  (define (augend s) (caddr s))
  (define (make-sum a1 a2)
    (cond ((=number? a1 0) a2)
          ((=number? a2 0) a1)
          ((and (number? a1) (number? a2))
           (+ a1 a2))
          (else (list '+ a1 a2))))
  (define (multiplier p) (cadr p))
  (define (multiplicand p) (caddr p))
  (define (make-product m1 m2)
    (cond ((or (=number? m1 0)
               (=number? m2 0))
           0)
          ((=number? m1 1) m2)
          ((=number? m2 1) m1)
          ((and (number? m1) (number? m2))
           (* m1 m2))
          (else (list '* m1 m2))))
  (define (base e) (cadr e))
  (define (exponent e) (caddr e))
  (define (make-exponentiation base exponent)
    (cond ((=number? exponent 0) 1)
          ((=number? exponent 1) base)
          (else (list '** base exponent))))
  (define (deriv-sum)
    (make-sum (deriv (addend exp) var)
              (deriv (augend exp) var)))
  (define (deriv-prod)
    (make-sum
           (make-product
            (multiplier exp)
            (deriv (multiplicand exp) var))
           (make-product
            (deriv (multiplier exp) var)
            (multiplicand exp))))
  (define (deriv-expt)
    (make-product
      (exponent exp)
      (make-product
        (make-exponentiation
          (base exp)
          (- (exponent exp) 1))
        (deriv (base exp) var))))

  (put 'deriv '(+) deriv-sum)
  (put 'deriv '(*) deriv-prod)
  (put 'deriv '(**) deriv-expt)
  'done)

; 4: nothing? I guess change the order in put too

; 2.74

; 1:

(define (get-record employee file)
  ((get 'get-record (car file))
   employee
   (cdr file)))

; They must provide a type tag as the first elem of the
; file, and put a corresponsing 'get-record entry for
; their type tag in the table.

; 2:

(define (get-salary record)
  ((get 'get-salary (car record)) (cdr record)))

; similarly to above

; 3:

(define (find-employee-record employee files)
  (if (null? files)
    #f
    (let ((record (get-record employee (car files))))
      (if (record)
        record
        (find-employee-record (cdr files))))))

; 4:

; make a new file with a tag, and put appropriate entries
; in the table for that tag.

; 2.75

(define (make-from-mag-ang r a)
  (define (dispatch op)
    (cond ((eq? op 'real-part) (* r (cos a)))
          ((eq? op 'imag-part) (* r (sin a)))
          ((eq? op 'magnitude) r)
          ((eq? op 'angle) a)
          (else (error "Unknown op:" op))
          ))
  dispatch)

; 2.76

; explicit - new ops, but not types without changing code
; data-directed - only adding entries in a table for both
; message passing - new types, but not ops without change

; explicit is better for new ops, message passing for new
; types, data-directed is ok for both.
