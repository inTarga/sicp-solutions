; 2.77

; we already defined these functions to be generic over
; rectangular and polar representations. With this
; addition the generic application sees the 'complex tag,
; strips it, passes the context to the inner function,
; which sees the 'rectangular or 'polar tag, strips that,
; and runs the specific implementation for the relevant
; representation.

; 2.78

(define (attach-tag type-tag contents)
  (if (eq? type-tag 'scheme-number)
    contents
    (cons type-tag contents)))

(define (type-tag datum)
  (if (pair? datum)
      (car datum)
      (if (number? datum)
        'scheme-number
        (error "Bad tagged datum:
              TYPE-TAG" datum))))

(define (contents datum)
  (if (pair? datum)
      (cdr datum)
      (if (number? datum)
        datum
        (error "Bad tagged datum:
              CONTENTS" datum))))

; 2.79

(define (equ? x y)
  (apply-generic 'equ? x y))

(define (install-generic-arithmetic-package)
  (define (numer x) (car x))
  (define (denom x) (cdr x))

  (put 'equ? ('scheme-number 'scheme-number)
       (lambda (x y) (= x y)))
  (put 'equ? ('rational 'rational)
       (lambda (x y) (and (= (numer x) (numer y))
                          (= (denom x) (denom y)))))
  (put 'equ? ('complex 'complex)
       (lambda (x y)
         (and (= (real-part x) (real-part y))
              (= (imag-part x) (imag-part y)))))

; 2.80

  (put '=zero? ('scheme-number)
       (lambda (x) (= 0 x)))
  (put '=zero? ('rational)
       (lambda (x) (and (= 0 (numer x))
                        (= 0 (denom x)))))
  (put '=zero? ('complex)
       (lambda (x) (and (= 0 (real-part x))
                        (= 0 (imag-part x)))))

  'done)

(define (=zero? x)
  (apply-generic '=zero? x))

; 2.81

; 1: we get stuck in an infinite loop

; 2: no, it works correctly as-is

; 3:

(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
        (apply proc (map contents args))
        (if (= (length args) 2)
          (let ((type1 (car type-tags))
                (type2 (cadr type-tags))
                (a1 (car args))
                (a2 (cadr args)))
            (if (eq? type1 type2)
              (error "No method for these types")
              (let ((t1->t2
                      (get-coercion type1
                                    type2))
                    (t2->t1
                      (get-coercion type2
                                    type1)))
                (cond (t1->t2
                        (apply-generic
                          op (t1->t2 a1) a2))
                      (t2->t1
                        (apply-generic
                          op a1 (t2->t1 a2)))
                      (else
                        (error
                          "No method for
                          these types"
                          (list
                            op
                            type-tags)))))))
            (error
              "No method for these types"
              (list op type-tags)))))))

; 2.82

(define (zip-with op l1 l2)
  (if (null? l1)
    '()
    (cons (op (car l1) (car l2))
          (zip-with op (cdr l1) (cdr l2)))))

(define (apply-generic op . args)
  (define (coerce-if-dif t1 t2)
    (if (eq? t1 t2)
      identity
      (get-coercion t1 t2)))
  (define (iter rem)
    (if (null? rem)
      #f
      (let ((to (car rem)))
        (if (for-all? args
                      (lambda (arg)
                        (coerce-if-dif arg to)))
          (map (lambda (arg) (coerce-if-dif arg to))
               args)
          (iter (cdr rem))))))
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
        (apply proc (map contents args))
        (let ((coercions (iter type-tags)))
          (if (coercions)
            (apply-generic
              op
              (zip-with
                (lambda (coercion type)
                  (coercion arg))
                coercions
                args))))))))

; 2.83

(define (raise x)
  (apply-generic 'raise x))

(define (install-tower-package)
  (put 'raise ('integer)
       (lambda (x) (make-rational x 1)))
  (put 'raise ('rational)
       (lambda (x) (make-real (/ (numer x) (denom x)))))
  (put 'raise ('real)
       (lambda (x) (make-complex-real-imag x 0)))
  'done)

; 2.84

(define (is-lower? x y)
  (let ((raise (get 'raise (type-tag x))))
    (if (raise)
      (let ((next (raise x)))
        (if (eq? (type-tag next) (type-tag y))
          #t
          (is-lower? next y)))
      #f)))

(define (raise-to x y)
  (let ((raise (get 'raise (type-tag x))))
    (if (raise)
      (let ((next (raise x)))
        (if (eq? (type-tag next) (type-tag y))
          next
          (raise-to next y)))
      (error "ran out of raises"))))

(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
        (apply proc (map contents args))
        (if (= (length args) 2)
          (let ((type1 (car type-tags))
                (type2 (cadr type-tags))
                (a1 (car args))
                (a2 (cadr args)))
            (if (eq? type1 type2)
              (error "No method for these types")
              (if (is-lower? a1 a2)
                (apply-generic op (raise-to a1 a2) a2)
                (apply-generic op a1 (raise-to a2 a1)))))
            (error
              "No method for these types"
              (list op type-tags)))))))

; 2.85

(define (project x)
  (apply-generic 'project x))

(define (install-tower-project-package)
  (put 'project ('complex)
       (lambda (x) (make-real (real-part x))))
  (put 'project ('real)
       (lambda (x) (make-rational (inexact->exact x) 1)))
  (put 'project ('rational)
       (lambda (x) (make-integer (round (/ (numer x) (denom x))))))
  'done)

(define (drop x)
  (let ((project (get 'project (type-tag x))))
    (if (project)
      (let ((projection (project x))
            (raise (get 'raise (type-tag projection))))
        (if (equ? x (raise projection))
          (let ((next (drop projection)))
            (if (next)
              next
              projection)))))))

(define (apply-generic op . args)
  (drop
    (let ((type-tags (map type-tag args)))
      (let ((proc (get op type-tags)))
        (if proc
          (apply proc (map contents args))
          (if (= (length args) 2)
            (let ((type1 (car type-tags))
                  (type2 (cadr type-tags))
                  (a1 (car args))
                  (a2 (cadr args)))
              (if (eq? type1 type2)
                (error "No method for these types")
                (if (is-lower? a1 a2)
                  (apply-generic op (raise-to a1 a2) a2)
                  (apply-generic op a1 (raise-to a2 a1)))))
            (error
              "No method for these types"
              (list op type-tags))))))))

; 2.86

; going to skip actually implementing this one, since I
; have a grasp on how to do it, but it's just messy and
; time consuming to implement, and I can't even test it.
; Essentially though, we just need switch to using
; generic operations in all functions in the complex
; package.

; skipping the rest of the exercises in this section.
; they feel like pointless busywork, made all the more
; pointless by being impossible to test.
