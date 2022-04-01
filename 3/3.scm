; 3.12

; (b)

; (b c d)

; append doesn't affect x, because it contructs a new list and returns it as z
; append! does though, because it modifies x then returns x, so w == x

; 3.13

; infinite loop, every time we reach c, we go back to a and start again, never
; finding a null

; 3.14

; mystery reverses a list

; v: (a)
; w: (d c b a)

(define (mystery x)
  (define (loop x y)
    (if (null? x)
        y
        (let ((temp (cdr x)))
          (set-cdr! x y)
          (loop temp x))))
  (loop x '()))

(define v (list 'a 'b 'c 'd))

(define w (mystery v))

v ; -> (a)
w ; -> (d c b a)

; 3.15

; no draw diagram

; 3.16

; Ben's procedure assume each pair has exactly one reference

; to return 3, refer to each pair once
; use multiple references to get to 4 or 7
; make a reference cycle to never return

; 3.17

(define (count-pairs-wrong x)
  (if (not (pair? x))
      0
      (+ (count-pairs-wrong (car x))
         (count-pairs-wrong (cdr x))
         1)))

(define (contains? l item)
  (if (null? l)
    #f
    (if (eq? (car l) item)
      #t
      (contains? (cdr l) item))))
(define (smart-insert l item)
  (if (contains? l item)
    l
    (cons item l)))

(define (count-pairs x)
  (define (iter found x)
    (if (or (not (pair? x)) (contains? found x))
      found
      (iter (iter (smart-insert found x) (car x)) (cdr x))))
  (define (count l)
    (if (null? l)
      0
      (+ 1 (count (cdr l)))))
  (count (iter '() x)))

(define pair1 (cons 'a 'b))
(define pair2 (cons 'c 'd))
(define pair3 (cons pair1 pair2))

(count-pairs-wrong pair3) ; -> 3
(count-pairs pair3) ; -> 3

(define pair4 (cons 'e 'f))
(define pair5 (cons 'g pair4))
(define pair6 (cons pair5 pair4))

(count-pairs-wrong pair6) ; -> 4
(count-pairs pair6) ; -> 3

(define pair7 (cons 'h 'i))
(define pair8 (cons pair7 pair7))
(define pair9 (cons pair8 pair8))

(count-pairs-wrong pair9) ; -> 7
(count-pairs pair9) ; -> 3

(define pair10 (cons 'j 'k))
(define pair11 (cons 'l 'm))
(define pair12 (cons 'n pair10))
(set-cdr! pair10 pair11)

(count-pairs-wrong pair12) ; -> ABORT
(count-pairs pair12) ; -> 3

; 3.18 & 3.19

(define (cycle? x)
  (define (iter curr)
    (cond ((null? curr) #f)
          ((eq? x curr) #t)
          (else (iter (cdr curr)))))
  (iter (cdr x)))

(define (make-cycle x)
  (set-cdr! (last-pair x) x)
  x)

(cycle? (list 'a 'b 'c)) ; -> #f
(cycle? (make-cycle(list 'a 'b 'c))) ; -> #t

; 3.20

; no diagram, only code

; 3.21

(define (front-ptr queue) (car queue))
(define (rear-ptr queue) (cdr queue))
(define (set-front-ptr! queue item)
  (set-car! queue item))
(define (set-rear-ptr! queue item)
  (set-cdr! queue item))

(define (empty-queue? queue)
  (null? (front-ptr queue)))

(define (make-queue) (cons '() '()))

(define (front-queue queue)
  (if (empty-queue? queue)
      (error "FRONT called with an
              empty queue" queue)
      (car (front-ptr queue))))

(define (insert-queue! queue item)
  (let ((new-pair (cons item '())))
    (cond ((empty-queue? queue)
           (set-front-ptr! queue new-pair)
           (set-rear-ptr! queue new-pair)
           queue)
          (else (set-cdr! (rear-ptr queue)
                          new-pair)
                (set-rear-ptr! queue new-pair)
                queue))))

(define (delete-queue! queue)
  (cond ((empty-queue? queue)
         (error "DELETE! called with
                 an empty queue" queue))
        (else (set-front-ptr!
               queue
               (cdr (front-ptr queue)))
              queue)))

(define q1 (make-queue))
q1 ; -> (())
(insert-queue! q1 'a) ; -> ((a) a)
(insert-queue! q1 'b) ; -> ((a b) b)
(delete-queue! q1) ; -> ((b) b)
(delete-queue! q1) ; -> (() b)

; what is printed here is a pair of the front and rear pointers. since these
; are each lists that terminate in nil, what ends up being printed is the rear
; pointer list, with the front pointer list appended to its front.

; I believe the front pointer list alone should be a correct representation of the queue

(define (format-queue q) (car q))

(define q1 (make-queue))
(format-queue q1) ; -> ()
(format-queue (insert-queue! q1 'a)) ; -> (a)
(format-queue (insert-queue! q1 'b)) ; -> (a b)
(format-queue (delete-queue! q1)) ; -> (b)
(format-queue (delete-queue! q1)) ; -> ()

; 3.22

(define (make-queue-proc)
  (let ((front-ptr '())
        (rear-ptr '()))
    (define (empty-queue?)
      (null? front-ptr))
    (define (front-queue)
      (if (empty-queue?)
        (error "FRONT called with an empty queue")
        (car front-ptr)))
    (define (format-queue)
      front-ptr)
    (define (insert-queue! item)
      (let ((new-pair (cons item '())))
        (cond ((empty-queue?) (set! front-ptr new-pair)
                              (set! rear-ptr new-pair))
              (else (set-cdr! rear-ptr new-pair)
                    (set! rear-ptr new-pair)))))
    (define (delete-queue!)
      (cond ((empty-queue?) (error "DELETE! called with an empty queue"))
            (else (set! front-ptr (cdr front-ptr)))))
    (define (dispatch m)
      (cond ((eq? m 'empty-queue?) (empty-queue?))
            ((eq? m 'front-queue) (front-queue))
            ((eq? m 'format-queue) (format-queue))
            ((eq? m 'insert-queue!) insert-queue!)
            ((eq? m 'delete-queue!) (delete-queue!))))
    dispatch))

(define q1 (make-queue-proc))
(q1 'format-queue) ; -> ()

((q1 'insert-queue!) 'a)
(q1 'format-queue) ; -> (a)

((q1 'insert-queue!) 'b)
(q1 'format-queue) ; -> (a b)

(q1 'delete-queue!)
(q1 'format-queue) ; -> (b)

(q1 'delete-queue!)
(q1 'format-queue) ; -> ()

; 3.23

(define (make-node content prev next)
  (cons content (cons prev next)))
(define (node-content node)
  (car node))
(define (node-prev node)
  (car (cdr node)))
(define (node-next node)
  (cdr (cdr node)))
(define (set-node-prev! node new-prev)
  (set-car! (cdr node) new-prev))
(define (set-node-next! node new-next)
  (set-cdr! (cdr node) new-next))

(define (make-deque) (cons '() '()))
(define (empty-deque? dq)
  (null? (front-ptr dq)))
(define (front-deque dq)
  (car (front-ptr dq)))
(define (rear-deque dq)
  (car (rear-ptr dq)))
(define (front-insert-deque! dq item)
  (let ((new-node (make-node item '() (front-ptr dq))))
    (if (empty-deque? dq)
      (begin (set-front-ptr! dq new-node)
             (set-rear-ptr! dq new-node))
      (begin (set-node-prev! (front-ptr dq) new-node)
             (set-front-ptr! dq new-node)))))
(define (rear-insert-deque! dq item)
  (let ((new-node (make-node item (rear-ptr dq) '())))
    (if (empty-deque? dq)
      (begin (set-front-ptr! dq new-node)
             (set-rear-ptr! dq new-node))
      (begin (set-node-next! (rear-ptr dq) new-node)
             (set-rear-ptr! dq new-node)))))
(define (front-delete-deque! dq)
  (if (eq? (front-ptr dq) (rear-ptr dq))
    (begin (set-front-ptr! dq '())
           (set-rear-ptr! dq '()))
    (begin (set-front-ptr! dq (node-next (front-ptr dq)))
           (set-node-prev! (front-ptr dq) '()))))
(define (rear-delete-deque! dq)
  (if (eq? (front-ptr dq) (rear-ptr dq))
    (begin (set-front-ptr! dq '())
           (set-rear-ptr! dq '()))
    (begin (set-rear-ptr! dq (node-prev (rear-ptr dq)))
           (set-node-next! (rear-ptr dq) '()))))
(define (format-deque dq)
  (define (iter output curr-node)
    (if (null? curr-node)
      output
      (iter (cons (node-content curr-node) output)
            (node-prev curr-node))))
  (iter '() (rear-ptr dq)))

(define dq1 (make-deque))
(format-deque dq1) ; -> ()

(front-insert-deque! dq1 'a)
(format-deque dq1) ; -> (a)

(front-insert-deque! dq1 'b)
(format-deque dq1) ; -> (b a)

(rear-insert-deque! dq1 'c)
(format-deque dq1) ; -> (b a c)

(rear-insert-deque! dq1 'd)
(format-deque dq1) ; -> (b a c d)

(front-delete-deque! dq1)
(format-deque dq1) ; -> (a c d)

(front-delete-deque! dq1)
(format-deque dq1) ; -> (c d)

(rear-delete-deque! dq1)
(format-deque dq1) ; -> (c)

(rear-delete-deque! dq1)
(format-deque dq1) ; -> ()

; 3.24

(define (make-table-2d same-key?)
  (let ((local-table (list '*table*)))
    (define (assoc key records)
      (cond ((null? records) false)
            ((same-key? key (caar records))
             (car records))
            (else (assoc key (cdr records)))))
    (define (lookup key-1 key-2)
      (let ((subtable
              (assoc key-1 (cdr local-table))))
        (if subtable
          (let ((record
                  (assoc key-2
                         (cdr subtable))))
            (if record (cdr record) false))
          false)))
    (define (insert! key-1 key-2 value)
      (let ((subtable
              (assoc key-1 (cdr local-table))))
        (if subtable
          (let ((record
                  (assoc key-2
                         (cdr subtable))))
            (if record
              (set-cdr! record value)
              (set-cdr!
                subtable
                (cons (cons key-2 value)
                      (cdr subtable)))))
          (set-cdr!
            local-table
            (cons (list key-1
                        (cons key-2 value))
                  (cdr local-table)))))
      'ok)
    (define (dispatch m)
      (cond ((eq? m 'lookup-proc) lookup)
            ((eq? m 'insert-proc!) insert!)
            (else (error "Unknown operation:
                         TABLE" m))))
                        dispatch))

(define t2d (make-table-2d equal?))
((t2d 'insert-proc!) 'x 'y 12) ; ok
((t2d 'lookup-proc) 'x 'y) ; 12

; 3.25

(define (make-table-nd same-key?)
  (let ((local-table (list '*table*)))
    (define (assoc key records)
      (cond ((null? records) false)
            ((same-key? key (caar records))
             (car records))
            (else (assoc key (cdr records)))))
    (define (lookup-iter keys table)
      (if (null? keys)
        (cdr table)
        (lookup-iter
          (cdr keys)
          (assoc (car keys) (cdr table)))))
    (define (lookup keys)
      (lookup-iter keys local-table))
    (define (insert-iter! keys table value)
      (if (null? keys)
        (let ((record table))
          (set-cdr! record value)
          'ok)
        (let ((subtable (assoc (car keys) (cdr table))))
          (if subtable
            (insert-iter! (cdr keys) subtable value)
            (begin (set-cdr! table (cons (list (car keys))
                                  (cdr table)))
                   (insert-iter! (cdr keys)
                                 (assoc (car keys) (cdr table))
                                 value))))))
    (define (insert! keys value)
      (insert-iter! keys local-table value))
    (define (dispatch m)
      (cond ((eq? m 'lookup-proc) lookup)
            ((eq? m 'insert-proc!) insert!)
            (else (error "Unknown operation:
                         TABLE" m))))
                        dispatch))

(define tnd (make-table-nd equal?))
((tnd 'insert-proc!) (list 'x 'y) 12) ; ok
((tnd 'lookup-proc) (list 'x 'y)) ; 12

; 3.26

; should be the same, just with assoc and insert! modified to call tree-lookup
; and tree-insert instead of doing unordered list lookups and inserts

; 3.27

; it does it in O(n) because each of the n previous fibonacci numbers is only
; computed once. if the function tries to compute a fibonacci number more than
; once, the recursive call just returns the memoized value instead.

; defining as (memoize fib) doesn;t work, because the recursive calls will go
; to the un-memoized version.

; 3.28

(define (or-gate a1 a2 output)
  (define (or-action-procedure)
    (let ((new-value (logical-or (get-signal a1)
                                 (get-signal a2))))
      (after-delay
        or-gate-delay
        (lambda ()
          (set-signal! output new-value)))))
  (add-action! a1 or-action-procedure)
  (add-action a2 or-action-procedure)
  'ok)

; 3.29

(define (or-gate-composite in1 in2 out)
  (let ((inv1 (make-wire))
        (inv2 (make-wire))
        (ao1 (make-wire))
        (ao2 (make-wire))
        (li1 (make-wire))
        (li2 (make-wire)))
    (and-gate in1 in2 ao1)
    (inverter in1 inv1)
    (inverter in2 inv2)
    (and-gate inv1 inv2 ao2)
    (inverter ao1 li1)
    (inverter ao2 li2)
    (and-gate li1 li2 out)
    'ok))

; 2*and-gate-delay + 2*inverter-delay

; 3.30

(define (ripple-carry-adder ak bk sk c)
  (let ((ci (make-wire))
        (ai (car ak))
        (bi (car bk))
        (si (car sk)))
    (if (null? ai)
      (set-signal! c 0)
      (begin (full-adder ai bi ci si c)
             (ripple-carry-adder (cdr ak) (cdr bk) (cdr sk) ci)))))

; half-adder-s-delay = 2*and + 1*inverter
; half-adder-c-delay = 1*and

; full-adder-s-delay = 2*half-adder-s-delay
;                    = 4*and + 2*inverter

; full-adder-c-delay = half-adder-s-delay + half-adder-c-delay + or-delay
;                    = 2*and + 1*inverter + 1*and + 1*or
;                    = 3*and + 1*inv + 1*or

; ripple-carry-adder-s-delay = full-adder-s-delay + (n-1)*full-adder-c-delay
;                            = 4*and + 2*inv + (n-1)*(3*and + 1*inv + 1*or)
; OR                         = n*full-adder-c-delay
;                            = 3n*and + n*inv + n*or
; whichever is worse
