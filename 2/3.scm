; 2.53

(list 'a 'b 'c) ; (a b c)
(list (list 'george)) ; ((george))
(cdr '((x1 x2) (y1 y2))) ; ((y1 y2))
(cadr '((x1 x2) (y1 y2))) ; (y1 y2)
(pair? (car '(a short list))) ; #f
(memq 'red '((red shoes) (blue socks))) ; #f
(memq 'red '(red shoes blue socks)) ; (red shoes blue socks)

; 2.54

(define (equal? sym1 sym2)
  (if (and (pair? sym1) (pair? sym2))
    (if (eq? (car sym1) (car sym2))
      (equal? (cdr sym1) (cdr sym2))
      #f)
    (eq? sym1 sym2)))

(equal? '(this is a list)
        '(this is a list)) ; #t

(equal? '(this is a list)
        '(this (is a) list)) ; #f

; 2.55

(car ''abracadabra) ; quote

; quoting once, returns a symbol. quoting again gives
; (quote <symbol>), which is a list, the car of which is
; quote

; 2.56

(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp)
         (if (same-variable? exp var) 1 0))
        ((sum? exp)
         (make-sum (deriv (addend exp) var)
                   (deriv (augend exp) var)))
        ((product? exp)
         (make-sum
           (make-product
             (multiplier exp)
             (deriv (multiplicand exp) var))
           (make-product
             (deriv (multiplier exp) var)
             (multiplicand exp))))
        ((exponentiation? exp)
         (make-product
           (exponent exp)
           (make-product
             (make-exponentiation
               (base exp)
               (- (exponent exp) 1))
             (deriv (base exp) var))))
        (else (error "unknown expression
                     type: DERIV" exp))))

(define (variable? x) (symbol? x))

(define (same-variable? v1 v2)
  (and (variable? v1)
       (variable? v2)
       (eq? v1 v2)))

;(define (make-sum a1 a2) (list '+ a1 a2))
;(define (make-product m1 m2) (list '* m1 m2))

(define (sum? x)
  (and (pair? x) (eq? (car x) '+)))

(define (addend s) (cadr s))

(define (augend s) (caddr s))

(define (product? x)
  (and (pair? x) (eq? (car x) '*)))

(define (multiplier p) (cadr p))

(define (multiplicand p) (caddr p))

(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2))
         (+ a1 a2))
        (else (list '+ a1 a2))))

(define (=number? exp num)
  (and (number? exp) (= exp num)))

(define (make-product m1 m2)
  (cond ((or (=number? m1 0)
             (=number? m2 0))
         0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2))
         (* m1 m2))
        (else (list '* m1 m2))))

(deriv '(+ x 3) 'x) ; 1
(deriv '(* x y) 'x) ; y
(deriv '(* (* x y) (+ x 3)) 'x) ; (+ (* x y) (* y (+ x 3)))

(define (exponentiation? x)
  (and (pair? x) (eq? (car x) '**)))
(define (base e) (cadr e))
(define (exponent e) (caddr e))
(define (make-exponentiation base exponent)
  (cond ((=number? exponent 0) 1)
        ((=number? exponent 1) base)
        (else (list '** base exponent))))

(deriv '(** x 2) 'x) ; (* 2 x)

; 2.57

(define (augend s)
  (let ((rest (cddr s)))
    (if (= (length rest) 1)
      (car rest)
      (cons '+ rest))))

(define (multiplicand p) ;(caddr p))
  (let ((rest (cddr p)))
    (if (= (length rest) 1)
      (car rest)
      (cons '* rest))))

(deriv '(* x y (+ x 3)) 'x) ; (+ (* x y) (* y (+ x 3)))

; 2.58

(define (sum? x)
  (and (pair? x) (eq? (cadr x) '+)))

(define (addend s) (car s))

(define (augend s) (caddr s))

(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2))
         (+ a1 a2))
        (else (list a1 '+ a2))))

(define (product? x)
  (and (pair? x) (eq? (cadr x) '*)))

(define (multiplier p) (car p))

(define (multiplicand p) (caddr p))

(define (make-product m1 m2)
  (cond ((or (=number? m1 0)
             (=number? m2 0))
         0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2))
         (* m1 m2))
        (else (list m1 '* m2))))

(deriv '(x + 3) 'x) ; 1
(deriv '(x * y) 'x) ; y
(deriv '(x * (y * (x + 3))) 'x) ; ((x * y) + (y * (x + 3)))
(deriv '(x + (3 * (x + (y + 2)))) 'x) ; 4

(define (index l elem)
  (define (iter i l)
    (if (eq? (car l) elem)
      i
      (iter (+ i 1) (cdr l))))
  (iter 0 l))

(define (simplify-outer-braces s)
  (if (= (length s) 1)
    (car s)
    s))

(define (sum? x)
  (and (pair? x)
       (there-exists?
         x
         (lambda (elem)
           (eq? elem '+)))))

(define (addend s)
  (let ((i (index s '+)))
    (simplify-outer-braces
      (sublist s 0 i))))

(define (augend s)
  (let ((i (index s '+)))
    (simplify-outer-braces
      (sublist s
               (+ i 1)
               (length s)))))

(define (product? x)
  (and (pair? x)
       (not (sum? x))
       (there-exists?
         x
         (lambda (elem)
           (eq? elem '*)))))

(define (multiplier p)
  (let ((i (index p '*)))
    (simplify-outer-braces
      (sublist p 0 i))))

(define (multiplicand p)
  (let ((i (index p '*)))
    (simplify-outer-braces
      (sublist p
               (+ i 1)
               (length p)))))

(deriv '(x + 3 * (x + y + 2)) 'x) ; 4

; 2.59

(define (element-of-set-ul? x set)
  (cond ((null? set) false)
        ((equal? x (car set)) true)
        (else (element-of-set-ul? x (cdr set)))))

(define (intersection-set-ul set1 set2)
  (cond ((or (null? set1) (null? set2))
         '())
        ((element-of-set-ul? (car set1) set2)
         (cons (car set1)
               (intersection-set-ul (cdr set1)
                                    set2)))
        (else (intersection-set-ul (cdr set1)
                                   set2))))

(define (union-set-ul set1 set2)
  (cond ((null? set1) set2)
        ((element-of-set-ul? (car set1) set2)
         (union-set-ul (cdr set1) set2))
        (else
          (cons (car set1)
                (union-set-ul (cdr set1) set2)))))

(intersection-set-ul '(1 2 3) '(2 3 4)) ; (2 3)
(union-set-ul '(1 2 3) '(2 3 4)) ; (1 2 3 4)

; 2.60

(define element-of-set-ul-dup? element-of-set-ul?)

(define adjoin-set-ul-dup cons)

(define intersection-set-ul-dup intersection-set-ul)

(define union-set-ul-dup append)

(intersection-set-ul-dup '(1 2 3) '(2 3 4)) ; (2 3)
(union-set-ul-dup '(1 2 3) '(2 3 4)) ; (1 2 3 2 3 4)

; adjoin and union are faster, element-of and
; intersection are slower. Any application that is
; write-heavy and read-light

; 2.61

(define (adjoin-set-ol x set)
  (let ((head (car set)))
    (cond ((= x head) set)
          ((< x head) (cons x set))
          (else (cons head
                      (adjoin-set-ol x (cdr set)))))))
; this only requires walking as far through the set as
; the entry fits (on average half it's length), while
; use of element-of-set-ul requires walking it all.

(adjoin-set-ol 4 '(1 3 6 10)) ; (1 3 4 6 10)

; 2.62

(define (union-set-ol set1 set2)
  (cond ((null? set1) set2)
        ((null? set2) set1)
        (else
          (let ((x1 (car set1))
                (x2 (car set2)))
            (cond ((= x1 x2)
                   (cons x1 (union-set-ol (cdr set1)
                                          (cdr set2))))
                  ((< x1 x2)
                   (cons x1 (union-set-ol (cdr set1)
                                          set2)))
                  ((< x2 x1)
                   (cons x2 (union-set-ol set1
                                          (cdr set2)))))))))

(union-set-ol '(1 3 5) '(2 4 6 8)) ; (1 2 3 4 5 6 8)

; 2.63

; 1: same. both are in-order traversal. they produce
; ordered lists

; 2: both O(n)

; 2.64

; 1: It selects the middle element of the list (rounding
; down) to be the entry of a tree, and recurses on the
; lists of elements before and after to form the left
; and right branches.

;    5
;  /   \
; 1     9
;  \   / \
;  3  7   11

; 2: O(n)

; 2.65

(define (entry tree) (car tree))
(define (left-branch tree) (cadr tree))
(define (right-branch tree) (caddr tree))
(define (make-tree entry left right)
  (list entry left right))

(define (tree->list tree)
  (if (null? tree)
      '()
      (append
       (tree->list-1
        (left-branch tree))
       (cons (entry tree)
             (tree->list-1
              (right-branch tree))))))

(define (list->tree elements)
  (define (partial-tree elts n)
    (if (= n 0)
      (cons '() elts)
      (let ((left-size
              (quotient (- n 1) 2)))
        (let ((left-result
                (partial-tree
                  elts left-size)))
          (let ((left-tree
                  (car left-result))
                (non-left-elts
                  (cdr left-result))
                (right-size
                  (- n (+ left-size 1))))
            (let ((this-entry
                    (car non-left-elts))
                  (right-result
                    (partial-tree
                      (cdr non-left-elts)
                      right-size)))
              (let ((right-tree
                      (car right-result))
                    (remaining-elts
                      (cdr right-result)))
                (cons (make-tree this-entry
                                 left-tree
                                 right-tree)
                      remaining-elts))))))))
  (car (partial-tree
         elements (length elements))))


(define (element-of-set-tree? x set)
  (cond ((null? set) false)
        ((= x (entry set)) true)
        ((< x (entry set))
         (element-of-set-tree?
           x
           (left-branch set)))
        ((> x (entry set))
         (element-of-set-tree?
           x
           (right-branch set)))))

(define (adjoin-set-tree x set)
  (cond ((null? set) (make-tree x '() '()))
        ((= x (entry set)) set)
        ((< x (entry set))
         (make-tree
           (entry set)
           (adjoin-set-tree x (left-branch set))
           (right-branch set)))
        ((> x (entry set))
         (make-tree
           (entry set)
           (left-branch set)
           (adjoin-set-tree x (right-branch set))))))

(define (union-set-tree set1 set2)
  (fold-right adjoin-set-tree set2 (tree->list set1)))

(define (intersection-set-tree set1 set2)
  (define (in-set-2? elem)
    (element-of-set-tree? elem set2))
  (list->tree (filter in-set-2? (tree->list set1))))

(tree->list
  (union-set-tree
    (list->tree '(1 2 3))
    (list->tree '(2 3 4)))) ; (1 2 3 4)

(tree->list
  (intersection-set-tree
    (list->tree '(1 2 3))
    (list->tree '(2 3 4)))) ; (2 3)

; 2.66

(define (lookup given-key set-of-records)
  (cond ((null? set-of-records) false)
        ((= given-key (key (entry set-of-records)))
         (entry set-of-records))
        ((< given-key (key (entry set-of-records)))
         (lookup
           given-key
           (left-branch set-of-records)))
        ((> given-key (key (entry set-of-records)))
         (lookup
           given-key
           (right-branch set-of-records)))))

; 2.67

(define (make-leaf symbol weight)
  (list 'leaf symbol weight))
(define (leaf? object)
  (eq? (car object) 'leaf))
(define (symbol-leaf x) (cadr x))
(define (weight-leaf x) (caddr x))

(define (make-code-tree left right)
  (list left
        right
        (append (symbols left)
                (symbols right))
        (+ (weight left) (weight right))))

(define (left-branch tree) (car tree))
(define (right-branch tree) (cadr tree))

(define (symbols tree)
  (if (leaf? tree)
      (list (symbol-leaf tree))
      (caddr tree)))

(define (weight tree)
  (if (leaf? tree)
      (weight-leaf tree)
      (cadddr tree)))

(define (decode bits tree)
  (define (choose-branch bit branch)
    (cond ((= bit 0) (left-branch branch))
          ((= bit 1) (right-branch branch))
          (else (error "bad bit:
                       CHOOSE-BRANCH" bit))))
  (define (decode-1 bits current-branch)
    (if (null? bits)
      '()
      (let ((next-branch
              (choose-branch
                (car bits)
                current-branch)))
        (if (leaf? next-branch)
          (cons
            (symbol-leaf next-branch)
            (decode-1 (cdr bits) tree))
          (decode-1 (cdr bits)
                    next-branch)))))
  (decode-1 bits tree))

(define sample-tree
  (make-code-tree
   (make-leaf 'a 4)
   (make-code-tree
    (make-leaf 'b 2)
    (make-code-tree
     (make-leaf 'd 1)
     (make-leaf 'c 1)))))

(define sample-message
  '(0 1 1 0 0 1 0 1 0 1 1 1 0))

(decode sample-message sample-tree) ; (a d a b b c a)

; 2.68

(define (encode-symbol symbol tree)
  (define (contains symbol l)
    (cond ((null? l) #f)
          ((eq? (car l) symbol) #t)
          (else (contains symbol (cdr l)))))
  (define (iter bits subtree)
    (if (leaf? subtree)
      (if (eq? symbol (symbol-leaf subtree))
        bits
        (error "symbol not found" symbol))
      (let ((left-symbols
              (symbols (left-branch subtree)))
            (right-symbols
              (symbols (right-branch subtree))))
        (cond ((contains symbol left-symbols)
               (iter (append bits '(0))
                     (left-branch subtree)))
              ((contains symbol right-symbols)
               (iter (append bits '(1))
                     (right-branch subtree)))
              (else
                (error "symbol not found" symbol))))))
  (iter '() tree))

(define (encode message tree)
  (if (null? message)
    '()
    (append
      (encode-symbol (car message)
                     tree)
      (encode (cdr message) tree))))

(encode '(a d a b b c a) sample-tree)
; (0 1 1 0 0 1 0 1 0 1 1 1 0)

; 2.69

(define (adjoin-set x set)
  (cond ((null? set) (list x))
        ((< (weight x) (weight (car set)))
         (cons x set))
        (else
          (cons (car set)
                (adjoin-set x (cdr set))))))

(define (make-leaf-set pairs)
  (if (null? pairs)
    '()
    (let ((pair (car pairs)))
      (adjoin-set
        (make-leaf (car pair)    ; symbol
                   (cadr pair))  ; frequency
        (make-leaf-set (cdr pairs))))))

(define (successive-merge set)
  (if (< (length set) 2)
    (car set)
    (let ((first (car set))
          (second (cadr set))
          (rest (cddr set)))
      (successive-merge
        (adjoin-set (make-code-tree first second)
                    rest)))))

(define (generate-huffman-tree pairs)
  (successive-merge
    (make-leaf-set pairs)))

(generate-huffman-tree '((a 4) (c 1) (b 2) (d 1)))
; ((leaf a 4) ((leaf b 2) ((leaf d 1) (leaf c 1) (d c) 2) (b d c) 4) (a b d c) 8)

; 2.70

(define rock-tree (generate-huffman-tree '((a 2) (boom 1) (get 2) (job 2) (na 16) (sha 3) (yip 9) (wah 1))))
(define get-a-job '(get a job sha na na na na na na na na get a job sha na na na na na na na na wah yip yip yip yip yip yip yip yip yip sha boom))

(length (encode get-a-job rock-tree)) ; 84

(* 3 (length get-a-job)) ; 108

; 2.71

;  (a b c d e)
;  /      \
; a    (b c d e)
;       /    \
;      b   (c d e)
;           /   \
;          c   (d e)
;               / \
;              d   e

; not going to bother with n=10, its the same pattern.
; most frequent: 1
; least-frequent: n - 1

; 2.72

; most-frequent: O(1)
; least-frequent: O(n^2)
