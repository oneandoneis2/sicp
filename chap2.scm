(define (add-rat x y)
  (make-rat (+ (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))

(define (sub-rat x y)
  (make-rat (- (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))

(define (mul-rat x y)
  (make-rat (* (numer x) (numer y))
            (* (denom x) (denom y))))

(define (div-rat x y)
  (make-rat (* (numer x) (denom y))
            (* (numer y) (denom x))))

(define (equal-rat? x y)
  (= (* (numer x) (denom y))
     (* (numer y) (denom x))))

; 2.1
(define (make-rat n d)
  (let ((g ((if (< d 0) - +) (abs (gcd n d)))))
    ; ^ Because both numbers get divided by gcd, a mechanism that
    ; sets gcd to -ve only if d is -ve will always DTRT:
    ; g will flip the sign of both if d is -ve
    (cons (/ n g) (/ d g))))

; Goes against the grain to not just do (define numer car)
; especially after reading the Little Schemer
; but I accept their argument of it being better for debugging
(define (numer x)
  (car x))

(define (denom x)
  (cdr x))

(define (print-rat x)
  (newline)
  (display (numer x))
  (display "/")
  (display (denom x))
  x) ; It's annoying to be undef and vaguely useful to see the real data

(define (gcd a b)
  (if (= b 0)
    a
    (gcd b (remainder a b))))

; 2.2
(define (make-segment a b)
  (cons a b))

(define (start-segment s)
  (car s))

(define (end-segment s)
  (cdr s))

(define (make-point x y)
  (cons x y))

(define (x-point p)
  (car p))

(define (y-point p)
  (cdr p))

(define (midpoint-segment s)
  (define (average f x y) (/ (+ (f x) (f y)) 2))
  (let ((start (start-segment s))
        (end (end-segment s)))
    (make-segment (average x-point start end)
                  (average y-point start end))))

(define (print-point p)
  (newline)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")"))

; 2.3
; Now, imp.1
(define (imp1 a b)
  ; First, the functions that must work for both implementations
  (define (perimiter r) (+ (* 2 (height r)) (* 2 (width r))))
  (define (area r) (* (height r) (width r)))
  ; Define a rectangle as two points: The bottom left, and the top right
  (define (make-rectangle bl tr)
    (cons bl tr))
  (define (bottom-left r)
    (car r))
  (define (top-right r)
    (cdr r))
  (define (point-diff f r)
    (- (f (top-right r)) (f (bottom-left r))))
  (define (height r)
    (point-diff y-point r))
  (define (width r)
    (point-diff x-point r))
  ; Right, that's everything defined. Try it out
  (let ((test (make-rectangle a b)))
    (newline)
    (display "Perimiter: ")
    (display (perimiter test))
    (display ", area: ")
    (display (area test))))

; Now, imp.2
(define (imp2 p w h)
  ; First, the functions that must work for both implementations
  (define (perimiter r) (+ (* 2 (height r)) (* 2 (width r))))
  (define (area r) (* (height r) (width r)))
  ; Define a rectangle as its bottom-left point & its dimensions
  (define (make-rectangle p w h)
    (cons p (cons w h)))
  (define (dimensions r)
    (cdr r))
  (define (height r)
    (cdr (dimensions r)))
  (define (width r)
    (car (dimensions r)))
  ; Right, that's everything defined. Try it out
  (let ((test (make-rectangle p w h)))
    (newline)
    (display "Perimiter: ")
    (display (perimiter test))
    (display ", area: ")
    (display (area test))))

; 2.4
(define (mycons x y)
  (lambda (m) (m x y)))
(define (mycar c)
  (c (lambda (x y) x)))
(define (mycdr c)
  (c (lambda (x y) y)))

; 2.5
; The two numbers are represented by a single number, the product of 2^car * 3^cons
; To retrieve car, we need to find a power of 2 that divides cons to give a power of 3
;   i.e. if cons/2^n = 3^something then car cons = n
; To retrive cdr, the same but swap the 2 and 3
(define (cons25 a b)
  (* (expt 2 a) (expt 3 b)))

(define (ispow_n? base n)
  (define (iter x)
    (let ((e (expt base x)))
      (cond ((= e n) #t)
            ((> e n) #f)
            (else (iter (+ 1 x))))))
  (iter 0))
(define (ispow2? n) (ispow_n? 2 n))
(define (ispow3? n) (ispow_n? 3 n))

(define (calc c num_wanted f_unwanted)
  (define (iter x)
    (let ((e (expt num_wanted x)))
      (if (f_unwanted (/ c e))
        x
        (iter (+ 1 x)))))
  (iter 0))
(define (car25 c) (calc c 2 ispow3?))
(define (cdr25 c) (calc c 3 ispow2?))

; 2.6
; Church numerals
; CN's basically deal with two arguments: a function and a value
; For the numeral n, the function is applied n times, so:
; 0 f x = x
; 1 f x = f(x)
; 2 f x = f( f(x) )
; etc.
(define zero (lambda (f) (lambda (x) x)))
(define (add-1 n) (lambda (f) (lambda (x) (f ((n f) x)))))
; So to define one and two directly, just sub the appropriate values
; one = (add-1 zero)
; one = (lambda (f) (lambda (x) (f ((zero f) x))))
; Zero ignores the f and just returns the identity of the x, so:
; one = (lambda (f) (lambda (x) (f x)))
; And so two is just:
; two = (add-1 one)
; two = (lambda (f) (lambda (x) (f ((one f) x))))
; one applies f to x once, thus:
; two = (lambda (f) (lambda (x) (f (f x))))
(define one (lambda (f) (lambda (x) (f x))))
(define two (lambda (f) (lambda (x) (f (f x)))))
; For simple testing purposes
(define (incf x) (+ 1 x))
; Aaand:
((one incf) 0)
;> 1
((two incf) 0)
;> 2
; CN's basically consider the numeral n to mean "apply the function f n times"
; So three doesn''t mean three, it means "do something three times"
; So a simple way of doing addition would be to make add-1 the function
; So we could define three as:
(define three ((one add-1) two))
((three incf) 0)
;> 3
; And that *does* work, and it's not defining addition in terms of (direct)
; repeated addition of 1 ; But it *does* still use add-1 repeatedly
; So it's not entirely kosher as a solution to the addition
; We're meant to be making a function that'll not need repeated iteration..
(define (cn-add i j)
  (lambda (f)
    (lambda (x)
      ((j f) ((i f) x)))))
(define five (cn-add two three))
((five incf) 0)
;> 5
; What we basically do is apply one CN to x, and then apply the other to its result
;
; As an aside, we can define an iszero? function simply and usefully:
; Just define a function that always returns false, and an argument of true
; Because zero ignores the function and returns the argument, it returns true
; Every other number applies the function, so will return false
(define (iszero? n)
  (define (return_f x) #f)
  ((n return_f) #t))

(define (add-interval x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
                 (+ (upper-bound x) (upper-bound y))))

; Was confused by a while why we needed to do all four multiplications
; but if you have -ve numbers, then all becomes clear!
(define (mul-interval x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
        (p2 (* (lower-bound x) (upper-bound y)))
        (p3 (* (upper-bound x) (lower-bound y)))
        (p4 (* (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))

; 2.7
(define (make-interval a b)
  (cons a b))

; The assumption throughout when making an interval has been that it's
; (lower . upper) but we may as well enforce it and not have to rely
; on ordering... TBH anything else makes the exercise seem pointless
(define (upper-bound i)
  (max (car i) (cdr i)))
(define (lower-bound i)
  (min (car i) (cdr i)))

; 2.8
; The basic logic here is [a,b]-[c,d] = [a-d,b-c] - so sayeth Wikipedia
(define (sub-interval a b)
  (make-interval (- (lower-bound a) (upper-bound b))
                 (- (upper-bound a) (lower-bound b))))

; 2.9 is just maths. No.
; 2.10
; Handle interval equivalent of divide-by-zero:
; Divinding by an interval of (1-,1) or (0,1) is not defined
(define (div-interval x y)
  (let ((up (upper-bound y))
        (lo (lower-bound y)))
    (if (>= 0 (* up lo))  ; A zero or -ve result must mean spanning zero
      (error "Division error: Interval spans zero")
      (mul-interval x
                    (make-interval (/ 1.0 up)
                                   (/ 1.0 lo))))))

; 2.11's rewrite is worse in every possible way. No.
; The nine cases are the 3x3 of both-positive, both-negative, and spans-zero

(define (make-center-width c w)
  (make-interval (- c w) (+ c w)))

(define (center i)
  (/ (+ (lower-bound i) (upper-bound i)) 2))

(define (width i)
  (/ (- (upper-bound i) (lower-bound i)) 2))
; 2.12
(define (make-center-percent c p)
  (let ((w (* (/ p 100) c)))
    (make-center-width c w))) ; Since we calc'd the width, might as well just recycle

(define (percent-interval i)
  ; Find %age using formula: 100 * (difference in values)/(sum of values)
  ; Since width and center are already defined, and both generate 1/2 the val we want
  ; we can just use them
  ( * 100 (/ (width i) (center i))))

; 2.13 -> 2.16 = pure maths. Not interested.
; Problem is that we don't handle "identity"
; e.g. A = (1 . 4), B = (1 . 4)
; Both A and B could be any number between 1 and 4
; A/B = (0.25, 4) - that's as precise as we can be
; A/A *should* = 1
;   (or (1 . 1) to keep it as an interval)
;   but instead will be considered equal to A/B
; So any time we use the same interval more than once, issues could arise
; Hence par2 is a better solution than par1. We'd need a reliable concept of identity
; to fix these issues. If we had one, then par1 and par2 would be equivalent.

; 2.17
(define nil (list)) ; Why fight it?
(define (last-pair l)
  (define (iter last lst)
    (if (null? lst)
      (cons last lst)
      (iter (car lst) (cdr lst))))
  (iter nil l))

; 2.18
(define (my-reverse l)
  (define (iter acc lst)
    (if (null? lst)
      acc
      (iter (cons (car lst) acc) (cdr lst))))
  (iter nil l))

; 2.19
; I admit, with all the buildup I thought this would be more complicated...
(define (first-denomination cv)
  (car cv))
(define (except-first-denomination cv)
  (cdr cv))
(define (no-more? cv)
  (null? cv))

; 2.20
(define (same-parity first . rest)
  (define (filtr lst f)
    (cond ((null? lst) nil)
          ((f (car lst)) (cons (car lst) (filtr (cdr lst) f)))
          (else (filtr (cdr lst) f))))
  (if (odd? first)
    (cons first (filtr (cdr rest) odd?))
    (cons first (filtr (cdr rest) even?))))

; Note. To define a lambda using dot notation, use:
(define return-tail (lambda (x . y) y))
; (return-tail 1 2 3 4 5)
;> (2 3 4 5)
(define return-all (lambda w w))
; (return-all 1 2 3 4 5)
;> (1 2 3 4 5)
; ^ Basically just defined cdr and list via lambda notation...

; 2.21
(define (square-list items)
  (if (null? items)
    nil
    (cons (square (car items))
          (square-list (cdr items)))))

(define (square-list items)
  (map square items))

; 2.22
; 1) Because you're consing answers onto the () at the end of the list,
; so as you go along the initial list, each new answer becomes the car.
; (This is how the reverse function I wrote earlier worked)
; 2) Wha? Who would think this will work? You're consing numbers as cdr onto
; an empty list as car - This is just wrong.

; 2.23
; Well.. it meets all the requirements..
(define (foreach f l)
  (map f l)
  #t)

(define (count-leaves x)
  (cond ((null? x) 0)
        ((not (pair? x)) 1)
        (else (+ (count-leaves (car x))
                 (count-leaves (cdr x))))))

; 2.24
(count-leaves (list 1 (list 2 (list 3 4))))
;> 4
;
; [1][ ]
;     |
;     v
;    [ ][nil]
;     |
;     v
;    [2][ ]
;        |
;        v
;       [ ][()]
;        |
;        v
;       [3][ ]
;           |
;           v
;          [4][nil]
;
; (1 (2 (3 4)))
;      ^
;     / \
;    1   (2 (3 4))
;            ^
;           / \
;          2   (3 4)
;                ^
;               / \
;              3   4

; 2.25
(car (cdr (car (cdr (cdr '(1 2 (' 5 7) 9))))))
(car (car '((7))))
(car (cdr (car (cdr (car (cdr (car (cdr (car (cdr (car (cdr '(1 (2 (3 (4 (5 (6 7))))))))))))))))))
; ^ ouch!

; 2.26
(define x (list 1 2 3))
(define y (list 4 5 6))
(append x y)
;> (1 2 3 4 5 6)
(cons x y)
;> ((1 2 3) 4 5 6)
(list x y)
;> ((1 2 3) (4 5 6))

; 2.27
(define (deep-reverse l)
  (define (iter acc lst)
    (cond ((null? lst) acc)
          ((pair? (car lst)) (iter (cons (iter nil (car lst)) acc) (cdr lst)))
          (else (iter (cons (car lst) acc) (cdr lst)))))
  (iter nil l))
; ^ To my genuine amazement, that worked on my first try. No errors or re-jigging!
; What is SICP doing to my brain? O_o

; 2.28
(define (fringe l)
  (define (iter acc lst)
    (if (null? lst)
      acc
      (let ((head (car lst)) (tail (cdr lst)))
        (if (pair? head)
          (iter (iter acc head) tail)
          (iter (cons head acc) tail)))))
  (reverse (iter nil l)))
; This would have taken two tries, but took a dozen because of one silly mistake
; Note to self: Make sure you return the accumulator, not nil, when the list is empty
; Sigh.
; Also, it would be a lot easier if append were available, but I don't think we're
; meant to know about it yet..?

(define (make-mobile left right)
  (cons left right))
(define (make-branch long struct)
  (cons long struct))

; 2.29
; Make life easy by defining a nested mobile
;            my-mob
;          /        \
;        --          ----
;        |              |
;        o              o
;       / \            / \
;    ---   -----     --   ---
;    |         |     |      |
;   10         o     5      4
;             / \
;         ----   -
;         |      |
;         6      2
(define my-mob
  (make-mobile (make-branch 2 (make-mobile (make-branch 3 10)
                                           (make-branch 5 (make-mobile (make-branch 4 6)
                                                                       (make-branch 1 2)))))
               (make-branch 4 (make-mobile (make-branch 2 5)
                                           (make-branch 3 4)))))

; a
(define (left-branch m) (car m))
(define (right-branch m) (cdr m))

;b
; Some helper utils
(define (get_length branch) (car branch))
(define (get_struct branch) (cdr branch))
(define (is_mobile? struct) (pair? struct))

; Mutual recursion seems the way to go..
(define (branch-weight branch)
  (define (iter b acc)
    (let ((struct (get_struct b)))
      ; The struct is either a simple number (just add) or a mobile (recurse)
      (if (is_mobile? struct)
        (total-weight struct)
        (+ acc struct))))
  (iter branch 0))

(define (total-weight mob)
  ; Any mobile is just the sum of its branches
  (+ (branch-weight (left-branch mob))
     (branch-weight (right-branch mob))))

;c
; Another helper
(define (torque branch)
  (* (get_length branch)
     (branch-weight branch)))

(define (balanced? mobile)
  (let ((l_br (left-branch mobile))
        (r_br (right-branch mobile)))
    (= (torque l_br)
       (torque r_br))))

;d
; Redefine to use cons instead of list
; Check the git diff to verify that I did, indeed, start out with lists
; and the changes weren't all that big. Go me!

(define my-tree '(1 (2 (3 4) 5) (6 7)))

(define (scale-tree tree factor)
  (cond ((null? tree) nil)
        ((not (pair? tree)) (* tree factor))
        (else (cons (scale-tree (car tree) factor)
                    (scale-tree (cdr tree) factor)))))

(define (scale-tree tree factor)
  (map (lambda (sub-tree)
         (if (pair? sub-tree)
           (scale-tree sub-tree factor)
           (* sub-tree factor)))
       tree))

; 2.30
; First, just refactor scale-tree
(define (square-tree1 tree)
  (cond ((null? tree) nil)
        ((not (pair? tree)) (* tree tree))
        (else (cons (square-tree1 (car tree))
                    (square-tree1 (cdr tree))))))

; Second, refactor their map version
(define (square-tree2 tree)
  (map (lambda (sub-tree)
         (if (pair? sub-tree)
           (square-tree2 sub-tree)
           (* sub-tree sub-tree)))
       tree))

; 2.31
; map equivalent for trees
(define (tree-map func tree)
  (cond ((null? tree) nil)
        ((not (pair? tree)) (func tree))
        (else (cons (tree-map func (car tree))
                    (tree-map func (cdr tree))))))

(define (square-tree3 tree) (tree-map square tree))

; 2.32
; More mathy than I'd like.
; To solve, I used the following advice
;   The set of all subsets of a given set is the union of:
;     # the set of all subsets excluding the first number.
;     # the set of all subsets excluding the first number,
;       with the first number re-inserted into each subset.
; This means that since 'rest' contains the first bullet point, we just cons the car
; (which is the first number) onto each of its elements and append them together
(define (subsets s)
  (if (null? s)
    (list nil)
    (let ((rest (subsets (cdr s))))
      (append rest (map (lambda (r) (cons (car s) r)) rest)))))

; 2.33
(define (accumulate op initial sequence)
  (if (null? sequence)
    initial
    (op (car sequence)
        (accumulate op initial (cdr sequence)))))

(define (accmap p sequence)
  (accumulate (lambda (x y) (cons (p x) y)) nil sequence))

(define (accappend seq1 seq2)
  (accumulate cons seq2 seq1))

(define (acclength sequence)
  (accumulate (lambda (val acc) (+ 1 acc)) 0 sequence))

; 2.34
; Almost skipped as "too mathy", but..
; Weird-arse way of solving the polynomial, I must say..
(define (horner-eval x coefficient-sequence)
  (accumulate (lambda (this-coeff higher-terms) (+ this-coeff (* x higher-terms)))
              0
              coefficient-sequence))

; 2.35
; This was hard work until I saw the "map" hint -
; was expecting op to be the lambda that did all the work,
; putting it into the seq instead is a nice touch
(define (acc-count-leaves x)
  (accumulate +
              0
              (map (lambda (node) (if (pair? node) (acc-count-leaves node) 1)) x)))

; 2.36
; Simpler than you'd think
(define (accumulate-n op init seqs)
  (if (null? (car seqs))
    nil
    (cons (accumulate op init (map car seqs))
          (accumulate-n op init (map cdr seqs)))))

; 2.37
; Almost too mathemetical, but the operations were easy enough to google...
(define matrix '((1 2 3 4)(4 5 6 6)(6 7 8 9)))
(define (dot-product v w)
  (accumulate + 0 (map * v w)))

(define (matrix-*-vector m v)
  (map (lambda (row) (map * row v)) m))

(define (transpose mat)
  (accumulate-n cons nil mat))

; A *lot* of mapping here.. can't be helped..
(define (matrix-*-matrix m n)
  (let ((cols (transpose n)))
    (map (lambda (m-row)
           (map (lambda (col-row)
                  (dot-product m-row col-row))
                cols))
         m)))

; 2.38
(define foldr accumulate)
(define (foldl op initial seq)
  (define (iter result rest)
    (if (null? rest)
    result
    (iter (op result (car rest))
          (cdr rest))))
  (iter initial seq))
(foldr / 1 (list 1 2 3))
; (/ 1 (/ 2 (/ 3 1)))
; (/ 1 2/3)
;> 3/2
(foldl / 1 (list 1 2 3))
; (/ (/ (/ 1 1) 2) 3)
; (/ (/ 1 2) 3)
; (/ 1/2 3)
;> 1/6
(foldr list nil (list 1 2 3))
;> (1 (2 (3 ())))
(foldl list nil (list 1 2 3))
;> (((() 1) 2) 3)
; I forget the name of the property.. it's the one that means
; x * y == y * x
; but
; x / y != y / x
; Googled it - commutative

; 2.39
(define (reverse_r seq)
  (foldr (lambda (x y) (append y (list x))) nil seq))

(define (reverse_l seq)
  (foldl (lambda (x y) (cons y x)) nil seq))

; pre-2.40
(define (flatmap proc seq)
  (accumulate append nil (map proc seq)))
(define (prime? n)
  (let loop ((d 2))
    (cond ((< n (* d d)) #t)
          ((zero? (modulo n d)) #f)
          (else (loop (+ d 1))))))
(define (prime-sum? pair)
  (prime? (+ (car pair) (cadr pair))))
(define (make-pair-sum pair)
  (list (car pair) (cadr pair) (+ (car pair) (cadr pair))))
(define (enumerate-interval low high)
  (if (> low high)
    nil
    (cons low (enumerate-interval (+ low 1) high))))
(define (prime-sum-pairs n)
  (map make-pair-sum
       (filter prime-sum?
               (unique-pairs n))))

; 2.40
(define (unique-pairs n)
  (flatmap
    (lambda (i)
      (map (lambda (j) (list i j))
           (enumerate-interval 1 (- i 1))))
    (enumerate-interval 1 n)))

; 2.41
(define (ordered-triples n s)
  (filter (lambda (l) (= s (accumulate + 0 l)))
          (flatmap (lambda (i)
                     (flatmap (lambda (j)
                                (map (lambda (k) (list i j k))
                                     (enumerate-interval 1 j)))
                              (enumerate-interval 1 i)))
                   (enumerate-interval 1 n))))

(define (queens board-size)
  (define (queen-cols k)
    (if (= k 0)
      (list empty-board)
      (filter
        (lambda (positions) (safe? positions))
        (flatmap
          (lambda (prev_cols)
            (map (lambda (row-num)
                   (add-queen row-num prev_cols))
                 (enumerate-interval 1 board-size)))
          (queen-cols (- k 1))))))
  (queen-cols board-size))

; 2.42
; Pain in the arse!
; Trying to work out the data structure from the sample code was painful
; Key points:
;   empty-board is defined outside of the context that knows the board size
;       so can't be a list of board-size lists. Can only be nil
;   the innermost map traverses the sequence 1 - board-size and add-queen doesn't
;       know board-size so the only representation of a column that works is a number
;   so we must be iterating over the list(s) in the "board so far" and adding every new 1-boardsize
;       possibility to it:
;       (()) -> ((1) (2) (3) (4)) -> ((1 1) (1 2) (1 3) (1 4) (2 1) etc... )
;   and then we filter it down to the valid ones. Checking for row clashes is a trivial accum
;       check, diagonals are a bit more of a PITA. Could probably write the code more tersely
;       but this way is legible enough. It does unecessary checks like -ve nums, but they
;       can't trigger a false positive so I don't care enough to guard against
(define empty-board nil)

(define (safe? positions)
    (and (safe-row? positions) (safe-diag? positions)))

(define (safe-row? positions)
  (let ((test (car positions)))
    (= 0 (accumulate +
                     0
                     (map (lambda (x) (if (= test x)
                                        1
                                        0))
                          (cdr positions))))))

(define (safe-diag? positions)
  (let ((test (car positions)))
    (define (iter l count)
      (cond ((null? l) #t)
            ((or (= test (+ (car l) count))
                 (= test (- (car l) count))) #f)
            (else (iter (cdr l) (+ 1 count)))))
    (iter (cdr positions) 1)))

(define (add-queen row-num prev_cols)
  (cons row-num prev_cols))

; 2.43 It's re-running queen-cols every time, of course it's slow - almost as bad as pure fib

; 2.44
; It's annoying to write code I can't run.
(define (up-split painter n)
  (if (= n 0)
    painter
    (let ((smaller (up-split painter (- n 1))))
      (below painter (beside smaller smaller)))))

; 2.45
; This is where I'd reach for labels in CL, but to keep within SICP..
(define (split op1 op2)
  (define (rec painter n)
    (if (= n 0)
    painter
    (let ((smaller (rec painter (- n 1))))
      (op1 (op2 smaller smaller) painter))))
  rec)

; 2.46
(define (make-vect a b)
  (cons a b))
(define (xcor-vect v)
  (car v))
(define (ycor-vect v)
  (cdr v))  ; cons not list - cdr not cadr!
(define (add-vect a b)
  (make-vect (+ (xcor-vect a)
                (xcor-vect b))
             (+ (ycor-vect a)
                (ycor-vect b))))
(define (sub-vect a b)
  (make-vect (- (xcor-vect a)
                (xcor-vect b))
             (- (ycor-vect a)
                (ycor-vect b))))
(define (scale-vect n v)
  (make-vect (* n (xcor-vect v))
             (* n (ycor-vect v))))

; 2.47
; Seems like things got easy again all of a sudden.. what are they up to?
; Imp 1:
(define (frame_origin f)
  (car f))
(define (frame_edge_1 f)
  (cadr f))
(define (frame_edge_2 f)
  (caddr f))
; Imp 2:
(define (frame_origin f)
  (car f))
(define (frame_edge_1 f)
  (cadr f))
(define (frame_edge_2 f)
  (cddr f))

; 2.48
; Take a list of segments, return a function that will paint them to a frame
(define (segments->painter segment-list)
  (lambda (frame)
    (for-each
      (lambda (segment)
        (draw-line
          ((frame-coord-map frame) (start-segment segment))
          ((frame-coord-map frame) (end-segment segment))))
      segment-list)))

(define (make-segment start end)
  (cons start end))
(define (start-segment s)
  (car s))
(define (end-segment s)
  (cdr s))

; 2.49
; a - outline frame (remember frames go from 0 to 1!)
(define vect-bl (make-vect 0 0))
(define vect-tl (make-vect 0 1))
(define vect-br (make-vect 1 0))
(define vect-tr (make-vect 1 1))

(define mid-t (make-vect 0.5 1))
(define mid-b (make-vect 0.5 0))
(define mid-l (make-vect 0 0.5))
(define mid-r (make-vect 1 0.5))

; NB: The following are all painters
(define (frame-outline f)
  (segments->painter (make-segment vect-bl vect-tl)
                     (make-segment vect-tl vect-tr)
                     (make-segment vect-tr vect-br)
                     (make-segment vect-br vect-bl)))

(define (frame-x f)
  (segments->painter (make-segment vect-bl vect-tr)
                     (make-segment vect-tl vect-br)))

(define (frame-diamond f)
  (segments->painter (make-segment mid-t mid-r)
                     (make-segment mid-r mid-b)
                     (make-segment mid-b mid-l)
                     (make-segment mid-l mid-t)))

;(define (frame-wave f)
  ; TBD - I'll need the physical book and/or a grid system!
;  )

; 2.50
(define (flip-horiz painter)
  (transform-painter painter
                     (make-vect 1.0 0.0)   ; new origin
                     (make-vect 0.0 0.0)   ; new end of edge1
                     (make-vect 1.0 1.0))) ; new end of edge2

; Could just call rotate90 two or three times, but..
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
; Modify beside
(define (below painter1 painter2)
  (let ((split-point (make-vect 0.0 0.5)))
    (let ((paint-below
            (transform-painter painter1
                               (make-vect 0.0 0.0)
                               (make-vect 1.0 0.0)
                               split-point))
          (paint-above
            (transform-painter painter2
                               split-point
                               (make-vect 1.0 0.5)
                               (make-vect 0.0 1.0))))
      (lambda (frame)
        (paint-below frame)
        (paint-above frame)))))

; Via beside & rotate
(define (below painter1 painter2)
  (rotate270 (beside (rotate90 painter1) (rotate90 painter2))))

; 2.52
(define (corner-split painter n)
  (if (= n 0)
    painter
    (beside (below painter (up-split painter (- n 1)))
            (below (right-split painter (- n 1)) (corner-split painter (- n 1))))))

(define (square-of-four tl tr bl br)
  (lambda (painter)
    (let ((top (beside (tl painter) (tr painter)))
          (bottom (beside (bl painter) (br painter))))
      (below bottom top))))

(define (square-limit painter n)
  (let ((combine4 (square-of-four flip-vert rotate180
                                  identity flip-horiz)))
    (combine4 (corner-split painter n))))

; 2.54
(define (my_equal? a b)
  (if (and (pair? a) (pair? b)) ; Can't eq? pairs...
    (and (my_equal? (car a) (car b))
         (my_equal? (cdr a) (cdr b)))
    (eq? a b)))

(define (deriv e var)
  (let ((exp (add-parens e)))
    (cond ((number? exp) 0)
          ((variable? exp)
           (if (same-variable? exp var) 1 0))
          ((sum? exp)
           (make-sum (deriv (addend exp) var)
                     (deriv (augend exp) var)))
          ((product? exp)
           (make-sum
             (make-product (multiplier exp)
                           (deriv (multiplicand exp) var))
             (make-product (deriv (multiplier exp) var)
                           (multiplicand exp))))
          ((exponentiation? exp)
           (make-product (make-product (exponent exp)
                                       (make-exponentiation (base exp)
                                                            (make-sum (exponent exp) -1)))
                         (deriv (base exp) var)))
          (else
            (error "unknown expression type -- DERIV" exp)))))

(define (variable? x) (symbol? x))

(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))

(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2)) (+ a1 a2))
        (else (list a1 '+ a2))))

(define (=number? exp num)
  (and (number? exp) (= exp num)))

(define (make-product m1 m2)
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2)) (* m1 m2))
        (else (list m1 '* m2))))

(define (sum? x)
  (and (pair? x) (eq? (cadr x) '+)))

(define (addend a) (car a))

(define (augend a) (caddr a))

(define (product? x)
  (and (pair? x) (eq? (cadr x) '*)))

(define (multiplier p) (car p))

(define (multiplicand p) (caddr p))

; 2.56
(define (exponentiation? x)
  (and (pair? x) (eq? (car x) '**)))

(define (base x) (cadr x))

(define (exponent x) (caddr x))

(define (make-exponentiation e1 e2)
  (cond ((=number? e2 0) 1)
        ((=number? e2 1) e1)
        (else (list '** e1 e2))))

; 2.58
; Ooooog. Okay. Right. So. Here's my thinking.
; Even without parens, infix operators are binary: They take two arguments only
; So (a + b + c) is two instances of (exp + exp), they're just nested
; Because sum and product are commutative, they can work in any order:
; (a + b + c) = (a + (b + c)) = ((a + b) + c)
; (a * b * c) = (a * (b * c)) = ((a * b) * c)
; However, because * has higher precedence than +
; (a + b * c) = (a + (b * c)) only
; (a * b + c) = ((a * b) + c) only
; So, this tells us:
; (a + b) - if b is a single thing, we're done
;           if b is more than one thing, we need (a + (b)) - i.e. deal with b then add a
; (a * b) - if b is a single thing, we're done
;           if b is more than one thing, we need ((a * (car b)) (cdr b))
;               i.e. multiple a by the first part of b before moving on
; So to process algebra with missing parens:
;   Single values need no work
;   (a + seq) -> (a + (seq))
;   (a * seq) -> ((a * seq1) seq2...)
; This will eventually get us to the fully-paren'd expression that deriv can already handle
; Originally, I thought this would require recursing. But it doesn't, because deriv already
; recurses to break down all the sub expressions. So we just need to handle:
;   Single value -> returned unchanged
;   length-3 list -> return unchanged, it's already a valid infix expression
;   Anything else -> put the parens around the right thing for the + or *

(define (add-parens lst)
  (cond ((not (pair? lst)) lst)
        ((= 3 (length lst)) lst)
        ((eq? '+ (cadr lst)) (list (car lst)
                                   '+
                                   (cddr lst)))
        ((eq? '* (cadr lst)) (append (list
                                       (list (car lst)
                                             '*
                                             (caddr lst)))
                                     (cdddr lst)))))


; Sets
; union-set, intersection-set, element-of-set?, and adjoin-set

(define (element-of-set? x set)
  (cond ((null? set) false)
        ((equal? x (car set)) true)
        (else (element-of-set? x (cdr set)))))

(define (adjoin-set x set)
  (if (element-of-set? x set)
    set
    (cons x set)))

(define (intersection-set set1 set2)
  (cond ((or (null? set1) (null? set2)) '())
        ((element-of-set? (car set1) set2)
         (cons (car set1)
               (intersection-set (cdr set1) set2)))
        (else (intersection-set (cdr set1) set2))))

; 2.59
(define (union-set set1 set2)
  (cond ((null? set1) set2)
        ((element-of-set? (car set1) set2)
         (union-set (cdr set1) set2))
        (else (cons (car set1) (union-set (cdr set1) set2)))))

; 2.60
; Only these two need changes if we're happy with dupes
(define (adjoin-set x set)  ; just stop checking for dupes
  (cons x set))

(define (union-set set1 set2)
  (append set1 set2))   ; If we don't care about dupes, just merge the two
; You get some efficiency wins, like union, but it does mean you have more entries to search
; when using element-of-set? so it comes down to whether you're doing a lot of looking vs.
; doing a lot of merging. You may also like the history that allowing dupes gives you:
; If you keep getting the same entries over and over, you may be able to optimise.

; 2.61 - ordered version
(define (adjoin-set x set)
  (cond ((null? set) (list x))
        ((= x (car set)) set)
        ((> x (car set)) (cons (car set) (adjoin-set x (cdr set))))
        (else (cons x set))))

; 2.62
(define (union-set set1 set2)
  (cond ((null? set1) set2)
        ((null? set2) set1)
        (else
          (let ((x1 (car set1)) (x2 (car set2)))
            (cond ((= x1 x2) (cons x1 (union-set (cdr set1) (cdr set2))))
                  ((< x1 x2) (cons x1 (union-set (cdr set1) set2)))
                  (else (cons x2 (union-set set1 (cdr set2)))))))))

(define fig2-16-1 '(7 (3 (1 () ()) (5 () ())) (9 () (11 () ()))))
(define fig2-16-2 '(3 (1 () ()) (7 (5 () ()) (9 () (11 () ())))))
(define fig2-16-3 '(5 (3 (1 () ()) ()) (9 (7 () ()) (11 () ()))))

(define (entry tree) (car tree))
(define (left-branch tree) (cadr tree))
(define (right-branch tree) (caddr tree))
(define (make-tree entry left right)
  (list entry left right))

(define (element-of-set? x set)
  (cond ((null? set) false)
        ((= x (entry set)) true)
        ((< x (entry set))
         (element-of-set? x (left-branch set)))
        ((> x (entry set))
         (element-of-set? x (right-branch set)))))

(define (adjoin-set x set)
  (cond ((null? set) (make-tree x '() '()))
        ((= x (entry set)) set)
        ((< x (entry set))
         (make-tree (entry set)
                    (adjoin-set x (left-branch set))
                    (right-branch set)))
        ((> x (entry set))
         (make-tree (entry set)
                    (left-branch set)
                    (adjoin-set x (right-branch set))))))

(define (tree->list-1 tree)
  (if (null? tree)
    '()
    (append (tree->list-1 (left-branch tree))
            (cons (entry tree)
                  (tree->list-1 (right-branch tree))))))

(define (tree->list-2 tree)
  (define (copy-to-list tree result-list)
    (if (null? tree)
      result-list
      (copy-to-list (left-branch tree)
                    (cons (entry tree)
                          (copy-to-list (right-branch tree)
                                        result-list)))))
  (copy-to-list tree '()))

(define (list->tree elements)
  (car (partial-tree elements (length elements))))

(define (partial-tree elts n)
  (if (= n 0)
    (cons '() elts)
    (let ((left-size (quotient (- n 1) 2)))
      (let ((left-result (partial-tree elts left-size)))
        (let ((left-tree (car left-result))
              (non-left-elts (cdr left-result))
              (right-size (- n (+ left-size 1))))
          (let ((this-entry (car non-left-elts))
                (right-result (partial-tree (cdr non-left-elts)
                                            right-size)))
            (let ((right-tree (car right-result))
                  (remaining-elts (cdr right-result)))
              (cons (make-tree this-entry left-tree right-tree)
                    remaining-elts))))))))

; 2.64
; So, taking the magical handwavy approach: We have an ordered list,
; we want an ordered tree. That list consists of:
;  zero or more low values
;  zero or one middle value
;  zero or more high values
; So our list of (lows, middle, highs) needs to be turned into ([lows], middle, [highs])
; where []s indicate trees. Imagine we have a list->tree function already, called magic.
; The effect we want is (list (magic lows) middle (magic highs))
; But that's a little tricky, since we want to just use car/cdr and that doesn't scale so
; well for the highs part. But hey, a tree is a single list! So if the highs are tree'd,
; things are easy. Let's imagine magic takes a list and a number, and returns a list
; where the first element is the first <number> of elements, correctly tree'd, then we'd have
; (magic list) -> ([lows] middle highs) - then middle is just the cadr, and the list we
; want to pass to magic for the right tree is just cddr.
; So we're looking for a function that takes a list and a number, and trees that number of
; entries in a list. Hello, partial-tree!
; The quotient call is what splits the list to find our middle entry and tree the others;
; when n hits 0 it returns an empty tree; and recursion basically does the rest!
; ...this would be a lot nicer with labels rather than let-let-let-let-let!

; (1 3 5 7 9 11) ->     5
;                      / \
;                     /   \
;                    1     \
;                     \     9
;                      3   / \
;                         7   11
;
; 2.65
; It's just tree->list -> union/intersection -> list->tree
(define (union-tree tree1 tree2)
  (list->tree (union-set (tree->list tree1)
                         (tree->list tree2))))

(define (intersection-tree tree1 tree2)
  (list->tree (intersection-set (tree->list tree1)
                                (tree->list tree2))))

; 2.66
(define (tree-lookup given-key set-of-records)
  (cond ((null? set-of-records) #f)
        ((= given-key (key (entry set-of-records)))
         (entry set-of-records))
        ((< given-key (key (entry set-of-records)))
         (tree-lookup given-key (left-branch set-of-records)))
        (else
         (tree-lookup given-key (right-branch set-of-records)))))

; Huffman
(define (make-leaf symbol weight)
  (list 'leaf symbol weight))
(define (leaf? object)
  (eq? (car object) 'leaf))
(define (symbol-leaf x) (cadr x))
(define (weight-leaf x) (caddr x))

(define (make-code-tree left right)
  (list left
        right
        (append (symbols left) (symbols right))
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
  (define (decode-1 bits current-branch)
    (if (null? bits)
      '()
      (let ((next-branch
              (choose-branch (car bits) current-branch)))
        (if (leaf? next-branch)
          (cons (symbol-leaf next-branch)
                (decode-1 (cdr bits) tree))
          (decode-1 (cdr bits) next-branch)))))
  (decode-1 bits tree))

(define (choose-branch bit branch)
  (cond ((= bit 0) (left-branch branch))
        ((= bit 1) (right-branch branch))
        (else (error "bad bit -- CHOOSE-BRANCH" bit))))

(define (adjoin-set x set)
  (cond ((null? set) (list x))
        ((< (weight x) (weight (car set))) (cons x set))
        (else (cons (car set)
                    (adjoin-set x (cdr set))))))

(define (make-leaf-set pairs)
  (if (null? pairs)
    '()
    (let ((pair (car pairs)))
      (adjoin-set (make-leaf (car pair)    ; symbol
                             (cadr pair))  ; frequency
                  (make-leaf-set (cdr pairs))))))

; 2.67
(define sample-tree
  (make-code-tree (make-leaf 'A 4)
                  (make-code-tree
                    (make-leaf 'B 2)
                    (make-code-tree (make-leaf 'D 1)
                                    (make-leaf 'C 1)))))

(define sample-message '(0 1 1 0 0 1 0 1 0 1 1 1 0))
(decode sample-message sample-tree) ; (a d a b b c a)

; 2.68
(define (encode message tree)
  (if (null? message)
    '()
    (append (encode-symbol (car message) tree)
            (encode (cdr message) tree))))

(define (in-list? x set)
  (cond ((null? set) false)
        ((equal? x (car set)) true)
        (else (in-list? x (cdr set)))))

(define (encode-symbol symbol tree)
  (define (correct-branch? symbol branch)
    (if (leaf? branch)
      (eq? symbol (symbol-leaf branch))
      (in-list? symbol (symbols branch))))
  (cond ((and (leaf? tree) (correct-branch? symbol tree)) '())
        ((correct-branch? symbol (left-branch tree))
         (cons 0 (encode-symbol symbol (left-branch tree))))
        ((correct-branch? symbol (right-branch tree))
         (cons 1 (encode-symbol symbol (right-branch tree))))
        (else (error "bad symbol -- ENCODE-SYMBOL" symbol))))

; 2.69
(define (generate-huffman-tree pairs)
  (successive-merge (make-leaf-set pairs)))

(define (successive-merge pairs)
  (cond ((null? pairs) '())
        ((= 1 (length pairs)) (car pairs))
        (else (successive-merge (adjoin-set (make-code-tree (car pairs)
                                                            (cadr pairs))
                                            (cddr pairs))))))

; 2.70
(define msg270 '(Get a job Sha na na na na na na na na Get a job Sha na na na na na na na na Wah yip yip yip yip yip yip yip yip yip Sha boom))
(define tree270 (generate-huffman-tree '((A 2) (NA 16) (BOOM 1) (SHA 3) (GET 2) (YIP 9) (JOB 2) (WAH 1))))
(define enc270 (encode msg270 tree270))
(length enc270)
; There are 84 bits. Fixed-length would require 3 bits to encode the 8 options, so
(* 3 (length msg270)) ; 108

; Quick & dirty function to generate a range of numbers
(define (.. x y) (cond ((> x y) '())(else (cons x (.. (+ 1 x) y)))))

; And onto generic functions!
(define (add-complex z1 z2)
  (make-from-real-imag (+ (real-part z1) (real-part z2))
                       (+ (imag-part z1) (imag-part z2))))
(define (sub-complex z1 z2)
  (make-from-real-imag (- (real-part z1) (real-part z2))
                       (- (imag-part z1) (imag-part z2))))
(define (mul-complex z1 z2)
  (make-from-mag-ang (* (magnitude z1) (magnitude z2))
                     (+ (angle z1) (angle z2))))
(define (div-complex z1 z2)
  (make-from-mag-ang (/ (magnitude z1) (magnitude z2))
                     (- (angle z1) (angle z2))))

(define (install-rectangular-package)
  ;; internal procedures
  (define (real-part z) (car z))
  (define (imag-part z) (cdr z))
  (define (make-from-real-imag x y) (cons x y))
  (define (magnitude z)
    (sqrt (+ (square (real-part z))
             (square (imag-part z)))))
  (define (angle z)
    (atan (imag-part z) (real-part z)))
  (define (make-from-mag-ang r a)
    (cons (* r (cos a)) (* r (sin a))))
  ;; interface to the rest of the system
  (define (tag x) (attach-tag 'rectangular x))
  (put 'real-part '(rectangular) real-part)
  (put 'imag-part '(rectangular) imag-part)
  (put 'magnitude '(rectangular) magnitude)
  (put 'angle '(rectangular) angle)
  (put 'make-from-real-imag 'rectangular
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'rectangular
       (lambda (r a) (tag (make-from-mag-ang r a))))
  'done)

(define (install-polar-package)
  ;; internal procedures
  (define (magnitude z) (car z))
  (define (angle z) (cdr z))
  (define (make-from-mag-ang r a) (cons r a))
  (define (real-part z)
    (* (magnitude z) (cos (angle z))))
  (define (imag-part z)
    (* (magnitude z) (sin (angle z))))
  (define (make-from-real-imag x y)
    (cons (sqrt (+ (square x) (square y)))
          (atan y x)))
  ;; interface to the rest of the system
  (define (tag x) (attach-tag 'polar x))
  (put 'real-part '(polar) real-part)
  (put 'imag-part '(polar) imag-part)
  (put 'magnitude '(polar) magnitude)
  (put 'angle '(polar) angle)
  (put 'make-from-real-imag 'polar
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'polar
       (lambda (r a) (tag (make-from-mag-ang r a))))
  'done)

; tagged data
(define (attach-tag type-tag contents)
  (cons type-tag contents))
(define (type-tag datum)
  (if (pair? datum)
    (car datum)
    (error "Bad tagged datum -- TYPE-TAG" datum)))
(define (contents datum)
  (if (pair? datum)
    (cdr datum)
    (error "Bad tagged datum -- CONTENTS" datum)))
(define (rectangular? z)
  (eq? (type-tag z) 'rectangular))
(define (polar? z)
  (eq? (type-tag z) 'polar))

(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))
          (error
            "No method for these types -- APPLY-GENERIC"
            (list op type-tags))))))

(define (real-part z) (apply-generic 'real-part z))
(define (imag-part z) (apply-generic 'imag-part z))
(define (magnitude z) (apply-generic 'magnitude z))
(define (angle z) (apply-generic 'angle z))

(define (make-from-real-imag x y)
  ((get 'make-from-real-imag 'rectangular) x y))
(define (make-from-mag-ang r a)
  ((get 'make-from-mag-ang 'polar) r a))


(define (deriv exp var)
   (cond ((number? exp) 0)
         ((variable? exp) (if (same-variable? exp var) 1 0))
         (else ((get 'deriv (operator exp)) (operands exp)
                                            var))))
(define (operator exp) (car exp))
(define (operands exp) (cdr exp))

; 2.73
(define (install-deriv-package)
  (define (addend d)
    (car d))
  (define (augend d)
    (cadr d))
  (define (make-sum x y)
    (cond ((= x 0) y)
          ((= y 0) x)
          ((and (number? x) (number? y)) (+ x y))
          (else (list '+ x y))))
  (define (deriv-sum exp var)
    (make-sum (deriv (addend exp) var)
              (deriv (augend exp) var)))
  (put 'deriv '+ deriv-sum)

  (define (multiplier d)
    (car d))
  (define (multiplicand d)
    (cadr d))
  (define (make-product x y)
    (cond ((or (=number? x 0) (=number? y 0)) 0)
          ((=number? x 1) y)
          ((=number? y 1) x)
          ((and (number? x) (number? y)) (* x y))
          (else (list '* x y))))
  (define (deriv-prod exp var)
    (make-sum
      (make-product (multiplier exp)
                    (deriv (multiplicand exp) var))
      (make-product (deriv (multiplier exp) var)
                    (multiplicand exp))))
  (put 'deriv '* deriv-prod)

  (define (base x)
    (car x))
  (define (exponent x)
    (cadr x))
  (define (make-exponentiation x y)
    (cond ((=number? y 0) 1)
          ((=number? y 1) x)
          (else (list '** x y))))

  (define (deriv-exp exp var)
    (make-product (make-product (exponent exp)
                                (make-exponentiation (base exp)
                                                     (make-sum (exponent exp) -1)))
                  (deriv (base exp) var)))
  (put 'deriv '** deriv-exp))

; 2.74
; Seems rather vague at first glance.. just "everything's different, write code for it"
; Still.. let's get what we know summed up
; IE has multiple divisions
; Divisions have one personnel file, each using its own structure
; Files contain records of all employees, keyed by name, different structure per div.
; Employee records contain a keyed set of inormation, varied by div.
; So, IE -> Divs -> File -> employees -> employee data
; So, logically..
(define (get-record employee file)
  ((get 'get-record (division file)) employee
                                      file))
; ^ Assumptions made: We have a "division" function to find out what division a file belongs to
;                     We have a "get" function that can retrieve the appropriate operation
;                       from the dispatch table given the operation and the division
;                     The retrieved function knows how to retrieve the record given the employee
;                       and file variables. The only type data needed at this point is the
;                       division that owns the file, which we assume is attached to the file itself

(define (get-salary record)
  ((get 'get-salary (division record)) record))
; ^ Pretty similar, we assume that the record knows what division it belongs to (which seems
; reasonable since record formats vary per division) and that we have the required functions
; etc. to get the type and use it to retrieve the right operation

(define (find-employee-record employee files)
  (define (iter lst)
    (if (null? lst)
      (error "Employee not in records" employee)
      (else (let ((rec (get-record employee (car lst))))
              (if (null? rec)
                (iter (cdr lst))
                rec)))))
  (iter files))
; ^ Basic assumption here is that get-record returns null if it finds no record.
; We just iterate over the files checking each for the employee, and return the record when found
; If not found, throw an error.

; Lastly, d: Just add the relevant operations for the new division to the dispatch table.
; Everything will JFW at that point

; 2.75
(define (make-from-mag-ang x y)
  (define (dispatch op)
    (cond ((eq? op 'real-part) (* x (cos y)))
          ((eq? op 'imag-part) (* x (sin y)))
          ((eq? op 'magnitude) x)
          ((eq? op 'angle) y)
          (else
            (error "Unknown op -- MAKE-FROM-REAL-IMAG" op))))
  dispatch)

; 2.76
; Explicit: Every function has a cond for different types
;   Pros: Simplicity
;   Cons: Need to update every function for new types
;
; Data-directed: Generic functions
;   Pros: Concise
;   Cons: More "book keeping" code to make it all work
;   I rate this as the best choice for changeable systems, whether type or op:
;       One of the others might seem better for variable types, since it keeps all logic
;         associated with that type. But each type must have operations to handle all other
;         types. So adding a new type would mean updating every existing type. Not helpful.
;       With generics, you can keep all the different versions of a function grouped together
;         in a useful way.
;
; Message-passing: OO
;   Pros: Concise
;   Cons: All logic for functions kept in the object itself

; 2.77
; Apply-generic "unwraps" the object passed in by one layer. Defining magnitude for complex
; means that you call apply-generic on the complex-tagged object, which results in it dispatching to
; the magnitude function and applying it to the rectangular- or magnitude-tagged object within.
; Which results in a second call of apply-generic, so we get the correct function for the data type.

; 2.78
(define (type-tag datum)
  (cond ((number? datum) 'scheme-number)
        ((pair? datum) (car datum))
        (else (error "Bad tagged datum -- TYPE-TAG" datum))))

(define (attach-tag type-tag contents)
  (if (eq 'scheme-number type-tag)
    contents
    (cons type-tag contents)))

(define (contents datum)
  (cond ((number? datum) datum)
        ((pair? datum) (cdr datum))
        (else (error "Bad tagged datum -- CONTENTS" datum))))

; 2.79 & 2.28
(define (add x y) (apply-generic 'add x y))
(define (sub x y) (apply-generic 'sub x y))
(define (mul x y) (apply-generic 'mul x y))
(define (div x y) (apply-generic 'div x y))
(define (equ? x y) (apply-generic 'equ? x y))
(define (=zero? x) (apply-generic '=zero? x))
(define (install-scheme-number-package)
  (define (tag x)
    (attach-tag 'scheme-number x))
  (put 'add '(scheme-number scheme-number)
       (lambda (x y) (tag (+ x y))))
  (put 'sub '(scheme-number scheme-number)
       (lambda (x y) (tag (- x y))))
  (put 'mul '(scheme-number scheme-number)
       (lambda (x y) (tag (* x y))))
  (put 'div '(scheme-number scheme-number)
       (lambda (x y) (tag (/ x y))))
  (put 'make 'scheme-number
       (lambda (x) (tag x)))
  (put 'equ? '(scheme-number scheme-number)
       (lambda (x y) (= x y)))
  (put '=zero 'scheme-number
       (lambda (x) (equ? 0 x)))
  'done)

(define (install-rational-package)
  ;; internal procedures
  (define (numer x) (car x))
  (define (denom x) (cdr x))
  (define (make-rat n d)
    (let ((g (gcd n d)))
      (cons (/ n g) (/ d g))))
  (define (add-rat x y)
    (make-rat (+ (* (numer x) (denom y))
                 (* (numer y) (denom x)))
              (* (denom x) (denom y))))
  (define (sub-rat x y)
    (make-rat (- (* (numer x) (denom y))
                 (* (numer y) (denom x)))
              (* (denom x) (denom y))))
  (define (mul-rat x y)
    (make-rat (* (numer x) (numer y))
              (* (denom x) (denom y))))
  (define (div-rat x y)
    (make-rat (* (numer x) (denom y))
              (* (denom x) (numer y))))
  ;; interface to rest of the system
  (define (tag x) (attach-tag 'rational x))
  (put 'add '(rational rational)
       (lambda (x y) (tag (add-rat x y))))
  (put 'sub '(rational rational)
       (lambda (x y) (tag (sub-rat x y))))
  (put 'mul '(rational rational)
       (lambda (x y) (tag (mul-rat x y))))
  (put 'div '(rational rational)
       (lambda (x y) (tag (div-rat x y))))
  (put 'equ? '(rational rational)
       (lambda (x y) (= (* (numer x) (denom y))
                        (* (numer y) (denom x)))))
  (put '=zero? 'rational
       (lambda (n) (equ? 0 n)))

  (put 'make 'rational
       (lambda (n d) (tag (make-rat n d))))
  'done)
(define (make-rational n d)
  ((get 'make 'rational) n d))

(define (install-complex-package)
  ;; imported procedures from rectangular and polar packages
  (define (make-from-real-imag x y)
    ((get 'make-from-real-imag 'rectangular) x y))
  (define (make-from-mag-ang r a)
    ((get 'make-from-mag-ang 'polar) r a))
  ;; internal procedures
  (define (add-complex z1 z2)
    (make-from-real-imag (+ (real-part z1) (real-part z2))
                         (+ (imag-part z1) (imag-part z2))))
  (define (sub-complex z1 z2)
    (make-from-real-imag (- (real-part z1) (real-part z2))
                         (- (imag-part z1) (imag-part z2))))
  (define (mul-complex z1 z2)
    (make-from-mag-ang (* (magnitude z1) (magnitude z2))
                       (+ (angle z1) (angle z2))))
  (define (div-complex z1 z2)
    (make-from-mag-ang (/ (magnitude z1) (magnitude z2))
                       (- (angle z1) (angle z2))))
  ;; interface to rest of the system
  (define (tag z) (attach-tag 'complex z))
  (put 'add '(complex complex)
       (lambda (z1 z2) (tag (add-complex z1 z2))))
  (put 'sub '(complex complex)
       (lambda (z1 z2) (tag (sub-complex z1 z2))))
  (put 'mul '(complex complex)
       (lambda (z1 z2) (tag (mul-complex z1 z2))))
  (put 'div '(complex complex)
       (lambda (z1 z2) (tag (div-complex z1 z2))))
  (put 'equ? '(complex complex)
       (lambda (x y) (and (= (real-part x) (real-part y))
                          (= (imag-part x) (imag-part y)))))
  (put '=zero? 'complex
       (lambda (x) (equ? 0 x)))
  (put 'make-from-real-imag 'complex
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'complex
       (lambda (r a) (tag (make-from-mag-ang r a))))
  'done)

(define (make-complex-from-real-imag x y)
  ((get 'make-from-real-imag 'complex) x y))
(define (make-complex-from-mag-ang r a)
  ((get 'make-from-mag-ang 'complex) r a))

(put 'real-part '(complex) real-part)
(put 'imag-part '(complex) imag-part)
(put 'magnitude '(complex) magnitude)
(put 'angle '(complex) angle)

; 2.81 a
; Infinite loop, it will keep trying to apply-generic on what it THINKS are 'raised' params
; b
; So long as we have a valid process in the table for the two types of operand, there is no
; problem - coercion is only tried when no process is found.
; c
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
            (if (not (equal? type1 type2))
              (let ((t1->t2 (get-coercion type1 type2))
                    (t2->t1 (get-coercion type2 type1)))
                (cond (t1->t2
                        (apply-generic op (t1->t2 a1) a2))
                      (t2->t1
                        (apply-generic op a1 (t2->t1 a2)))
                      (else
                        (error "No method for these types"
                               (list op type-tags)))))))
          (error "No method for these types"
                 (list op type-tags)))))))

; 2.82
(define (apply-generic op . args)
  (define (id x) x) ; I'm surprised this doesn't seem to exist, but we need it
  (define (get-coercions types lst)
    ; This function tries to get the list of coercions that will coerce all items
    ; to the same type. If it fails, it returns the empty list
    (define (iter t l a)
      ; This function tries the first type in t on all elements in l
      ; If all elements in l can be coerced to that type, it returns a list of
      ; the coercion operations. Else it tries the next item in t until t is exhausted
      ; whereupon it returns nil
      (let ((to_type (car t))
            (from_type (car l)))
        (cond ((null? l) a) ; Passed-in list is empty - we got coercions for all!
              ((null? t) '()) ; Ran out of types to try coercing to :(
              ((equal? to_type from_type) (iter t (cdr l) (cons id a))) ; No coerce needed
              (else (let ((op (get-coercion from_type to_type)))
                      (if op    ; can we coerce to the desired type?
                        (iter t (cdr l) (cons op a)) ; Yes! Onto the next one
                        (iter (cdr t) lst '()))))))) ; No :( Try the next type, reset to original
                                                     ; list and empty accumulator
    (iter types lst '()))
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
        (apply proc (map contents args))
        (let ((coercions (get-coercions type-tags type-tags)))
          (if coercions
            (let (cargs (map (lambda (coercion arg) (coercion arg)) coercions args))
              (apply apply-generic (cons op cargs)))
            (error "No method for these types" (list op type-tags))))))
    (error "No method for these types" (list op type-tags))))
; Note: This will endlessly-loop if there's no suitable process to operate on params
; that have been coerced already.
; It will also not work on items that could be coerced to a type that is not in the list.

; 2.83
(define (raise x) (apply-generic 'raise x))
; Integer -> rational
(put 'raise 'integer
     (lambda (i) (make-rational i 1)))
; Rational -> real
(put 'raise 'rational
     (lambda (r) (* 1.0 (/ (numer r) (denom r)))))
; Real -> complex
(put 'raise 'real
     (lambda (r) (make-complex-from-real-imag r 0)))

; 2.84
(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
        (apply proc (map contents args))
        (if (= (length args) 2)
          (let ((type1 (car type-tags))
                (type2 (cadr type-tags)))
            (if (not (equal? type1 type2))
              (let ((raised (raise-to-equal args)))
                (apply-generic op (car raised) (cadr raised)))
              (error "No method for these types" (list op type-tags))))
          (error "No method for these types" (list op type-tags)))))))

(define (raise-to-equal args)
  (define (raised-order x)
    ; Probably nicer ways to do this.. but we don't know about hashes etc. yet!
    (cond ((eq? x 'integer)  0)
          ((eq? x 'rational) 1)
          ((eq? x 'real)     2)
          ((eq? x 'complex)  3)))
  (let ((a1 (car args))
        (a2 (cadr args)))
    (let ((ta1 (type-tag a1))
          (ta2 (type-tag a2)))
      (cond ((eq? ta1 ta2) args)
            ((> ta1 ta2) (raise-to-equal (list a1 (raise a2))))
            (else (raise-to-equal (list (raise a1) a2)))))))

; 2.85
(define (project x) (apply-generic 'project x))
(put 'project 'rational
     (lambda (x) (numer x)))
(put 'project 'real
     (lambda (x) (round x)))
(put 'project 'complex
     (lambda (x) (real-part x)))

(define (drop x)
  (let (projected (project x))
    (if (and (equ? x (raise projected))
             (not (eq? 'scheme-number (type-tag x))))
      (drop projected)
      x)))

(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
        (let (result (apply proc (map contents args)))
          (if (or (eq? op 'equ) (eq? op 'raise)) ; Don't drop in a raise or equality test
            result
            (drop result)))
        (if (= (length args) 2)
          (let ((type1 (car type-tags))
                (type2 (cadr type-tags)))
            (if (not (equal? type1 type2))
              (let ((raised (raise-to-equal args)))
                (apply-generic op (car raised) (cadr raised)))
              (error "No method for these types" (list op type-tags))))
          (error "No method for these types" (list op type-tags)))))))

; 2.86 +
; No, I just can't be bothered. It's really cool to see how the generic concept lends itself
; even to having polynomial fractions and all... but I'm sick of re-writing apply-generic
; Probably worth coming back to it, but I just don't see enough value in hammering the
; "Look how generic we can get!" point this far to justify the amount of time it's going to take
