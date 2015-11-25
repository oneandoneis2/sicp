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

(define (frame-wave f)
  ; TBD - I'll need the physical book and/or a grid system!
  )

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
