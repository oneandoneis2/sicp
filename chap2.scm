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
