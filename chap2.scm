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
