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
  (define (average x y) (/ (+ x y) 2))
  (let ((start (start-segment s))
        (end (end-segment s)))
    (make-segment (average (x-point start) (x-point end))
                  (average (y-point start) (y-point end)))))

(define (print-point p)
  (newline)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")"))
