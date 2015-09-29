(define (sum-of-squares x y) (+ (square x) (square y)))
(define (sos_gt x y z)
  (define vals (cdr (sort (list x y z) <)))
  (sum-of-squares (car vals) (cadr vals)))
(define (a-plus-abs-b a b)
  ((if (> b 0) + -) a b))
; ^ a-b == a+(-b) so use subtract function if b<0
(define (p) (p))
(define (test x y)
  (if (= x 0)
    0
    y))
; (test 0 (p))
; applicative-order - attempts to evaluate (p) first -> endless loop
; normal-order - (test 0 (p)) -> (if (= 0 0) 0 (p)) -> 0 - no need to evaluate (p)

; Square-root calculations - first, the book way
(define (booksqrt x)
  (define (good-enough? guess x)
    (< (abs (- ( square guess) x)) 0.001))
  (define (average x y)
    (/ (+ x y) 2))
  (define (improve guess x)
    (average guess (/ x guess)))
  (define (sqrt-iter guess x)
    (if (good-enough? guess x)
      guess
      (sqrt-iter (improve guess x) x)))
  (sqrt-iter 1.0 x))

; My version, changing the test for the difference between guesses
(define (mysqrt x)
  (define (good-enough? guess oldguess)
    (< (abs (- oldguess guess)) 0.001))
  (define (average x y)
    (/ (+ x y) 2))
  (define (improve guess x)
    (average guess (/ x guess)))
  (define (sqrt-iter guess oldguess x)
    (if (good-enough? guess oldguess)
      guess
      (sqrt-iter (improve guess x) guess x)))
  (sqrt-iter 1.0 x x))
; Certainly seems better for small numbers, not much in it for bigger ones tho..
(booksqrt 9)
;> 3.00009155413138
(mysqrt 9)
;> 3.000000001396984
