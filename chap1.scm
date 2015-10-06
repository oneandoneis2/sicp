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

; Ackerman
(define (Ackerman)
  (define (A x y)
    (cond ((= y 0) 0)
          ((= x 0) (* 2 y))
          ((= y 1) 2)
          (else (A (- x 1)
                   (A x (- y 1))))))
  (define (f n) (A 0 n)) ; x is 0, call 2*y so |f = 2n|
  (define (g n) (A 1 n)) ; x is 1, recursively call the 2*y path so |f = 2^n|
  (define (h n) (A 2 n)) ; x is 2, this one's nasty:
                         ; n=1 -> 2                     2^1
                         ; n=2 -> 4     2*2             2^2
                         ; n=3 -> 16    2*2*2*2         2^4
                         ; n=4 -> 65536 2*2*2*2....*2   2^16
                         ; n=5 can't be computer, presumably because it's 2^65536
                         ; So, basically, h n = 2^(h(n-1))
                         ; Fibonacci on steroids :)
  (define (k n) (* 5 n n)))

; Ex 1.11
; fn3 = f(n) = n if n < 3
; fn3 = f(n) = f(n-1) + 2f(n-2) + 3f(n-3)
; Recursive def:
(define (fn3r x)
  (if (< x 3)
    x
    (+ (fn3r (- x 1))
       (* 2 (fn3r (- x 2)))
       (* 3 (fn3r (- x 3))))))

; Iterative def:
(define (fn3i x)
  (define (f n-3 n-2 n-1 n)
    (if (= n 0)
      n-3
      (f n-2 n-1 (+ n-1 (* 2 n-2) (* 3 n-3)) (- n 1))))
  (f 0 1 2 x))

; Ex 1.12
; Pascal's triangle
(define (pascal n)
  (define (sum_above l)
    ; Sum the first two numbers of the supplied list
    ; to generate the appropriate next Pascal number
    (+ (car l) (cadr l)))
  (define (helper l)
    ; Recurse over the supplied list and generate a new list based on the
    ; sum-of-two's, or just '1' if there's only one number to work with
    (cond ((eq? (cdr l) ()) (list 1))
          (else (cons (sum_above l) (helper (cdr l))))))
  ; The generate-the-triangle bit
  (cond ((= n 0) ())        ; Could just assume >0 always, but meh
        ((= n 1) (list 1))  ; First row
        ; All other rows begin with 1 and then are sum_aboves all the way down
        (else (cons 1 (helper (pascal (- n 1)))))))

; 1.16
(define (fast-expt b n)
  (define (helper a b n)
    (cond ((= n 0) a)
          ((even? n) (helper a (square b) (/ n 2)))
          (else (helper (* a b) b (- n 1)))))
  (helper 1 b n))

; 1.17
; Define multiplication via addition, double and half
; in the same "fast" way as the expt example ^
; i.e. 4 * 4 = 8 * 2 = 16 * 1
;      4 * 5 = 4 + (4 * 4) = 4 + (8 * 2) ...etc
(define (fast-mult x y)
  (define (double x) (+ x x))
  (define (halve x) (/ x 2))
  (cond ((= y 0) 0)
        ((even? y) (fast-mult (double x) (halve y)))
        (else (+ x (fast-mult x (- y 1))))))

; 1.18
; Iterative version of above
(define (fast-mult-i x y)
  (define (double x) (+ x x))
  (define (halve x) (/ x 2))
  (define (iter a x y)
    (cond ((= y 0) a)
          ((even? y) (iter a (double x) (halve y)))
          (else (iter (+ a x) x (- y 1)))))
  (iter 0 x y))

; 1.20
; gcd 206 40
; Normal order:
; (if (= 40 0)...
; gcd 40 (r 206 40)
; (if (= (r 206 40) 0) -> (if (= 6 0)...
; gcd (r 206 40) (r 40 (r 206 40))
; And so on until the r evaluates to 0
; So many evals!
