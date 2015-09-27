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
