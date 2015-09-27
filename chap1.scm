(define (sum-of-squares x y) (+ (square x) (square y)))
(define (sos_gt x y z)
  (define vals (cdr (sort (list x y z) <)))
  (sum-of-squares (car vals) (cadr vals)))
