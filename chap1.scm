(define (sum-of-squares x y) (+ (square x) (square y)))
(define (smallest_first? x y z) (and (<= x y)(<= x z)))
(define (sos_gt x y z)
  (cond
    ((smallest_first? x y z) (sum-of-squares y z))
    ((smallest_first? y x z) (sum-of-squares x z))
    ((smallest_first? z x y) (sum-of-squares x y))))
