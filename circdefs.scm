(define (append x y)
  (if (null? x)
    y
    (cons (car x)
          (append (cdr x) y))))

(define (map fn lst)
  (if (null? lst)
    '()
    (cons (fn (car lst))
          (map fn (cdr lst)))))
