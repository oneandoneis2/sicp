#lang sicp
(#%require sicp-pict)

;2.52
(define (corner-split painter n)
  (if (= n 0)
    painter
    (beside (below painter (up-split painter (- n 1)))
            (below (right-split painter (- n 1)) (corner-split painter (- n 1))))))

(define (square-limit painter n)
  (let ((combine4 (square-of-four flip-horiz identity rotate180 flip-vert)))
    (combine4 (corner-split painter n))))

(define (square-of-four tl tr bl br)
  (lambda (painter)
    (let ((top (beside (tl painter) (tr painter)))
          (bottom (beside (bl painter) (br painter))))
      (below bottom top))))

(define (flipped-pairs painter)
  (let ((combine4 (square-of-four identity flip-vert
                                  identity flip-vert)))
    (combine4 painter)))

; 2.45
(define (split op1 op2)
  (lambda (painter n)
    (if (= n 0)
      painter
      (let ((smaller (right-split painter (- n 1))))
        (op1 painter (op2 smaller smaller))))))
(define right-split (split beside below))
(define up-split (split below beside))

(define (frame-coord-map frame)
  (lambda (v)
    (add-vect
     (frame-origin frame)
     (add-vect (scale-vect (xcor-vect v)
                           (frame-edge1 frame))
               (scale-vect (ycor-vect v)
                           (frame-edge2 frame))))))

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