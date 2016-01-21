; 3.1
(define (make-accumulator n)
  (lambda (x)
    (set! n (+ n x))
    n))

; 3.2
(define (make-monitored fn)
  (let ((count 0))
    (lambda (x)
      (cond ((eq? x 'how-many-calls?) count)
            (else (begin (set! count (+ 1 count))
                         (fn x)))))))

; 3.3
(define (make-account balance pwd)
  (define (withdraw amount)
    (if (>= balance amount)
      (begin (set! balance (- balance amount))
             balance)
      "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (define (wrong_pass amount)
    "Incorrect password")
  (define (dispatch pass m)
    (if (eq? pass pwd)
      (cond ((eq? m 'withdraw) withdraw)
            ((eq? m 'deposit) deposit)
            (else (error "Unknown request -- MAKE-ACCOUNT"
                         m)))
      wrong_pass))
  dispatch)

; 3.4
(define (make-account balance pwd)
  (let ((count 0))
    (define (withdraw amount)
      (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds"))
    (define (deposit amount)
      (set! balance (+ balance amount))
      balance)
    (define (wrong_pass amount)
      (if (> count 7)
        (call-the-cops)
        "Incorrect password"))
    (define (call-the-cops)
      "I've called the cops!")
    (define (dispatch pass m)
      (if (eq? pass pwd)
        (begin (set! count 0)
               (cond ((eq? m 'withdraw) withdraw)
                     ((eq? m 'deposit) deposit)
                     (else (error "Unknown request -- MAKE-ACCOUNT"
                                  m))))
        (begin (set! count (+ 1 count))
               wrong_pass)))
    dispatch))

; 3.5
(define (monte-carlo trials experiment)
  (define (iter trials-remaining trials-passed)
    (cond ((= trials-remaining 0)
           (/ trials-passed trials))
          ((experiment)
           (iter (- trials-remaining 1) (+ trials-passed 1)))
          (else
            (iter (- trials-remaining 1) trials-passed))))
  (iter trials 0))

(define (random-in-range low high)
  (let ((range (- high low)))
    (+ low (random range))))

(define (estimate-integral p x1 x2 y1 y2 trials)
  (define (experiment) (p (random-in-range x1 x2) (random-in-range y1 y2)))
  (monte-carlo trials experiment))

(define (estimate-pi trials)
  ; Area of a circle = pi r²
  ; If r = 1 then area = pi
  ; So a box that's 2x2 can hold a circle that's r = 1
  (define (test-circle x y) (<= (+ (square x) (square y)) 1))
  ( * (* 2.0 2.0)   ; Area of box
      (estimate-integral test-circle
                         -1.0 1.0 -1.0 1.0  ; Must be floats or r-i-r only does ints - no good!
                         trials)))  ; Proportion of box that is circle
; (estimate-pi 10000000)
; ;Value: 3.1416156
; Close enough! :)
