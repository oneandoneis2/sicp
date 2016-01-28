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

; 3.6
(define random-init 4) ; Chosen by fair dice roll. Gauranteed random.
(define rand
  (let ((x random-init))
    (define (rand-update n) (+ 1 n))
    (lambda (type)
      (cond ((eq? type 'generate) (begin (set! x (rand-update x))
                                         x))
            ((eq? type 'reset) (lambda (new)
                                 (set! x new)
                                 x))
            (else (error "Unknown random operation: " type))))))

; 3.7
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
            ((eq? m 'make-joint) (lambda (new-pass)
                                   (lambda (pwd m)
                                     (if (eq? pwd new-pass)
                                       (dispatch pass m)))))
            (else (error "Unknown request -- MAKE-ACCOUNT"
                         m)))
      wrong_pass))
  dispatch)

(define (make-joint acc acc-pass new-pass)
  ; It's tempting to just return a function with the original account & pass
  ; stored here. But that doesn't involve changing make-account as per their hint.
  ; We'll need a different approach for that....
  ; So, it's a function that makes a function
  ; that makes a function that returns a function.
  ; What's wrong with that..?
  ((acc acc-pass 'make-joint) new-pass))
; (define peter-acc (make-account 100 'open-sesame))
; ((peter-acc 'open-sesame 'withdraw) 1)
; 99
; ((peter-acc 'open-sesame 'withdraw) 1)
; 98
; (define paul-acc (make-joint peter-acc 'open-sesame 'rosebud))
; ((paul-acc 'rosebud 'withdraw) 1)
; 97
; ((peter-acc 'open-sesame 'withdraw) 1)
; 96

; 3.8
(define f
  ; Toggle between returning param and 0
  (let ((count 0))
    (lambda (x) (if (= count 0)
                  (begin (set! count 1) x)
                  (begin (set! count 0) 0)))))
; (+ (f 0) (f 1))
; 1
; (+ (f 1) (f 0))
; 0

; 3.12
; First (cdr x) -> (b) because append does not mutate
; Second (cdr x) -> (b c d) because append! points the last cdr in x to y

; 3.13
; It never ends - cyclic list!

; 3.14
; mystery is a weird alternative reverse

; 3.17

(define (count-pairs x)
  (let ((seen '()))
    (define (count-if-unseen pair)
      (if (memq pair seen)
        0
        (begin (set! seen (cons pair seen))
               1)))
    (define (iter pair)
      (if (not (pair? pair))
        0
        (+ (iter (car pair))
           (iter (cdr pair))
           (count-if-unseen pair))))
    (iter x)))

; 3.18
(define (make-cycle x)
  (set-cdr! (last-pair x) x)
  x)
(define z (make-cycle (list 'a 'b 'c)))
(define (detect-cyclic-list x)
  ; Note: Exercise only wants to know about simple lists, not arbitrary nestings thereof
  (let ((seen '()))
    (define (detect-if-seen lst)
      (if (memq lst seen)
        #t
        (begin (set! seen (cons lst seen))
               #f)))
    (define (iter lst)
      (if (null? lst)
        #f
        (or (detect-if-seen lst)
            (iter (cdr lst)))))
    (iter x)))

; 3.19
; Tortoise vs. hare! t moves on one iterm at a time, h moves two.
(define (detect-cyclic-list2 x)
  (define (safe-cdr l)
    (if (null? l)
      '()
      (cdr l)))
  (let ((t x) (h x))
    (define (iter)
      (set! t (safe-cdr t))
      (set! h (safe-cdr (safe-cdr h)))
      (cond ((eq? t h) #t)  ; This can only happen if there's a loop
            ((null? h) #f)  ; If there's no loop, h will reach the end first
            (else (iter))))   ; Haven't found a loop, haven't reached the end, keep going
    (iter)))
