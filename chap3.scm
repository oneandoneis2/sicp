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

; Queues
(define (make-queue) (cons '() '()))
(define (empty-queue? q) (null? (front-ptr q)))
(define (front-queue q)
  (if (empty-queue? q)
    (error "FRONT called with an empty queue" q)
    (car (front-ptr q))))
(define (insert-queue! q i)
  (let ((new-pair (cons i '())))
    (cond ((empty-queue? q)
           (set-front-ptr! q new-pair)
           (set-rear-ptr! q new-pair)
           q)
          (else
            (set-cdr! (rear-ptr q) new-pair)
            (set-rear-ptr! q new-pair)
            q))))
(define (delete-queue! q)
  (cond ((empty-queue? q)
         (error "DELETE! called with an empty queue" q))
        (else
          (set-front-ptr! q (cdr (front-ptr q)))
          q)))

(define (front-ptr q) (car q))
(define (rear-ptr q) (cdr q))
(define (set-front-ptr! q i) (set-car! q i))
(define (set-rear-ptr! q i) (set-cdr! q i))

; 3.21
; The queue is interpreted as a list, for which the output is correct.
; e.g. the queue 'a b' is the list ((a b) b), and deleting from the queue
; only updates the car because it doesn't need to update the cdr too
(define (print-queue q)
  (if (empty-queue? q)
    (display "Empty queue")
    (let ((queue (front-ptr q)))
      (map (lambda (x) (display x)(display " ")) queue)
      (newline)
      q)))

; 3.22
(define (make-queue)
  (let ((front-ptr '())
        (rear-ptr '()))
    (define (empty-queue?) (null? front-ptr))
    (define (front-queue)
      (if (empty-queue?)
        (error "FRONT called with an empty queue")
        (car front-ptr)))
    (define (insert-queue! i)
      (let ((new-pair (cons i '())))
        (cond ((empty-queue?)
               (set! front-ptr new-pair)
               (set! rear-ptr new-pair)
               dispatch)
              (else
                (set-cdr! rear-ptr new-pair)
                (set! rear-ptr new-pair)
                dispatch))))
    (define (delete-queue!)
      (cond ((empty-queue?)
             (error "DELETE! called with an empty queue"))
            (else
              (set! front-ptr (cdr front-ptr))
              dispatch)))
    (define (print-queue)
      (if (empty-queue?)
        (display "Empty queue")
        (map (lambda (x) (display x)(display " ")) front-ptr)))
    (define (dispatch m)
      (cond ((eq? m 'empty?) (empty-queue?))
            ((eq? m 'front) (front-queue))
            ((eq? m 'insert) insert-queue!)
            ((eq? m 'delete) (delete-queue!))
            ((eq? m 'print) (print-queue))
            (else (error "Undefined operation -- QUEUE" m))))
    dispatch))

(define (empty-queue? q) (q 'empty?))
(define (front-queue q) (q 'front))
(define (insert-queue! q i) ((q 'insert) i))
(define (delete-queue! q) (q 'delete))
(define (print-queue q) (q 'print))

; 3.23
; The tricky part here is that you need to be able to backtrack from the last element
; in order to be able to delete the last element. So whilst initially it seems like
; you can just copy the original queue for the most part, it doesn't work.
; Nor does "I'll keep track on the penultimate as well" since you may need to wipe out
; multiple last elements. Realistically, the best way is to make a doubly-linked-list,
; one that tracks elements in both directions.
; Rather than implement it from scratch, may as well re-use the built in list stuff
; where possible. So a standard list, but with a car that contains a cons, the car of
; which has the value, the cdr of which has a pointer to the previous cell.
; So, a list where caar gets value, cdar gets prev, cdr gets next
;
;     1st             last
;      |               |
;  |->[ ][ ]->[ ][ ]->[ ][/]
;  |   |       |       |
;  |  [1][/]  [2][ ]  [3][ ]
;  |______________|    ...|

(define (front-dq-ptr dq) (car dq))
(define (next-dq-ptr dq) (cdar dq))
(define (rear-dq-ptr dq) (cdr dq))
(define (prev-dq-ptr dq) (cdadr dq))
(define (set-front-dq-ptr! dq new) (set-car! dq new))
(define (set-rear-dq-ptr! dq new) (set-cdr! dq new))
(define (set-de-next! de new) (set-cdr! de new))
(define (set-de-prev! de new) (set-cdr! (car de) new))
(define (get-de-val de) (car de))
(define (mk-dq-item i) (cons (cons i '()) '()))

(define (make-deque) (cons '() '()))
(define (empty-deque? dq) (null? (front-dq-ptr dq)))
(define (front-deque dq)
  (if (empty-deque? dq)
    (error "FRONT called on empty deque" dq)
    (get-de-val (front-dq-ptr dq))))
(define (rear-deque dq)
  (if (empty-deque? dq)
    (error "REAR called on empty deque" dq)
    (get-de-val (rear-dq-ptr dq))))
(define (print-deque dq)
  (if (empty-deque? dq)
    (display "Empty queue")
    (let ((q (front-dq-ptr dq)))
      (map (lambda (x) (display (get-de-val x))(display " ")) q)
      (newline))))
(define (front-insert-deque! dq i)
  (let ((new-dq (mk-dq-item i)))
    (cond ((empty-deque? dq)
           (set-front-dq-ptr! dq new-dq)
           (set-rear-dq-ptr! dq new-dq))
          (else
            (set-de-prev! (front-dq-ptr dq) new-dq)
            (set-de-next! new-dq (front-dq-ptr dq))
            (set-front-dq-ptr! dq new-dq)))))
(define (rear-insert-deque! dq i)
  (let ((new-dq (mk-dq-item i)))
    (cond ((empty-deque? dq)
           (set-front-dq-ptr! dq new-dq)
           (set-rear-dq-ptr! dq new-dq))
          (else
            (set-de-prev! new-dq (rear-dq-ptr dq))
            (set-de-next! (rear-dq-ptr dq) new-dq)
            (set-rear-dq-ptr! dq new-dq)))))
(define (front-delete-deque! dq)
  (cond ((empty-deque? dq)
         (error "DELETE! called on empty deque dq"))
        (else
          (let ((next (next-dq-ptr dq)))
            (if (null? next)
              (set-rear-dq-ptr! dq '())
              (set-de-prev! next '()))
            (set-front-dq-ptr! dq next)))))
(define (rear-delete-deque! dq)
  (cond ((empty-deque? dq)
         (error "DELETE! called on empty deque dq"))
        (else
          (let ((prev (prev-dq-ptr dq)))
            (if (null? prev)
              (set-front-dq-ptr! dq '())
              (set-de-next! prev '()))
            (set-rear-dq-ptr! dq prev)))))
