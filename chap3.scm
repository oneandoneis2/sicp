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

; 3.24
(define (make-table . equality)
  (let ((local-table (list '*table*)))
    (let ((same-key? (if (null? equality) equal? (car equality))))
      (define (my-assoc key records)
        (cond ((null? records) false)
              ((same-key? key (caar records)) (car records))
              (else (my-assoc key (cdr records)))))
      (define (lookup key-1 key-2)
        (let ((subtable (my-assoc key-1 (cdr local-table))))
          (if subtable
            (let ((record (my-assoc key-2 (cdr subtable))))
              (if record
                (cdr record)
                false))
            false)))
      (define (insert! key-1 key-2 value)
        (let ((subtable (my-assoc key-1 (cdr local-table))))
          (if subtable
            (let ((record (my-assoc key-2 (cdr subtable))))
              (if record
                (set-cdr! record value)
                (set-cdr! subtable
                          (cons (cons key-2 value)
                                (cdr subtable)))))
            (set-cdr! local-table
                      (cons (list key-1
                                  (cons key-2 value))
                            (cdr local-table)))))
        'ok)
      (define (dispatch m)
        (cond ((eq? m 'lookup-proc) lookup)
              ((eq? m 'insert-proc!) insert!)
              (else (error "Unknown operation -- TABLE" m))))
      dispatch)))

; 3.25
(define (make-table)
  (let ((local-table (list '*table*)))
    (define (lookup keys)
      (define (iter keylist table)
        (let ((subtable (assoc (car keylist) (cdr table))))
          (if subtable
            (let ((record (cdr subtable))
                  (next (cdr keylist)))
              (if (null? next)
                record
                (iter next subtable)))
            false)))
      (iter keys local-table))
    (define (insert! keys value)
      (define (iter keylist table)
        (let* ((key (car keylist))
               (next (cdr keylist))
               (subtable (assoc key (cdr table))))
          (if subtable
            (if (null? next)
              (set-cdr! subtable value)
              (iter next subtable))
            (if (null? next)
              (set-cdr! table (cons (cons key value) (cdr table)))
              (begin (set-cdr! table (cons (list key) (cdr table)))
                     (iter next (assoc key (cdr table))))))))
      (iter keys local-table))
    (define (dispatch m)
      (cond ((eq? m 'lookup-proc) lookup)
            ((eq? m 'insert-proc!) insert!)
            (else (error "Unknown operation -- TABLE" m))))
    dispatch))

; 3.26 - Surprisingly nice to implement!
(define (make-table-tree)
  ; For a tree that's made up of ((key value) left-ptr right-ptr)
  ; where an empty list represents termination

  ; Define some utility methods:
  (define (entry tree) (cdar tree))
  (define (id tree) (caar tree))
  (define (left-branch tree) (cadr tree))
  (define (right-branch tree) (caddr tree))
  (define (make-tree key value left right) (list (cons key value) left right))
  ; and mutators
  (define (set-tree-right! tree new) (set-car! (cddr tree) new))
  (define (set-tree-left! tree new) (set-car! (cdr tree) new))
  (define (set-tree-val! tree new) (set-cdr! (car tree) new))

  (let ((local-table (list '*table*)))
    (define (lookup key)
      (define (iter tree)
        ; Nice & simple tree iteration, go left or right until found/out of tree
        (cond ((null? tree) #f)
              ((= key (id tree)) (entry tree))
              ((< key (id tree)) (iter (left-branch tree)))
              (else (iter (right-branch tree)))))
      (iter (cdr local-table)))
    (define (insert! key value)
      (define (iter tree)
        (cond ((null? tree) (set-cdr! local-table (make-tree key value '() '())))
              ; ^ Only happens if no nodes have been entered yet,
              ; i.e. local-table is just (*table*)
              ((= key (id tree)) (set-tree-val! tree value)) ; Exists, mutate
              ((< key (id tree)) ; Either iterate further or create new left node
               (if (null? (left-branch tree))
                 (set-tree-left! tree (make-tree key value '() '()))
                 (iter (left-branch tree))))
              (else ; Ditto, only on right
                (if (null? (right-branch tree))
                  (set-tree-right! tree (make-tree key value '() '()))
                  (iter (right-branch tree))))))
      (iter (cdr local-table))
      'ok)
    (define (show-tree) (display local-table))
    (define (dispatch m)
      (cond ((eq? m 'lookup-proc) lookup)
            ((eq? m 'insert-proc!) insert!)
            ((eq? m 'display) (show-tree))
            (else (error "Unknown operation -- TABLE" m))))
    dispatch))

; 3.27
; memo-fib only calculates each value once, therefore linear growth in steps
; Wouldn't work, because fib calls itself for recursion, which would bypass the memoization
; of the values it returns.

(define (half-adder a b s c)
  (let ((d (make-wire)) (e (make-wire)))
    (or-gate a b d)
    (and-gate a b c)
    (inverter c e)
    (and-gate d e s)
    'ok))

(define (full-adder a b c-in sum c-out)
  (let ((s (make-wire))
        (c1 (make-wire))
        (c2 (make-wire)))
    (half-adder b c-in s c1)
    (half-adder a s sum c2)
    (or-gate c1 c2 c-out)
    'ok))

(define (inverter input output)
  (define (invert-input)
    (let ((new-value (logical-not (get-signal input))))
      (after-delay inverter-delay
                   (lambda ()
                     (set-signal! output new-value)))))
  (add-action! input invert-input)
  'ok)

(define (and-gate a1 a2 output)
  (define (and-action-procedure)
    (let ((new-value
            (logical-and (get-signal a1) (get-signal a2))))
      (after-delay and-gate-delay
                   (lambda ()
                     (set-signal! output new-value)))))
  (add-action! a1 and-action-procedure)
  (add-action! a2 and-action-procedure)
  'ok)

(define (logical-not s)
  (cond ((= s 0) 1)
        ((= s 1) 0)
        (else (error "Invalid signal" s))))

(define (logical-and s1 s2)
  (cond ((and (= s1 1) (= s2 1)) 1)
        ((and (= s1 1) (= s2 0)) 0)
        ((and (= s1 0) (= s2 1)) 0)
        ((and (= s1 0) (= s2 0)) 0)
        (else (error "Invalid signal" s))))

(define (logical-or s1 s2)
  (cond ((and (= s1 1) (= s2 1)) 1)
        ((and (= s1 1) (= s2 0)) 1)
        ((and (= s1 0) (= s2 1)) 1)
        ((and (= s1 0) (= s2 0)) 0)
        (else (error "Invalid signal" s))))

; 3.28
(define (or-gate o1 o2 output)
  (define (or-action-procedure)
    (let ((new-value
            (logical-or (get-signal o1) (get-signal o2))))
      (after-delay or-gate-delay
                   (lambda ()
                     (set-signal! output new-value)))))
  (add-action! o1 or-action-procedure)
  (add-action! o2 or-action-procedure)
  'ok)

; 3.29
(define (or-gate o1 o2 output)
  (let ((no1 (make-waire))
        (no2 (make-wire))
        (aout (make-wire)))
    (inverter o1 no1)
    (inverter o2 no2)
    (and-gate no1 no2 aout)
    (inverter aout output)
    'ok))

; 3.30
(define (ripple-carry-adder list_a list_b list_s c_out)
  (if (or (null? list_a) (null? list_b) (null? list_s))
    (error "Prematurely exhausted list" (list list_a list_b list_s)))
  (let ((c_in (make-wire)))
    (if (and (null? (cdr list_a)) (null? (cdr list_b)) (null? (cdr list_s)))
      (set-signal! c_in 0)
      (iter (cdr list_a) (cdr list_b) (cdr list_s) c_in))
    (full-adder (car list_a) (car list_b) c_in (car list_s) c_out))
  'ok)

; The wire-making
(define (make-wire)
  (let ((signal-value 0) (action-procedures '()))
    (define (set-my-signal! new-value)
      (if (not (= signal-value new-value))
        (begin (set! signal-value new-value)
               (call-each action-procedures))
        'done))
    (define (accept-action-procedure! proc)
      (set! action-procedures (cons proc action-procedures))
      (proc))
    (define (dispatch m)
      (cond ((eq? m 'get-signal) signal-value)
            ((eq? m 'set-signal!) set-my-signal!)
            ((eq? m 'add-action!) accept-action-procedure!)
            (else (error "Unknown operation -- WIRE" m))))
    dispatch))

(define (call-each procedures)
  (if (null? procedures)
    'done
    (begin
      ((car procedures))
      (call-each (cdr procedures)))))

(define (get-signal wire)
  (wire 'get-signal))
(define (set-signal! wire new-value)
  ((wire 'set-signal!) new-value))
(define (add-action! wire action-procedure)
  ((wire 'add-action!) action-procedure))

; 3.31
; Otherwise the initial state would be wrong, e.g. an invertor wouldn't be correctly
; outputting the opposite of the input

(define (after-delay delay action)
  (add-to-agenda! (+ delay (current-time the-agenda))
                  action
                  the-agenda))

(define (propagate)
  (if (empty-agenda? the-agenda)
    'done
    (let ((first-item (first-agenda-item the-agenda)))
      (first-item)
      (remove-first-agenda-item! the-agenda)
      (propagate))))

(define (probe name wire)
  (add-action! wire
               (lambda ()
                 (newline)
                 (display name)
                 (display " ")
                 (display (current-time the-agenda))
                 (display "  New-value = ")
                 (display (get-signal wire)))))

(define (make-agenda) (list 0))
(define the-agenda (make-agenda))
(define inverter-delay 2)
(define and-gate-delay 3)
(define or-gate-delay 5)

(define (make-time-segment time queue)
  (cons time queue))
(define (segment-time s) (car s))
(define (segment-queue s) (cdr s))

(define (current-time agenda) (car agenda))
(define (set-current-time! agenda time)
  (set-car! agenda time))
(define (segments agenda) (cdr agenda))
(define (set-segments! agenda segments)
  (set-cdr! agenda segments))
(define (first-segment agenda) (car (segments agenda)))
(define (rest-segments agenda) (cdr (segments agenda)))

(define (empty-agenda? agenda)
  (null? (segments agenda)))

(define (add-to-agenda! time action agenda)
  (define (belongs-before? segments)
    (or (null? segments)
        (< time (segment-time (car segments)))))
  (define (make-new-time-segment time action)
    (let ((q (make-queue)))
      (insert-queue! q action)
      (make-time-segment time q)))
  (define (add-to-segments! segments)
    (if (= (segment-time (car segments)) time)
      (insert-queue! (segment-queue (car segments))
                     action)
      (let ((rest (cdr segments)))
        (if (belongs-before? rest)
          (set-cdr!
            segments
            (cons (make-new-time-segment time action)
                  (cdr segments)))
          (add-to-segments! rest)))))
  (let ((segments (segments agenda)))
    (if (belongs-before? segments)
      (set-segments!
        agenda
        (cons (make-new-time-segment time action)
              segments))
      (add-to-segments! segments))))

(define (remove-first-agenda-item! agenda)
  (let ((q (segment-queue (first-segment agenda))))
    (delete-queue! q)
    (if (empty-queue? q)
      (set-segments! agenda (rest-segments agenda)))))

(define (first-agenda-item agenda)
  (if (empty-agenda? agenda)
    (error "Agenda is empty -- FIRST-AGENDA-ITEM")
    (let ((first-seg (first-segment agenda)))
      (set-current-time! agenda (segment-time first-seg))
      (front-queue (segment-queue first-seg)))))

; 3.32
; Otherwise you'd update the output before the input, which would lead to it
; being the wrong state

(define (adder a1 a2 sum)
  (define (process-new-value)
    (cond ((and (has-value? a1) (has-value? a2))
           (set-value! sum
                       (+ (get-value a1) (get-value a2))
                       me))
          ((and (has-value? a1) (has-value? sum))
           (set-value! a2
                       (- (get-value sum) (get-value a1))
                       me))
          ((and (has-value? a2) (has-value? sum))
           (set-value! a1
                       (- (get-value sum) (get-value a2))
                       me))))
  (define (process-forget-value)
    (forget-value! sum me)
    (forget-value! a1 me)
    (forget-value! a2 me)
    (process-new-value))
  (define (me request)
    (cond ((eq? request 'I-have-a-value)
           (process-new-value))
          ((eq? request 'I-lost-my-value)
           (process-forget-value))
          (else
            (error "Unknown request -- ADDER" request))))
  (connect a1 me)
  (connect a2 me)
  (connect sum me)
  me)

(define (inform-about-value constraint)
  (constraint 'I-have-a-value))
(define (inform-about-no-value constraint)
  (constraint 'I-lost-my-value))

(define (multiplier m1 m2 product)
  (define (process-new-value)
    (cond ((or (and (has-value? m1) (= (get-value m1) 0))
               (and (has-value? m2) (= (get-value m2) 0)))
           (set-value! product 0 me))
          ((and (has-value? m1) (has-value? m2))
           (set-value! product
                       (* (get-value m1) (get-value m2))
                       me))
          ((and (has-value? product) (has-value? m1))
           (set-value! m2
                       (/ (get-value product) (get-value m1))
                       me))
          ((and (has-value? product) (has-value? m2))
           (set-value! m1
                       (/ (get-value product) (get-value m2))
                       me))))
  (define (process-forget-value)
    (forget-value! product me)
    (forget-value! m1 me)
    (forget-value! m2 me)
    (process-new-value))
  (define (me request)
    (cond ((eq? request 'I-have-a-value)
           (process-new-value))
          ((eq? request 'I-lost-my-value)
           (process-forget-value))
          (else
            (error "Unknown request -- MULTIPLIER" request))))
  (connect m1 me)
  (connect m2 me)
  (connect product me)
  me)

(define (constant value connector)
  (define (me request)
    (error "Unknown request -- CONSTANT" request))
  (connect connector me)
  (set-value! connector value me)
  me)

(define (probe name connector)
  (define (print-probe value)
    (newline)
    (display "Probe: ")
    (display name)
    (display " = ")
    (display value))
  (define (process-new-value)
    (print-probe (get-value connector)))
  (define (process-forget-value)
    (print-probe "?"))
  (define (me request)
    (cond ((eq? request 'I-have-a-value)
           (process-new-value))
          ((eq? request 'I-lost-my-value)
           (process-forget-value))
          (else
            (error "Unknown request -- PROBE" request))))
  (connect connector me)
  me)

(define (make-connector)
  (let ((value false) (informant false) (constraints '()))
    (define (set-my-value newval setter)
      (cond ((not (has-value? me))
             (set! value newval)
             (set! informant setter)
             (for-each-except setter
                              inform-about-value
                              constraints))
            ((not (= value newval))
             (error "Contradiction" (list value newval)))
            (else 'ignored)))
    (define (forget-my-value retractor)
      (if (eq? retractor informant)
        (begin (set! informant false)
               (for-each-except retractor
                                inform-about-no-value
                                constraints))
        'ignored))
    (define (connect new-constraint)
      (if (not (memq new-constraint constraints))
        (set! constraints
          (cons new-constraint constraints)))
      (if (has-value? me)
        (inform-about-value new-constraint))
      'done)
    (define (me request)
      (cond ((eq? request 'has-value?)
             (if informant true false))
            ((eq? request 'value) value)
            ((eq? request 'set-value!) set-my-value)
            ((eq? request 'forget) forget-my-value)
            ((eq? request 'connect) connect)
            (else (error "Unknown operation -- CONNECTOR"
                         request))))
    me))

(define (for-each-except exception procedure list)
  (define (loop items)
    (cond ((null? items) 'done)
          ((eq? (car items) exception) (loop (cdr items)))
          (else (procedure (car items))
                (loop (cdr items)))))
  (loop list))

(define (has-value? connector)
  (connector 'has-value?))
(define (get-value connector)
  (connector 'value))
(define (set-value! connector new-value informant)
  ((connector 'set-value!) new-value informant))
(define (forget-value! connector retractor)
  ((connector 'forget) retractor))
(define (connect connector new-constraint)
  ((connector 'connect) new-constraint))

(define (celsius-fahrenheit-converter c f)
  (let ((u (make-connector))
        (v (make-connector))
        (w (make-connector))
        (x (make-connector))
        (y (make-connector)))
    (multiplier c w u)
    (multiplier v x u)
    (adder v y f)
    (constant 9 w)
    (constant 5 x)
    (constant 32 y)
    'ok))

; 3.33
; Average is half of a + b, therefore a + b = 2c
; So that's it stated in terms of addition and multiplication, just translate:
(define (averager a b c)
  (let ((x (make-connector))
        (two (make-connector)))
    (adder a b x)
    (multiplier c two x)
    (constant 2 two)))

; 3.34
; multiplier can only work out one operand when it knows the other, as well as the result
; It's not smart enough to work back from "result is 9, so operands are both 3"

; 3.35
(define (squarer a b)
  (define (process-new-value)
    (if (has-value? b)
      (if (< (get-value b) 0)
        (error "square less than 0 -- SQUARER" (get-value b))
        (set-value! a
                    (sqrt (get-value b))
                    me))
      (if (has-value? a)
        (set-value! b
                    (square (get-value a))
                    me))))
  (define (process-forget-value)
    (forget-value! a me)
    (forget-value! b me))
  (define (me request)
    (cond ((eq? request 'I-have-a-value)
           (process-new-value))
          ((eq? request 'I-lost-my-value)
           (process-forget-value))
          (else
            (error "Unknown request -- SQUARER" request))))
  (connect a me)
  (connect b me)
  me)

(define (c+ x y)
  (let ((z (make-connector)))
    (adder x y z)
    z))

; 3.37
(define (c- x y)
  (let ((z (make-connector)))
    (adder z y x)
    z))

(define (c* x y)
  (let ((z (make-connector)))
    (multiplier x y z)
    z))

(define (c/ x y)
  (let ((z (make-connector)))
    (multiplier z y x)
    z))

(define (cv x)
  (let ((z (make-connector)))
    (constant x z)
    z))

(define (celsius-fahrenheit-converter x)
  (c+ (c* (c/ (cv 9) (cv 5))
          x)
      (cv 32)))
(define C (make-connector))
(define F (celsius-fahrenheit-converter C))

(probe "Celsius temp" C)
(probe "Fahrenheit temp" F)
(set-value! C 25 'user)

; 3.38
; a: $40, $50, $45, $35
; b: $110, $80, $30

; 3.39
; 100 as well as 101, 121 as the set can now be affected by the interleaving

; 3.40
; 1,000,000 100,000 10,000 1,000 100
; 1,000,000

; 3.41
; Why would a single read-only operation need serializing..?

; 3.42
; BB is right, this is more efficient but functionally identical

; 3.44
; He's wrong - so long as the money is in from-account (as we're told to assume)
; this is safe. It's a single operation submitted to a serialized function in both cases.

; 3.45
; It results in serialising the serialiser, which leads to mutex.
; It will effectively be waiting for itself forever.

; 3.47
; I hate the concept of mangling a single-use thing for a multi-use, but..
; a:
(define (make-semaphore n)
  (let ((mutex (make-mutex))
        (taken 0))
    (define (the-sem m)
      (cond ((eq? m 'acquire)
             (mutex 'acquire)
             (if (< taken n)
               (begin (set! taken (+ 1 taken)) (mutex 'release))
               (begin (mutex 'release) (the-sem 'acquire))
               (mutex 'release)))
            ((eq? m 'release)
             (mutex 'acquire)
             (set! taken (- taken 1))
             (mutex 'release)))
      )
    the-sem))

; b:
(define (make-semaphore n)
  (define (test-and-set! cell)
    (if (< (car cell) n)
      (begin (set-car! cell (+ 1 (car cell)))
             false)
      true))
  (define (reduce! cell)
    (set-car! cell (- 1 (car cell))))
  (let ((cell (list 0)))
    (define (the-semaphore m)
      (cond ((eq? m 'acquire)
             (if (test-and-set! cell)
               (the-semaphore 'acquire))) ; retry
            ((eq? m 'release) (reduce! cell))))
    the-semaphore))
