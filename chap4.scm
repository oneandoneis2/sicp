; 4.1
; Left to right:
(define (list-of-values exps env)
  (if (no-operands? exps)
    '()
    (let* ((first (eval (first-operand exps) env))
           (rest (list-of-values (rest-operands exps) env)))
      (cons first rest))))
; Right to left:
(define (list-of-values exps env)
  (if (no-operands? exps)
    '()
    (let* ((rest (list-of-values (rest-operands exps) env))
           (first (eval (first-operand exps) env)))
      (cons first rest))))

; 4.2
; a - (define ...) will be treated as an application instead of an assignment
; b
(define (application? exp) (tagged-list? exp 'call))
(define (operator exp) (cadr exp))
(define (operands exp) (cddr exp))

; 4.3
; First, invent the universe. Or at least the alist
(define (make-alist)
  (let ((procs '()))
    (define put (lambda (key val) (set! procs (cons (cons key val) procs))))
    (define (find key lst)
      (cond ((eq? lst '()) #f)
            ((eq? (caar lst) key) (cdar lst))
            (else (find key (cdr lst)))))
    (define get (lambda (key) (find key procs)))
    (define (op arg)
      (cond ((eq? arg 'show) (display procs)(newline))
            ((eq? arg 'get) get)
            ((eq? arg 'put) put)))
    op))

(define eval-procs (make-alist))
(define (setup-eval ops)
  (define (put key val) ((eval-procs 'put) key val))
  (put 'quote text-of-quotation)
  (put 'set eval-assignment)
  (put 'define eval-definition)
  (put 'if eval-if)
  (put 'lambda make-procedure)
  (put 'begin eval-sequence))

(define (dd-eval exp env)
  (define (get key) ((eval-procs 'get) key))
  (cond ((self-evaluating? exp) exp)
        ((variable? exp) (lookup-variable-value exp env))
        ((get (car exp)) ((get (car exp)) exp env))
        ((application? exp)
         (apply (eval (operator exp) env)
                (list-of-values (operands exp) env)))
        (else
          (error "Unknown expression type -- EVAL" exp))))

; 4.4
(define (and-exps exp) (cdr exp))
(define (eval-and exp env)
  (let ((exps (and-exps exp)))
    (cond ((null? exps) #t)
          ((last-exp? exps) (eval (first-exp exps) env))
          (else
            (if (eval (first-exp exps) env)
              (eval-and (rest-exps exps) env)
              #f)))))

(define (or-exps exp) (cdr exp))
(define (eval-or exp env)
  (let ((exps (or-exps exp)))
    (if (null? exps)
      #f
      (let ((res (eval (first-exp exps) env)))
        (if res
          res
          (eval-or (rest-exps exps) env))))))

; 4.5
; This means evaluating the predicate twice, but can't avoid this yet as we don't
; have 'let' in our evaluator to store data :(
(define (cond=>? clause)
  (eq? '=> (cadr clause)))
(define (cond=>fun clause)
  (caddr clause))
(define (expand-clauses clauses)
  (if (null? clauses)
    'false                          ; no else clause
    (let ((first (car clauses))
          (rest (cdr clauses)))
      (if (cond-else-clause? first)
        (if (null? rest)
          (sequence->exp (cond-actions first))
          (error "ELSE clause isn't last -- COND->IF" clauses))
        (make-if (cond-predicate first)
                 (if (cond=>? first)
                   (list (cond=>fun first) (cond-predicate first))
                   (sequence->exp (cond-actions first)))
                 (expand-clauses rest))))))

; 4.6
(define (let? exp) (tagged-list? exp 'let))
(define (let-vars exp)
  (let ((let-list (cadr exp)))
    (map car let-list)))
(define (let-vals exp)
  (let ((let-list (cadr exp)))
    (map cadr let-list)))
(define (let-body exp) (cddr exp))
(define (let->lambda exp)
  `((lambda ,(let-vars exp) ,@(let-body exp)) ,@(let-vals exp)))

; Mostly doing stuff in the meta file instead now. Easier.
; 4.14
; the function param to map in the circ. is actually '(primitive <the + function>)
; if we write map, it will correctly handle the primitive.
; If we import it, it'll blow up about being passed a list instead of a function
;
; 4.15
; You can't prove a negative - halt? could only work by running the procedure and waiting
; You can't distinguish between "hasn't returned yet" and "will never return"
;
; 4.21 a: Anonymous fib function:
(lambda (n)
  ((lambda (fn)
     (fn fn n))
   (lambda (fn num)
     (if (< num 2)
       num
       (+ (fn fn (- num 1)) (fn fn (- num 2)))))))

; b: Fill in blanks:
(define (f x)
  ((lambda (even? odd?)
     (even? even? odd? x))
   (lambda (ev? od? n)
     (if (= n 0) true (od? ev? od? (- n 1))))
   (lambda (ev? od? n)
     (if (= n 0) false (ev? ev? od? (- n 1))))))

; 4.35
; First attempt:
(define (an-integer-between lo hi)
  (amb lo (if (> lo hi)
            (amb)
            (an-integer-between (+ n 1) hi))))

; Rewrite when you remember & understand "require"
(define (an-integer-between lo hi)
  (require (<= lo hi))
  (amb lo (an-integer-between (+ lo 1) hi)))

; 4.36
(define (all-pythag-trips)
  (define (an-integer-starting-from n)
    (amb n (an-integer-starting-from (+ n 1))))
  (define (iter j)
    ; i is the numbers between 1 and j
    ; k can't be longer than i+j
    (let ((i (an-integer-between 1 j)))
      (let ((k (an-integer-between 1 (+ i j))))
        (require (= (+ (* i i) (* j j)) (* k k)))
        (list i j k))))
  (iter (an-integer-starting-from 1)))

(define (a-pythagorean-triple)
  (let ((i (an-integer-between low high)))
    (let ((j (an-integer-between i high)))
      (let ((k (an-integer-between j high)))
        (require (= (+ (* i i) (* j j)) (* k k)))
        (list i j k)))))

; 4.40
(define (multiple-dwelling)
  (let ((cooper (amb 2 3 4 5))
        (miller (amb 3 4 5)))
    (require (> miller cooper))
    (let ((fletcher (amb 2 3 4)))
      (require (not (= (abs (- fletcher cooper)) 1)))
      (let ((smith (amb 1 2 3 4 5)))
        (require (not (= (abs (- smith fletcher)) 1)))
        (let ((baker (amb 1 2 3 4)))
          (require (distinct? (list baker cooper fletcher miller smith)))
          (list (list 'baker baker)
                (list 'cooper cooper)
                (list 'fletcher fletcher)
                (list 'miller miller)
                (list 'smith smith)))))))

; 4.41
; Fuck elegance, we're going to brute-force all the things!
(define (brute-multiple-dwelling)
  (define (floors) (list 1 2 3 4 5))
  (define (adjacent? x y)
    (= (abs (- x y)) 1))
  (define (valid? baker cooper fletcher miller smith)
    (and (not (= baker 5))
         (not (= cooper 1))
         (not (= fletcher 1))
         (not (= fletcher 5))
         (> miller cooper)
         (not (adjacent? smith fletcher))
         (not (adjacent? cooper fletcher))
         (distinct? (list baker cooper fletcher miller smith))))
  (define (distinct? items)
    (cond ((null? items) true)
          ((null? (cdr items)) true)
          ((member (car items) (cdr items)) false)
          (else (distinct? (cdr items)))))
  (let ((results '()))
    (map (lambda (baker)
           (map (lambda (cooper)
                  (map (lambda (fletcher)
                         (map (lambda (miller)
                                (map (lambda (smith)
                                       (if (valid? baker cooper fletcher miller smith)
                                         (set! results (list (list 'baker baker)
                                                             (list 'cooper cooper)
                                                             (list 'fletcher fletcher)
                                                             (list 'miller miller)
                                                             (list 'smith smith)))
                                         '()))
                                     (floors)))
                              (floors)))
                       (floors)))
                (floors)))
         (floors))
    results))

; 4.43
(define (father-of-lorna)
  (define (names) (amb 'Lorna 'Melissa 'Rosalind 'Gabrielle 'MaryAnne))
  (define daughter car)
  (define yacht cadr)
  (let ((Moore (list 'MaryAnne 'Lorna))
        (Barnacle (list 'Melissa 'Gabrielle))
        (Downing (list names 'Melissa))
        (Hall (list names 'Rosalind))
        (Parker (list names 'Parker)))
    (require (distinct? Downing))   ; yacht can't have same name as daughter
    (require (distinct? Hall))
    (require (distinct? Parker))
    (require (not (eq? (daughter Parker) 'Gabrielle)))
    (require (not (eq? (daughter Hall) 'Rosalind)))
    ; Gabrielle's father owns the yacht that is named after Dr. Parker's daughter
    ; Her father can only be Downing or Hall
    (require (cond ((eq? (daughter Hall) 'Gabrielle) (eq? (yacht Hall) (daughter Parker)))
                   ((eq? (daughter Downing) 'Gabrielle) (eq? (yacht Downing) (daughter Parker)))
                   #f))
    ; And now we can't share any names for daughters or yachts
    (require (distinct? (map daughter (list Moore Barnacle Downing Hall Parker))))
    (require (distinct? (map yacht (list Moore Barnacle Downing Hall Parker))))
    (list (list 'Moore Moore)
          (list 'Barnacle Barnacle)
          (list 'Downing Downing)
          (list 'Hall Hall)
          (list 'Parker Parker))))
