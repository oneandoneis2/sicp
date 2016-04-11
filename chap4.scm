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
