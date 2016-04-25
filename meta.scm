(define (myeval exp env)
  (cond ((self-evaluating? exp) exp)
        ((variable? exp) (lookup-variable-value exp env))
        ((quoted? exp) (text-of-quotation exp))
        ((assignment? exp) (eval-assignment exp env))
        ((definition? exp) (eval-definition exp env))
        ((if? exp) (eval-if exp env))
        ((cond? exp) (myeval (cond->if exp) env))
        ((and? exp) (eval-and exp env))
        ((or? exp) (eval-or exp env))
        ((lambda? exp)
         (make-procedure (lambda-parameters exp)
                         (lambda-body exp)
                         env))
        ((begin? exp)
         (eval-sequence (begin-actions exp) env))
        ((let? exp) (myeval (let->combination exp) env))
        ((let*? exp) (myeval (let*->nested-lets exp) env))
        ((letrec? exp) (myeval (letrec->recursive-lets exp) env))
        ((while? exp) (myeval (while->combination exp) env))
        ((application? exp)
         (myapply (myeval (operator exp) env)
                (list-of-values (operands exp) env)))
        (else
          (error "Unknown expression type -- EVAL" exp))))

(define (myapply procedure arguments)
  (cond ((primitive-procedure? procedure)
         (apply-primitive-procedure procedure arguments))
        ((compound-procedure? procedure)
         (eval-sequence
           (procedure-body procedure)
           (extend-environment
             (procedure-parameters procedure)
             arguments
             (procedure-environment procedure))))
        (else
          (error "Unknown procedure type -- APPLY" procedure))))

(define (list-of-values exps env)
  (if (no-operands? exps)
    '()
    (cons (myeval (first-operand exps) env)
          (list-of-values (rest-operands exps) env))))

(define (eval-if exp env)
  (if (true? (myeval (if-predicate exp) env))
    (myeval (if-consequent exp) env)
    (myeval (if-alternative exp) env)))

(define (eval-sequence exps env)
  (cond ((last-exp? exps) (myeval (first-exp exps) env))
        (else (myeval (first-exp exps) env)
              (eval-sequence (rest-exps exps) env))))

(define (eval-assignment exp env)
  (set-variable-value! (assignment-variable exp)
                       (myeval (assignment-value exp) env)
                       env)
  'ok)

(define (eval-definition exp env)
  (define-variable! (definition-variable exp)
                    (myeval (definition-value exp) env)
                    env)
  'ok)

(define (self-evaluating? exp)
  (cond ((number? exp) true)
        ((string? exp) true)
        ((eq? exp #f) true)
        (else false)))

(define (variable? exp) (symbol? exp))

(define (quoted? exp) (tagged-list? exp 'quote))
(define (text-of-quotation exp) (cadr exp))

(define (tagged-list? exp tag)
  (if (pair? exp)
    (eq? (car exp) tag)
    false))

(define (assignment? exp)
  (tagged-list? exp 'set!))
(define (assignment-variable exp) (cadr exp))
(define (assignment-value exp) (caddr exp))

(define (definition? exp)
  (tagged-list? exp 'define))
(define (definition-variable exp)
  (if (symbol? (cadr exp))
    (cadr exp)
    (caadr exp)))
(define (definition-value exp)
  (if (symbol? (cadr exp))
    (caddr exp)
    (make-lambda (cdadr exp)    ; formal parameters
                 (cddr exp))))  ; body

(define (lambda? exp) (tagged-list? exp 'lambda))
(define (lambda-parameters exp) (cadr exp))
(define (lambda-body exp) (cddr exp))
(define (make-lambda params body)
  (cons 'lambda (cons params body)))

(define (if? exp) (tagged-list? exp 'if))
(define (if-predicate exp) (cadr exp))
(define (if-consequent exp) (caddr exp))
(define (if-alternative exp)
  (if (not (null? (cdddr exp)))
    (cadddr exp)
    'false))
(define (make-if predicate consequent alternative)
  (list 'if predicate consequent alternative))

(define (begin? exp) (tagged-list? exp 'begin))
(define (begin-actions exp) (cdr exp))
(define (last-exp? seq) (null? (cdr seq)))
(define (first-exp seq) (car seq))
(define (rest-exps seq) (cdr seq))

(define (sequence->exp seq)
  (cond ((null? seq) seq)
        ((last-exp? seq) (first-exp seq))
        (else (make-begin seq))))
(define (make-begin seq) (cons 'begin seq))

(define (application? exp) (pair? exp))
(define (operator exp) (car exp))
(define (operands exp) (cdr exp))
(define (no-operands? ops) (null? ops))
(define (first-operand ops) (car ops))
(define (rest-operands ops) (cdr ops))

(define (cond? exp) (tagged-list? exp 'cond))
(define (cond-clauses exp) (cdr exp))
(define (cond-else-clause? clause)
  (eq? (cond-predicate clause) 'else))
(define (cond-predicate clause) (car clause))
(define (cond-actions clause) (cdr clause))
(define (cond->if exp)
  (expand-clauses (cond-clauses exp)))
(define (extended-cond? clause)
  (eq? '=> (cadr clause)))
(define (extended-cond-action clause)
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
                 (if (extended-cond? first)
                   (list (extended-cond-action first) (cond-predicate first))
                   (sequence->exp (cond-actions first)))
                 (expand-clauses rest))))))

(define (and? exp) (tagged-list? exp 'and))
(define (and-exps exp) (cdr exp))
(define (eval-and exp env)
  (let ((exps (and-exps exp)))
    (cond ((null? exps) #t)
          ((last-exp? exps) (myeval (first-exp exps) env))
          (else
            (if (true? (myeval (first-exp exps) env))
              (eval-and (rest-exps exps) env)
              #f)))))

(define (or? exp) (tagged-list? exp 'or))
(define (or-exps exp) (cdr exp))
(define (eval-or exp env)
  (let ((exps (or-exps exp)))
    (if (null? exps)
      #f
      (let ((res (myeval (first-exp exps) env)))
        (if (true? res)
          res
          (eval-or (rest-exps exps) env))))))

(define (let? exp) (tagged-list? exp 'let))
(define (let-binds exp) (cadr exp))
(define (let-vars exp) (map car (cadr exp)))
(define (let-vals exp) (map cadr (cadr exp)))
(define (let-body exp) (cddr exp))
(define (named-let-var exp) (cadr exp))
(define (named-let-binds exp) (caddr exp))
(define (named-let-vars exp) (map car (named-let-binds exp)))
(define (named-let-vals exp) (map cadr (named-let-binds exp)))
(define (named-let-body exp) (cdddr exp))
(define (named-let-make-lambda-list exp) (cons (named-let-var exp) (named-let-vars exp)))
(define (named-let-make-lambda-call exp) (cons (named-let-var exp) (named-let-vals exp)))
(define (let->combination exp)
  (if (pair? (let-binds exp))
    (cons (make-lambda (let-vars exp) (let-body exp))
          (let-vals exp))
    (sequence->exp (list (append (list 'define (named-let-make-lambda-list exp))
                                 (named-let-body exp))
                         (named-let-make-lambda-call exp)))))

(define (let*? exp) (tagged-list? exp 'let*))
(define (let*-list exp) (cadr exp))
(define (let*-body exp) (cddr exp))
(define (let*-last? exp) (null? (cdr exp)))

(define (let*->nested-lets exp)
  (define (iter vars body)
    (if (let*-last? vars)
      (cons 'let (cons (list (car vars)) body))
      (list 'let (list (car vars))
            (iter (cdr vars) body))))
  (iter (let*-list exp) (let*-body exp)))

(define (while? exp) (tagged-list? exp 'while))
(define (while-pred exp) (cadr exp))
(define (while-body exp) (caddr exp)) ; only accepts one expression, if-style
(define (while->combination exp)
  (sequence->exp (list (list 'define
                             (list 'while-iter)
                             (make-if (while-pred exp)
                                      (while-body exp)
                                      #f))
                       (list 'while-iter))))

; 4.10
; Say we wanted a RPN-style syntax of:
;   (1 2 +) => 3
;   (4 double) => 8
; etc.
; Just updating tagged-list? to look at the end of a list fixes all the eval "thing?" tests
; And so on

(define (true? x)
  (not (eq? x false)))
(define (false? x)
  (eq? x false))

(define (make-procedure parameters body env)
  (list 'procedure parameters (scan-out-defines body) env))
(define (compound-procedure? p)
  (tagged-list? p 'procedure))
(define (procedure-parameters p) (cadr p))
(define (procedure-body p) (caddr p))
(define (procedure-environment p) (cadddr p))

(define (enclosing-environment env) (cdr env))
(define (first-frame env) (car env))
(define the-empty-environment '())

(define (make-frame variables values)
  (map cons variables values))
(define (frame-variables frame) (map car frame))
(define (frame-values frame) (map cdr frame))
(define (add-binding-to-frame! var val frame)
  (set-cdr! frame (cons (cons var val)
                    (cdr frame))))

(define (extend-environment vars vals base-env)
  (if (= (length vars) (length vals))
    (cons (make-frame vars vals) base-env)
    (if (< (length vars) (length vals))
      (error "Too many arguments supplied" vars vals)
      (error "Too few arguments supplied" vars vals))))

(define (var-in-frame? var frame)
  (define (iter lst)
    (cond ((eq? lst '()) #f)
          ((eq? (caar lst) var) (car lst))
          (else (iter (cdr lst)))))
  (iter frame))

(define (frame-for-var var env)
  (define (iter lst)
    (cond ((eq? lst '()) #f)
          ((var-in-frame? var (first-frame lst)) (first-frame lst))
          (else (iter (enclosing-environment lst)))))
  (iter env))

(define (lookup-variable-value var env)
  (let ((frame (frame-for-var var env)))
    (if frame
      (let ((val (cdr (var-in-frame? var frame))))
        (if (eq? val '*unassigned*)
          (error "value has not yet been assigned" var)
          val))
      (error "Unbound variable" var))))

(define (set-variable-value! var val env)
  (let ((frame (frame-for-var var env)))
    (if frame
      (set-cdr! (var-in-frame? var frame) val)
      (error "Unbound variable -- SET!" var))))

(define (define-variable! var val env)
  (let ((frame (first-frame env)))
    (let ((varpair (var-in-frame? var frame)))
      (if varpair
        (set-cdr! varpair val)
        (add-binding-to-frame! var val frame)))))

(define primitive-procedures
  (list (list 'car car)
        (list 'cdr cdr)
        (list 'list list)
        (list 'cons cons)
        (list 'null? null?)
        (list '+ +)
        (list '- -)
        (list '* *)
        (list '/ /)
        (list '= =)
        ;<more primitives>
        ))

(define (primitive-procedure-names)
  (map car
       primitive-procedures))

(define (primitive-procedure-objects)
  (map (lambda (proc) (list 'primitive (cadr proc)))
       primitive-procedures))

(define (setup-environment)
  (let ((initial-env
          (extend-environment (primitive-procedure-names)
                              (primitive-procedure-objects)
                              the-empty-environment)))
    (define-variable! 'true true initial-env)
    (define-variable! 'false false initial-env)
    initial-env))

(define (primitive-procedure? proc)
  (tagged-list? proc 'primitive))

(define (primitive-implementation proc) (cadr proc))

(define (apply-primitive-procedure proc args)
  (apply (primitive-implementation proc) args))

(define input-prompt ";;; M-Eval input:")
(define output-prompt ";;; M-Eval value:")
(define (driver-loop)
  (prompt-for-input input-prompt)
  (let ((input (read)))
    (let ((output (myeval input the-global-environment)))
      (announce-output output-prompt)
      (user-print output)))
  (driver-loop))

(define (prompt-for-input string)
  (newline) (newline) (display string) (newline))

(define (announce-output string)
  (newline) (display string) (newline))

(define (user-print object)
  (if (compound-procedure? object)
    (display (list 'compound-procedure
                   (procedure-parameters object)
                   (procedure-body object)
                   '<procedure-env>))
    (display object)))

(define (scan-out-defines body)
  (define (id x) x)
  (define (has-defines? body)
    (not (null? (filter id (map definition? body)))))
  (define (defs2letvars body)
    (map unassigned-arg
         (map definition-variable
              (filter definition? body))))
  (define (defs2sets body)
    (map (lambda (x) (cons 'set! (cdr x)))
         (filter definition? body)))
  (define (deflessBody body)
    (filter (lambda (x) (not (definition? x))) body))
  (if (has-defines? body)
    (list `(let ,(defs2letvars body) ,@(defs2sets body) ,@(deflessBody body)))
    body))

(define (unassigned-arg x) (list x ''*unassigned*))
(define (letrec? exp) (tagged-list? exp 'letrec))
(define (letrec-list->set exp) (map (lambda (l) (cons 'set! l)) (cadr exp)))
(define (letrec-args exp) (map car (cadr exp)))
(define (letrec-body exp) (cddr exp))

(define (letrec->recursive-lets exp)
  `(let ,(map unassigned-arg (letrec-args exp))
     ,@(letrec-list->set exp)
     ,@(letrec-body exp)))

(define the-global-environment (setup-environment))
(driver-loop)
