; 5.2
; Factorial via register-machine language
(controller
  (assign prod (const 1))
  (assign count (const 1))
  test-b
      (test (op >) (reg count) (reg n))
      (branch (label fact-done))
      (assign tmp (op *) (reg prod) (reg count))
      (assign prod (reg tmp))
      (assign tmp (op +) (reg count) (const 1))
      (assign count (reg tmp))
      (goto (label test-b))
  fact-done)

; 5.11
; a
; Initial code:
 afterfib-n-2                         ; upon return, val contains Fib(n - 2)
   (assign n (reg val))               ; n now contains Fib(n - 2)
   (restore val)                      ; val now contains Fib(n - 1)
   (restore continue)
   (assign val                        ;  Fib(n - 1) +  Fib(n - 2)
           (op +) (reg val) (reg n))

; Changed code:
 afterfib-n-2                         ; upon return, val contains Fib(n - 2)
   (restore n)                        ; n now contains Fib(n - 1)
   (restore continue)
   (assign val                        ;  Fib(n - 2) +  Fib(n - 1)
           (op +) (reg val) (reg n))

; 5.21 a
(define (count-leaves tree)
  (cond ((null? tree) 0)
        ((not (pair? tree)) 1)
        (else (+ (count-leaves (car tree))
                 (count-leaves (cdr tree))))))

(define count-leaves-machine
  (make-machine ; assuming the dynamic register allocation from 5.13
    (list (list 'null? null?) (list 'pair? pair?) (list '+ +) (list 'car car) (list 'cdr cdr))
    '(
      (assign val (const 0))
      (assign continue (label count-done))
      count
          (test (op null?) (reg tree))  ; Handle null
          (branch (label null-tree))
          (test (op pair?) (reg tree))
          (branch (label count-pair))   ; Handle pair
          (assign val (const 1))        ; Handle leaf
          (goto (reg continue))
      count-pair
      ; If it's a pair, total the car then continue to the cdr
          (save continue)
          (save tree)
          (assign continue (label count-cdr))
          (assign tree (op car) (reg tree))
          (goto (label count))
      count-cdr
      ; Handle cdr by saving current tree then summing the subtree
          (restore tree)
          (save val)
          (assign tree (op cdr) (reg tree))
          (assign continue (label after-pair))
          (goto (label count))
      after-pair
      ; Get the cdr value into var so we can restore val, then sum them
          (assign var (reg val))
          (restore val)
          (assign val (op +) (reg val) (reg var))
          (restore continue)
          (goto (reg continue))
      null-tree
          (assign val (const 0))
          (goto (reg continue))
      count-done)))

; 5.21 b
(define (count-leaves tree)
  (define (count-iter tree n)
    (cond ((null? tree) n)
          ((not (pair? tree)) (+ n 1))
          (else (count-iter (cdr tree)
                            (count-iter (car tree) n)))))
  (count-iter tree 0))

(define count-iter-tree-machine
  (make-machine
    (list (list '+ +) (list 'null? null?) (list 'pair? pair?) (list 'car car) (list 'cdr cdr))
    '((assign n (const 0))
      (assign continue (label count-leaves-done))
      tree-loop
        (test (op null?) (reg tree))
        (branch (label null-tree))
        (test (op pair?) (reg tree))
        (branch (label start-iter))
        (assign n (op +) (reg n) (const 1))
        (goto (reg continue))
      start-iter
        (save continue)
        (save tree)
        (assign continue (label sum-cdr))
        (assign tree (op car) (reg tree))
        (goto (label tree-loop))
      sum-cdr
        (restore tree)
        (assign continue (label after-pair))
        (assign tree (op cdr) (reg tree))
        (goto (label tree-loop))
      after-pair
        (restore continue)
        (goto (reg continue))
      null-tree
        (goto (reg continue))
      count-leaves-done)))

; 5.22
(define (append x y)
  (if (null? x)
    y
    (cons (car x) (append (cdr x) y))))

(define append-machine
  (make-machine
    (list (list 'car car) (list 'cdr cdr) (list 'cons cons) (list 'null? null?))
    '(
       (assign continue (label append-done))
      begin-loop
       (test (op null?) (reg x))
       (branch (label reached-null))
       (assign val (op car) (reg x))
       (assign x (op cdr) (reg x))
       (save continue)
       (save val)
       (assign continue (label do-cons))
       (goto (label begin-loop))
      do-cons
       (restore val)
       (restore continue)
       (assign y (op cons) (reg val) (reg y))
       (goto (reg continue))
      reached-null
       (goto (reg continue))
      append-done)))

(define (append! x y)
  (set-cdr! (last-pair x) y)
  x)

(define append!-machine
  (make-machine
    (list (list 'car car) (list 'cdr cdr) (list 'cons cons) (list 'set-cdr! set-cdr!) (list 'null? null?))
    '(
       (assign continue (label append-done))
       (assign list-val (reg x))
      begin-loop
       (assign cdr-val (op cdr) (reg list-val))
       (test (op null?) (reg cdr-val))
       (branch (label reached-null))
       (assign list-val (op cdr) (reg list-val))
       (goto (label begin-loop))
      reached-null
       (perform (op set-cdr!) (reg list-val) (reg y))
      append-done)))
