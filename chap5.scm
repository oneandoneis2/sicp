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
