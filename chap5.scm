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
