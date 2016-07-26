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

; 5.8

(define (extract-labels text receive)
  (if (null? text)
      (receive '() '())
      (extract-labels (cdr text)
       (lambda (insts labels)
         (let ((next-inst (car text)))
           (if (symbol? next-inst)
             (if (label-present? next-inst labels)
               (error "Label defined twice -- " next-inst)
               (receive insts
                        (cons (make-label-entry next-inst
                                                insts)
                              labels)))
               (receive (cons (make-instruction next-inst)
                              insts)
                        labels)))))))

(define label-present? assoc)
