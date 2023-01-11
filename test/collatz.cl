(mod (A) ;;; COLLATZ conjecture

  ;; set language standard
  (include *standard-cl-22*)
  (include test-inc.clsp)

  ;; Determine if number is odd
  ;; test.
  (defun-inline odd (X) (logand X 1))
  ;; Actual collatz function
  ;; determines number of step til 1
  (defun collatz (N X)
    (if (= X 1) ; We got 1
      N ; Return the number of steps
      (let ((incN (+ N 1))) ; Next N
        (if (odd X) ; Is it odd?
          (collatz incN (+ 1 (* 3 X))) ; Odd? 3 X + 1
          (collatz incN (/ X 2)) ; Even? X / 2
          )
        )
      )
    )
  (collatz 1 5) ; Run it
  )
