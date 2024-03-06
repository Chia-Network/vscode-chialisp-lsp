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
        (assign
          (two three) (list 2 3)

          (if (odd X) ; Is it odd?
            (collatz incN (+ 1 (* three X))) ; Odd? 3 X + 1
            (collatz incN (/ X two)) ; Even? X / 2
            )
          )
        )
      )
    )
  (collatz 1 5) ; Run it
  )
