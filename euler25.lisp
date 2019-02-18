;;;;* (time (euler25 1000))

;;;;Evaluation took:
;;;;  1.121 seconds of real time
;;;;  1.125000 seconds of total run time (1.093750 user, 0.031250 system)
;;;;  [ Run times consist of 0.171 seconds GC time, and 0.954 seconds non-GC time. ]
;;;;  100.36% CPU
;;;;  1,694,765,289 processor cycles
;;;;  2,073,976,016 bytes consed

;;;;4781

(defun recursive-fibonacci (n)
"recursive-fibonacci returns the nth index of the Fibonacci sequence (somehow highly inefficient, 6min for recursive-fibonacci 50)"
(cond
  ((eql n 0)
    1)
  ((eql n 1)
    1)
  (t
    (+ (recursive-fibonacci (- n 1)) (recursive-fibonacci (- n 2))))
))

(defun fibonacci (n)
"fibonacci is a fast implementation to get to the nth fibonacci number"
(let ((l '(1 1)))
(loop for i from 1 to (- n 2)
  do
    (setf l (cons (+ (nth 0 l) (nth 1 l)) l))

)
(car l)))

(defun number-of-digit (n)
"number-of-digit returns the number of digit in number n"
(length (write-to-string n))
)

(defun euler25 (p)
"euler25 returns the index of the smallest Fibonacci number that has p digit"
(let ((i 0))
(loop while (< (number-of-digit (fibonacci i)) p) do
  (setf i (+ i 1))
)
i))
