;;;;There is necessarily this cycle because rational number (p / q form)
;;;;* (time (euler26 1000))

;;;;Evaluation took:
;;;;  31.820 seconds of real time
;;;;  31.718750 seconds of total run time (31.703125 user, 0.015625 system)
;;;;  [ Run times consist of 0.032 seconds GC time, and 31.687 seconds non-GC time. ]
;;;;  99.68% CPU
;;;;  48,112,155,450 processor cycles
;;;;  352,200,576 bytes consed

;;;;983



(defun first-decimal (n p)
"first-decimal returns a list of the first non zero number of (/ n p) and 10 times the the remainder of this integer division"
(list (parse-integer (subseq (write-to-string (coerce (/ n p) 'double-float)) 0 1)) (* 10 (rem n p))))

(defun recurring-cycle-length-in-list (l)
"recurring-cycle-length-in-list gives the recurring cycle length in a list of digit"
(cond
  ((eql l nil)
    0)
  ((position (car l) (cdr l))
    (+ 1 (position (car l) (cdr l))))
  (t
    (recurring-cycle-length-in-list (cdr l)))
))

(defun recurring-cycle-length (p)
"recurring-cycle-length returns the length of the recurring cycle in the list of digit l"
(let* (
      (l (list 1))
      (f (first-decimal 10 p))
      (r (recurring-cycle-length-in-list l)))
(loop while (eql r 0) do
  (setf f (first-decimal (nth 1 f) p))
  (setf l (append l (list (nth 1 f))))
  (setf r (recurring-cycle-length-in-list l))
)
r))

(defun index-of-max (l)
"index-of-max returns the index of the max number in a list"
(let ((a (car l))
      (j 0))
(loop for i from 1 to (- (length l) 1) do
(if (< a (nth i l))
(progn
  (setf a (nth i l))
  (setf j i))
))
(+ 1 j)))

(defun euler26 (d)
"euler26 returns the solution of projecteuler problem number 26, for numbers < d"
(let ((l (loop for i from 1 to d
  collect (recurring-cycle-length i))))
(index-of-max l)
))  
