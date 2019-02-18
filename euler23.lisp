;;;* (time (euler23 28123))

;;;Evaluation took:
;;;  54.530 seconds of real time
;;;  54.437500 seconds of total run time (54.421875 user, 0.015625 system)
;;;  [ Run times consist of 1.120 seconds GC time, and 53.318 seconds non-GC time. ]
;;;  99.83% CPU
;;;  82,449,197,027 processor cycles
;;;  10,845,266,896 bytes consed

;;;4179871

(defun proper-divisors (p)
"proper-divisors returns the list of the proper divisors of p (3x faster than rec version, but more bytes consed"
(let ((max (floor (/ p 2)))
      (res '(1))
      (i 2))
(loop while (< i max) do
  ;(format t "~S" max)
  (if (proper-divisor-p i p)
  (progn
    (setf res (cons i res))
    (if (not (eql i (/ p i))) (setf res (cons (/ p i) res)))
    (setf max (floor (/ p i)))
    (setf i (+ 1 i)))
  (progn
    (setf max (+ (floor (/ p i) 1)))
    (setf i (+ 1 i))
    )))
res))

(defun proper-divisor-p (n p)
"proper-divisor-p returns a t boolean if n is a proper divisor of p, nil otherwise"
(if (and (< n p) (eql 0 (rem p n)))
    t
    nil
))

(defun abundant-p (p)
"abundat-p returns true if p is abundant, nil if not"
(if (and (> (reduce '+ (proper-divisors p)) p) (> p 0))
    t
    nil))

(defun abundant (p)
"abundant returns the list of abundant numbers below p, p included (twice as fast as recursive-abundant)"
(remove nil (loop for i from 1 to p
  collect (if (abundant-p i) i)))
)

(defun add-number-to-list (n l max)
"add-number-to-list returns the list of numbers that are the sum of n and all members of l which result is inferior to max"
(remove nil (loop for i from 0 to (- (length l) 1) collect (if (<= (+ n (nth i l)) max) (+ n (nth i l)))))
)

(defun two-numbers-p (p l)
"two-numbers-p returns true as soon as it finds a combination of two numbers from l which sum adds up to p"
(loop for i from 0 to (- (length l) 1) do
  (if (abundant-p (- p (nth i l)))
      (return-from two-numbers-p t))))

(defun sum-of-two-abundants (p)
"sum-of-two-abundants returns the sum of the numbers below p that can be written as the sum of 2 abundant numbers"
(let ((1-abundant (sort (abundant p) #'>))
      (agg 0))
(loop for i from 1 to p do
  (if (two-numbers-p i (member-if #'(lambda (x) (<= x i)) 1-abundant))
      (setf agg (+ i agg))
  ))
agg
))

(defun euler23 (m)
"euler23 returns the solution to ProjectEuler 23rd project for any given number (23rd m = 28123)"
(- (/ (* m (+ m 1)) 2) (sum-of-two-abundants m))
)
