;* (time (euler24 1000000 9))

;Evaluation took:
;  0.000 seconds of real time
;  0.000000 seconds of total run time (0.000000 user, 0.000000 system)
;  100.00% CPU
;  120,666 processor cycles
;  32,768 bytes consed

;(2 7 8 3 9 1 5 4 6 0)


(defun factorial (n)
"factorial recursively returns n!"
(cond
((eql 0 n) 1)
(t (* n (factorial ( - n 1))))
))

(defun factorial-decomposition (n m)
"factorial-decomposition decomposes number n in base factorial up to m!"
(cond
  ((< m 0)
    '())
  (t
    (cons (floor (/ n (factorial m))) (factorial-decomposition (rem n (factorial m)) (- m 1)))
)))

(defun increment (l1 l2)
"increment takes l1 list of increments to implement and l2 the sorted list of numbers in a base (0 1 2 3 4 5 6 7 8 9) for instance and increments it accordingly"
(cond
  ((eql (length l1) 1)
    (cons (nth (nth 0 l1) l2) '()))
  (t
    (cons (nth (nth 0 l1) l2) (increment (cdr l1) (remove (nth (nth 0 l1) l2) l2))))
))

(defun euler24 (n m)
"euler24 returns the nth permutations of 0123...m  (lexicographic order)"
(let ((a (factorial-decomposition (- n 1) m));(- n 1) because the nth permutation is of index n-1 in the order of the permutations starting with 0
      (b (loop for i from 0 to m collect i)))
(increment a b)
)) 
