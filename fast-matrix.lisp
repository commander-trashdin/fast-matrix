;;;; fast-matrix.lisp

(in-package #:fast-matrix)



(defun %check-two-matrices (fst snd)
  "fst and snd are cons of a form (height . width) in this case.
   The function returns the dimensions of resulting thing and the number of
   multiplications requred. I work under assertion, that those matrices are
   actually multipliable => (= (cdr fst) (car snd))."
  (list (cons (car fst) (cdr snd)) (* (car fst) (cdr fst) (cdr snd))))

(defun %add-two-cases (fst snd)
  "Compares two cases of what previous thing returns and returns the best one."
  (let ((res (%check-two-matrices (car fst) (car snd))))
    (incf (second res) (second fst))
    (incf (second res) (second snd))
    res))

(defun %index->matrix (operation indices matrices)
  "Tree of indexes -> tree of multiplications"
  (if (atom indices)
      (elt matrices indices)
      (list operation (%index->matrix operation (car indices) matrices)
                      (%index->matrix operation (cadr indices) matrices))))


(defun %straight-forward-check (sizes)
  "Naive atomic multiplications check for comparison"
  (loop :for i :from 1 :below (length sizes)
        :with fh := (car (aref sizes 0))
        :sum (* fh (car (aref sizes i)) (cdr (aref sizes i)))))


(defun %build-tree (dimensions)
  "Builds an order tree of multiplications, dynamically solving it for all ranges."
  (let* ((n (length dimensions))
         (table (make-array (list n n))))
    (loop :for i :from 0 :below n
          :do (setf (aref table i i) (list i (aref dimensions i) 0)))
    (loop :for i :from 0 :below (1- n)
          :do (setf (aref table i (1+ i))
                    (cons (list i (1+ i)) (%check-two-matrices (cadr (aref table i i))
                                                 (cadr (aref table (1+ i) (1+ i)))))))
    (loop :for len :from 2 :below n
          :do (loop :for ind :from 0 :below (- n len)
                    :do (setf (aref table ind (+ ind len))
                              (loop :for step :from 1 :upto len
                                    :with best := (cons (list (car (aref table ind ind))
                                                              (car (aref table (1+ ind) (+ ind len))))
                                                        (%add-two-cases
                                                            (cdr (aref table ind ind))
                                                            (cdr (aref table (1+ ind) (+ ind len)))))
                                    :for next := (cons (list (car (aref table ind (+ ind (1- step))))
                                                             (car (aref table (+ ind step) (+ ind len))))
                                                       (%add-two-cases
                                                        (cdr (aref table ind (+ ind (1- step))))
                                                        (cdr (aref table (+ ind step) (+ ind len)))))
                                    :when (< (third next) (third best))
                                      :do (setf best next)
                                    :finally (return best)))))
    (aref table 0 (1- n))))


(defmacro fast-multiply (mult-function extract-dimensions &rest matrices)
  (let* ((dimensions (map 'vector extract-dimensions matrices))
         (dimension-vector (make-array (length matrices) :initial-contents dimensions))
         (naive-time (%straight-forward-check dimensions)))
    (destructuring-bind (tree size multiplications) (%build-tree dimension-vector)
      (format t "The resulting matrix dimensions: ~s~%Required atomic multiplications: ~s~%Compared to: ~s multiplications in naive implementation~%Acceleration ratio is ~s~%"
              size multiplications naive-time (float (/ naive-time multiplications)))
      (%index->matrix mult-function tree matrices))))

