;;;; fast-matrix.lisp

(in-package #:fast-matrix)


(defun %check-two-matrices (first second)
  #.(format
     nil "~@{~A~%~}"
     "first and second are cons of a form (height width) in this case."
     "The function returns the dimensions of resulting thing and the number of"
     "multiplications requred. I work under assertion, that those matrices are"
     "actually multipliable => (= (cdr first) (car second)).")
  (destructuring-bind (first-height first-width) first
    (destructuring-bind (sec-height sec-width) second
      (list (list first-height sec-width) (* first-width sec-height sec-width)))))

(defun %add-two-cases (first second)
  "Compares two cases of what previous thing returns and returns the best one."
  (destructuring-bind (first-matrix first-mult) first
    (destructuring-bind (sec-matrix sec-mult) second
      (let ((res (%check-two-matrices first-matrix sec-matrix)))
        (incf (second res) first-mult)
        (incf (second res) sec-mult)
        res))))


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
        :sum (* fh (car (aref sizes i)) (cadr (aref sizes i)))))


(defun %build-tree (dimensions)
  "Builds an order tree of multiplications, dynamically solving it for all ranges."
  (let* ((n (length dimensions))
         (table (make-array (list n n))))
    (loop :for i :from 0 :below n
          :do (setf (aref table i i) (list i (aref dimensions i) 0)))
    (loop :for i :from 0 :below (1- n)
          :do (setf (aref table i (1+ i))
                    (cons (list i (1+ i)) (%check-two-matrices (second (aref table i i))
                                                               (second (aref table (1+ i) (1+ i)))))))
    (loop :for len :from 2 :below n
          :do (loop :for ind :from 0 :below (- n len)
                    :do (setf (aref table ind (+ ind len))
                              (loop :for step :from 1 :upto len
                                    :with best := (cons (list (first (aref table ind ind))
                                                              (first (aref table (1+ ind) (+ ind len))))
                                                        (%add-two-cases
                                                            (cdr (aref table ind ind))
                                                            (cdr (aref table (1+ ind) (+ ind len)))))
                                    :for next := (cons (list (first (aref table ind (+ ind (1- step))))
                                                             (first (aref table (+ ind step) (+ ind len))))
                                                       (%add-two-cases
                                                        (cdr (aref table ind (+ ind (1- step))))
                                                        (cdr (aref table (+ ind step) (+ ind len)))))
                                    :when (< (third next) (third best))
                                      :do (setf best next)
                                    :finally (return best)))))
    (aref table 0 (1- n))))

(defmacro matmul-fast (&rest matrices)
  `,(reduce (lambda (acc m)
              (list 'numcl:matmul acc m))
            (cdr matrices)
            :initial-value (car matrices)))

(define-compiler-macro matmul-fast (&whole form &rest arg-list)
  (if (= 2 (length arg-list))
      `(matmul-fast ,@arg-list)
      (let* ((dimension-vector (map 'vector #'array-dimensions arg-list))
             (tree (car (%build-tree dimension-vector))))
        (loop :for pair-1 :across dimension-vector
              :for pair-2 :across (subseq dimension-vector 1)
              :do (assert (= (second pair-1)
                             (first pair-2))
                          nil
                          "Illegal array dimensions for multiplication! Can't multiply ~A by ~A"
                          pair-1 pair-2))
        (labels ((process-tree (tree)
                   (cond ((atom tree) (nth tree arg-list))
                         ((consp tree)
                          (assert (= 2 (length tree)) nil "Tree must be binary")
                          (list 'numcl:matmul
                                (process-tree (first tree))
                                (process-tree (second tree)))))))
          (process-tree tree)))))
