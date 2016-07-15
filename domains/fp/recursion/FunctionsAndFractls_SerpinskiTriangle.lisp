(defmacro pred (x y)
  `(lambda (trikk)
      (destructuring-bind (row column count) trikk
        (and (<= (abs (- column ,x)) (- ,y row)) (< ,y (+ row count))))))

(defmacro multiple (tril)
  `(mapcan #'(lambda (tri) (destructuring-bind (row col cou) tri
                            (let ((nc  (/ cou 2)))
                              (list (list row col nc)
                                    (list (+ row nc) (- col nc) nc)
                                    (list (+ row nc) (+ col nc) nc)))))
          ,tril))


(defmacro printtri1 ()
  (labels ((pic (tri) (apply #'concatenate
                                      (cons 'string (loop for y from 0 to 31
                                                          collect (apply #'concatenate
                                                                         (append '(string) (loop for x from 0 to 62
                                                                                             collect (if (eval (cons 'or (mapcar (pred x y) tri))) "1" "_")) '("~%"))))))))
    (cons 'list
          (let ((ret '((0 31 32))))
            (loop repeat 6
                  collect (pic ret)
                  do (setf ret (multiple ret)))))))

(format t (nth (read) (printtri1)))
