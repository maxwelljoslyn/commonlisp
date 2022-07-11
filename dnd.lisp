;; var vs param?
(defvar character-classes '(warrior wizard))
(defvar actions '(attack cast-spell move))
(defvar locations '(house lawn))



(defun has-exits (location) t)

(defun available-actions (class location)
  (let ((result '(attack)))
    (when (equal class 'wizard)
      (pushnew 'cast-spell result ))
    (when (has-exits location)
      (pushnew 'move result ))
    result))

(setf (symbol-function 'bazooper)
      #'(lambda (x) (cons x '(a b c))))

(mapcar #'bazooper '(1 2 3  ))
(apply #'bazooper '(1))
(funcall #'bazooper 1 30 199 39)

;; sort is destructive...
;; (remove-if #'(lambda (x) (oddp (floor x))) (sort (mapcar #'(lambda (x) (/ x 3)) '(1 30 199 18 11 99 39)) #'<))

(defun mj/remove-if (fn somelist)
  (if (null somelist)
      nil
      (let ((each (first somelist)))
        (if each
            (cons each (mj/remove-if (fn (rest somelist))))
            (mj/remove-if (fn (rest somelist)))))))

(let ((balls '(1 2 3 11 12 13)))
  (equal (remove-if #'(lambda (x) (> x 10)) balls)
         (mj/remove-if #'(lambda (x) (> x 10)) balls)))
