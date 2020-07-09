(in-package :hir-clj)


(defun class-list (object)
  (mapcan (lambda (class)
            (let ((name (class-name class)))
              (unless (member name '(standard-object t))
                (list (format nil "~(~A~)" name)))))
          (closer-mop:class-precedence-list (class-of object))))



