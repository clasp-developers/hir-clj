(in-package :hir-clj)


(defparameter *object-ids* (make-hash-table :test #'eq))


(defun object-id (object)
  (or (gethash object *object-ids*)
      (setf (gethash object *object-ids*) (jupyter:make-uuid))))


(defparameter *graph-style* nil)


(defun set-graph-style (name)
  (setq *graph-style* (alexandria:read-file-into-string (asdf:component-pathname (asdf:find-component :hir-clj (list "styles" (format nil "~A.css" name))))))
  (values))


(set-graph-style "graphviz")


(defgeneric make-node (object))


(defgeneric make-edges (object))


(defmethod cleavir-ir-graphviz:label ((object cleavir-ir:constant-input))
  (cleavir-ir:value object))


(defmethod cleavir-ir-graphviz:label ((object cleavir-ir:lexical-location))
  (cleavir-ir:name object))


(defmethod cleavir-ir-graphviz:label ((object cleavir-ir:values-location))
  "V")


(defmethod cleavir-ir-graphviz:label ((object cleavir-ir:immediate-input))
  (cleavir-ir:value object))


(defmethod cleavir-ir-graphviz:label ((object cleavir-ir:load-time-value-input))
  (cleavir-ir:form object))


(defun class-list (object)
  (mapcan (lambda (class)
            (let ((name (class-name class)))
              (unless (member name '(standard-object t))
                (list (format nil "~(~A~)" name)))))
          (closer-mop:class-precedence-list (class-of object))))


(defmethod make-node (object)
  (make-instance 'cytoscape:element
                 :group "nodes"
                 :data (list (cons "id" (object-id object))
                             (cons "label" (cleavir-ir-graphviz:label object)))
                 :classes (class-list object)))


(defmethod make-edges (object))


(defmethod make-edges ((object cleavir-ir:instruction))
  (let (elements)

    (do ((successors (cleavir-ir:successors object) (cdr successors))
         (i 0 (1+ i)))
        ((null successors))
      (push (make-instance 'cytoscape:element
                           :group "edges"
                           :data (list (cons "source" (object-id object))
                                       (cons "target" (object-id (car successors)))
                                       (cons "target_label" (write-to-string i)))
                           :classes (list "successor"))
            elements))

    (do ((inputs (cleavir-ir:inputs object) (cdr inputs))
         (i 0 (1+ i)))
        ((null inputs))
      (push (make-instance 'cytoscape:element
                           :group "edges"
                           :data (list (cons "source" (object-id (car inputs)))
                                       (cons "target" (object-id object))
                                       (cons "target_label" (cleavir-ir-graphviz:input-label object (car inputs) i)))
                           :classes (list "input"))
            elements))

    (do ((outputs (cleavir-ir:outputs object) (cdr outputs))
         (i 0 (1+ i)))
        ((null outputs))
      (push (make-instance 'cytoscape:element
                           :group "edges"
                           :data (list (cons "source" (object-id object))
                                       (cons "target" (object-id (car outputs)))
                                       (cons "source_label" (cleavir-ir-graphviz:output-label object (car outputs) i)))
                           :classes (list "output"))
            elements))

    elements))


(defmethod make-edges ((object cleavir-ir:enclose-instruction))
  (append (list (make-instance 'cytoscape:element
                               :group "edges"
                               :data (list (cons "source" (object-id (cleavir-ir:code object)))
                                           (cons "target" (object-id object)))
                               :classes (list "code")))
          (call-next-method)))


(defmethod make-edges ((object cleavir-ir:unwind-instruction))
  (cons (make-instance 'cytoscape:element
                       :group "edges"
                       :data (list (cons "source" (object-id (cleavir-ir:destination object)))
                                   (cons "target" (object-id object)))
                       :classes (list "destination"))
        (call-next-method)))


(defmethod cytoscape:add-graph (widget (initial-instruction cleavir-ir:instruction))
  (let* ((*object-ids* (make-hash-table :test #'eq))
         (start-id (jupyter:make-uuid))
         (elements (list (make-instance 'cytoscape:element
                                        :group "nodes"
                                        :data (list (cons "id" start-id)
                                                    (cons "label" "START"))
                                        :classes (list "start"))
                         (make-instance 'cytoscape:element
                                        :group "edges"
                                        :data (list (cons "source" start-id)
                                                    (cons "target" (object-id initial-instruction)))
                                        :classes (list "start")))))

    (cleavir-ir:map-instructions-arbitrary-order
      (lambda (instruction)
        (setq elements (nconc elements (make-edges instruction))))
      initial-instruction)

    (maphash (lambda (object id)
               (push (make-node object) elements))
             *object-ids*)

    (setf (cytoscape:elements widget)
          (nconc (cytoscape:elements widget) elements))))


(defun hir-graph (&rest instructions)
  (let ((widget (make-instance 'cytoscape:cytoscape-widget
                               :graph-layouts (list (make-instance 'cytoscape:dagre-layout))
                               :graph-style *graph-style*
                               :layout (make-instance 'jupyter-widgets:layout :width "auto" :height "2000px"))))
    (dolist (instruction instructions)
      (cytoscape:add-graph widget instruction))
    widget))


(defun draw-form-hir2 (form)
  "Generate a HIR graph for the form using the cst compiler"
  (let (result)
    (cmp::with-compiler-env ()
      (let* ((module (cmp::create-run-time-module-for-compile)))
        ;; Link the C++ intrinsics into the module
        (let* ((cleavir-cst-to-ast:*compiler* 'cl:compile)
               (cst (cst:cst-from-expression form))
               (ast (cleavir-cst-to-ast:cst-to-ast cst nil clasp-cleavir::*clasp-system*))
               (hoisted-ast (clasp-cleavir::hoist-ast ast))
               (hir (clasp-cleavir::ast->hir hoisted-ast)))
          (setq result hir)
          (cmp::llvm-create-module "foo"))))
    (hir-graph result)))


(defun draw-form-hir (form)
  "Generate a HIR graph for the form using the cst compiler"
  (let ((clasp-cleavir::*save-hir* t))
    (compile 'nil form)
    (let ((hir clasp-cleavir::*hir*))
      (unless hir
        (error "Could not grab hir"))
      (hir-graph hir))))
