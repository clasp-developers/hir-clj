(in-package :hir-clj)


(defparameter *default-graph-style* nil)

(defun load-graph-style (name)
  (alexandria:read-file-into-string (asdf:component-pathname (asdf:find-component :hir-clj (list "styles" (format nil "~A.css" name))))))

(defun set-graph-style (name)
  (setq *default-graph-style* (load-graph-style name))
  (values))

(set-graph-style "graphviz")


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


(defclass container ()
  ((object
     :reader object
     :initarg :object)
   (visible
     :accessor visible
     :initform nil)
   (children
     :accessor children
     :initform nil)
   (proxies
     :accessor proxies
     :initform nil)))


(defun container-toggle (instance)
  (with-slots (children proxies visible) instance
    (dolist (element proxies)
      (setf (cytoscape:removed element) visible))
    (setf visible (not visible))
    (dolist (element children)
      (setf (cytoscape:removed element) visible))))


(defclass hir-graph ()
  ((cytoscape
     :accessor cytoscape
     :initform (make-instance 'cytoscape:cytoscape-widget
                               :graph-layouts (list (make-instance 'cytoscape:dagre-layout :align "L"))
                               :graph-style *default-graph-style*
                               :layout (make-instance 'jupyter-widgets:layout :width "auto" :height "2000px")))
   (object-to-id
     :accessor object-to-id
     :initform (make-hash-table :test #'eq))
   (id-to-object
     :accessor ids
     :initform (make-hash-table :test #'equal))
   (object-to-proxy
     :accessor object-to-proxy
     :initform (make-hash-table :test #'eq))))


(defun id-of-object (graph object)
  (or (gethash object (object-to-id graph))
      (setf (gethash object (object-to-id graph)) (jupyter:make-uuid))))


(defun object-of-id (graph id)
  (gethash id (id-to-object graph)))


(defun proxy-of-object (graph object &optional create)
  (or (gethash object (object-to-proxy graph))
      (and create
           (setf (gethash object (object-to-proxy graph)) (make-instance 'container :object object)))))


(defun set-graph-style (graph name)
  (setf (cytoscape:graph-style (cytoscape graph)) (load-graph-style name)))


(defgeneric make-node (graph object owner proxy))


(defgeneric make-edges (graph object))


(defmethod make-node (graph object owner proxy)
  (let ((data (list (cons "id" (id-of-object graph object))
                    (cons "label" (cleavir-ir-graphviz:label object)))))
    (cond
      (proxy
        (push (cons "parent" (id-of-object graph proxy)) data))
      (owner
        (push (cons "parent" (id-of-object graph owner)) data)))
    (make-instance 'cytoscape:element
                   :group "nodes"
                   :data data
                   :classes (class-list object))))


(defmethod make-edges (graph object))


(defmethod make-edges (graph (object cleavir-ir:instruction))
  (let (elements)

    (do ((successors (cleavir-ir:successors object) (cdr successors))
         (i 0 (1+ i)))
        ((null successors))
      (push (make-instance 'cytoscape:element
                           :group "edges"
                           :data (list (cons "id" (jupyter:make-uuid))
                                       (cons "source" (id-of-object graph object))
                                       (cons "target" (id-of-object graph (car successors)))
                                       (cons "label" (write-to-string i)))
                           :classes (list "successor"))
            elements))

    (do ((inputs (cleavir-ir:inputs object) (cdr inputs))
         (i 0 (1+ i)))
        ((null inputs))
      (push (make-instance 'cytoscape:element
                           :group "edges"
                           :data (list (cons "id" (jupyter:make-uuid))
                                       (cons "source" (id-of-object graph (car inputs)))
                                       (cons "target" (id-of-object graph object))
                                       (cons "label" (cleavir-ir-graphviz:input-label object (car inputs) i)))
                           :classes (list "input"))
            elements))

    (do ((outputs (cleavir-ir:outputs object) (cdr outputs))
         (i 0 (1+ i)))
        ((null outputs))
      (push (make-instance 'cytoscape:element
                           :group "edges"
                           :data (list (cons "id" (jupyter:make-uuid))
                                       (cons "source" (id-of-object graph object))
                                       (cons "target" (id-of-object graph (car outputs)))
                                       (cons "label" (cleavir-ir-graphviz:output-label object (car outputs) i)))
                           :classes (list "output"))
            elements))

    elements))


(defmethod make-edges (graph (object cleavir-ir:enclose-instruction))
  (append (list (make-instance 'cytoscape:element
                               :group "edges"
                               :data (list (cons "id" (jupyter:make-uuid))
                                           (cons "source" (id-of-object graph (cleavir-ir:code object)))
                                           (cons "target" (id-of-object graph object)))
                               :classes (list "code")))
          (call-next-method)))


(defmethod make-edges (graph (object cleavir-ir:unwind-instruction))
  (cons (make-instance 'cytoscape:element
                       :group "edges"
                       :data (list (cons "id" (jupyter:make-uuid))
                                   (cons "source" (id-of-object graph (cleavir-ir:destination object)))
                                   (cons "target" (id-of-object graph object)))
                       :classes (list "destination"))
        (call-next-method)))


(defun make-hir (form &optional direct)
  (cond
    (direct
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
        result))
      (t
        (let ((clasp-cleavir::*save-hir* t))
          (compile 'nil form)
          (unless clasp-cleavir::*hir*
            (error "Could not grab hir"))
          clasp-cleavir::*hir*))))


(defun parent (el)
  (cdr (assoc "parent" (cytoscape:data el) :test #'equal)))


(defun id (el)
  (cdr (assoc "id" (cytoscape:data el) :test #'equal)))


(defun generation-sort (elements)  (prog (results younger-nodes
         (nodes (mapcan (lambda (el)
                          (when (equal "nodes" (cytoscape:group el))
                            (list el)))
                        elements)))
   next
    (when (null nodes)
      (return (nconc (nreverse results)
                     (mapcan (lambda (el)
                               (when (equal "edges" (cytoscape:group el))
                                 (list el)))
                             elements))))
    (setq younger-nodes nil)
    (dolist (node nodes)
      (if (or (null (parent node))
              (member (parent node) results :key #'id :test #'equal))
        (push node results)
        (push node younger-nodes)))
    (setq nodes younger-nodes)
    (go next)))


(defun graph-form (graph form &key show-owners show-basic-blocks direct)
  (let* ((initial-instruction (make-hir form direct))
         (owners (make-hash-table :test #'eq))
         (start-id (jupyter:make-uuid))
         (elements (list (make-instance 'cytoscape:element
                                        :group "nodes"
                                        :data (list (cons "id" start-id)
                                                    (cons "label" "START"))
                                        :classes (list "start"))
                         (make-instance 'cytoscape:element
                                        :group "edges"
                                        :data (list (cons "source" start-id)
                                                    (cons "target" (id-of-object graph initial-instruction)))
                                        :classes (list "start")))))

    (when show-owners
      (cleavir-ir:map-instructions-with-owner
        (lambda (instruction owner &aux proxy)
          (when owner
            (setq proxy (proxy-of-object graph owner t))
            (unless (gethash instruction owners)
              (setf (gethash instruction owners) proxy))
            (dolist (datum (append (cleavir-ir:inputs instruction)
                                   (cleavir-ir:outputs instruction)))
              (unless (gethash datum owners)
                (setf (gethash datum owners) proxy)))))
        initial-instruction))

    (when show-basic-blocks
      (dolist (basic-block (cleavir-basic-blocks:basic-blocks initial-instruction))
        (let ((container (make-instance 'container :object basic-block))
              (owner (cleavir-basic-blocks:owner basic-block)))
          (when (and show-owners owner)
            (setf (gethash container owners)
                  (or (proxy-of-object graph owner) owner)))
          (cleavir-basic-blocks:map-basic-block-instructions
            (lambda (instruction)
              (setf (gethash instruction owners) container))
            basic-block))))

    (cleavir-ir:map-instructions-with-owner
      (lambda (instruction owner)
        (setq elements (nconc elements (make-edges graph instruction))))
      initial-instruction)

    (maphash (lambda (object id)
               (push (make-node graph object (gethash object owners) (proxy-of-object graph object)) elements))
             (object-to-id graph))

    (setf (cytoscape:elements (cytoscape graph))
          (nconc (cytoscape:elements (cytoscape graph))
                 (generation-sort elements))))) 
