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
  (let ((proxy (gethash object (object-to-proxy graph))))
    (cond
      (proxy proxy)
      (create
        (setf (gethash object (object-to-proxy graph)) (make-instance 'container :object object))))))


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


(defun generation-sort (elements)
  (prog (results younger-nodes
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
    (format t "~A~%" (mapcar #'cytoscape:data nodes))
    (finish-output)
    (dolist (node nodes)
      (if (or (null (parent node))
              (member (parent node) results :key #'id :test #'equal))
        (push node results)
        (push node younger-nodes)))
    (cond
      ((= (length nodes) (length younger-nodes))
        (write-line "bail")
        (setq results (nconc younger-nodes results))
        (setq nodes nil))
      (t
        (setq nodes younger-nodes)))
    (go next)))

(defun item-id (item hash-table)
  (or (gethash item hash-table)
      (setf (gethash item hash-table) (jupyter:make-uuid))))

(defun graph-form (graph form &key direct)
  (let* ((initial-instruction (make-hir form direct))
         (node-ids (make-hash-table :test #'eq))
         (parent-ids (make-hash-table :test #'eq))
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
                                                    (cons "target" (item-id initial-instruction node-ids)))
                                        :classes (list "start")))))


    (write-line "Assign owners and index instructions and datums")
    (finish-output)

    (cleavir-ir:map-instructions-with-owner
      (lambda (instruction owner &aux (parent-id (item-id owner parent-ids)))
        (setf (gethash instruction owners) parent-id)
        (item-id instruction node-ids)
        (dolist (datum (append (cleavir-ir:inputs instruction)
                               (cleavir-ir:outputs instruction)))
          (item-id datum node-ids)
          (unless (gethash datum owners)
            (setf (gethash datum owners) parent-id))))
      initial-instruction)

    (write-line "add basic blocks")
    (finish-output)

    (let ((datum-blocks (make-hash-table :test #'eq)))
      (dolist (basic-block (cleavir-basic-blocks:basic-blocks initial-instruction))
        (unless (typep (cleavir-basic-blocks:first-instruction basic-block) 'cleavir-ir:enter-instruction)
          (let ((parent-id (item-id basic-block parent-ids))
                (owner (cleavir-basic-blocks:owner basic-block)))
            (when owner
              (setf (gethash basic-block owners)
                    (item-id owner parent-ids)))
            (cleavir-basic-blocks:map-basic-block-instructions
              (lambda (instruction)
                (setf (gethash instruction owners) parent-id)
                (dolist (datum (cleavir-ir:outputs instruction))
                  (setf (gethash datum datum-blocks)
                        (if (gethash datum datum-blocks) t parent-id))))
              basic-block))))
      (maphash (lambda (datum owner)
                 (unless (eql t owner)
                   (setf (gethash datum owners) owner)))
               datum-blocks))

    (maphash (lambda (object id &aux (data (list (cons "id" id)
                                                 (cons "label" (cleavir-ir-graphviz:label object))))
                                     (parent-id (gethash object owners))
                                     (classes (class-list object)))
               (when parent-id
                 (push (cons "parent" parent-id) data))
               (when (gethash object parent-ids)
                 (push "proxy" classes))
               (push (make-instance 'cytoscape:element
                                    :groups "nodes"
                                    :data data
                                    :classes classes)
                     elements)

               (when (typep object 'cleavir-ir:enclose-instruction)
                  (push (make-instance 'cytoscape:element
                               :group "edges"
                               :data (list (cons "id" (jupyter:make-uuid))
                                           (cons "source" (gethash (cleavir-ir:code object) node-ids))
                                           (cons "target" id))
                               :classes (list "code"))
                        elements))

               (when (typep object 'cleavir-ir:unwind-instruction)
                  (push (make-instance 'cytoscape:element
                               :group "edges"
                               :data (list (cons "id" (jupyter:make-uuid))
                                           (cons "source" (gethash (cleavir-ir:destination object) node-ids))
                                           (cons "target" id))
                               :classes (list "destination"))
                        elements))

               (when (typep object 'cleavir-ir:instruction)
                 (do ((successors (cleavir-ir:successors object) (cdr successors))
                      (i 0 (1+ i)))
                     ((null successors))
                   (push (make-instance 'cytoscape:element
                                        :group "edges"
                                        :data (list (cons "id" (jupyter:make-uuid))
                                                    (cons "source" id)
                                                    (cons "target" (gethash (car successors) node-ids))
                                                    (cons "label" (write-to-string i)))
                                        :classes (list "successor"))
                         elements))

                 (do ((inputs (cleavir-ir:inputs object) (cdr inputs))
                      (i 0 (1+ i)))
                     ((null inputs))
                   (push (make-instance 'cytoscape:element
                                        :group "edges"
                                        :data (list (cons "id" (jupyter:make-uuid))
                                                    (cons "source" (gethash (car inputs) node-ids))
                                                    (cons "target" id)
                                                    (cons "label" (cleavir-ir-graphviz:input-label object (car inputs) i)))
                                        :classes (list "input"))
                         elements))

                 (do ((outputs (cleavir-ir:outputs object) (cdr outputs))
                      (i 0 (1+ i)))
                     ((null outputs))
                   (push (make-instance 'cytoscape:element
                                        :group "edges"
                                        :data (list (cons "id" (jupyter:make-uuid))
                                                    (cons "source" id)
                                                    (cons "target" (gethash (car outputs) node-ids))
                                                    (cons "label" (cleavir-ir-graphviz:output-label object (car outputs) i)))
                                        :classes (list "output"))
                         elements))))
             node-ids)

    (maphash (lambda (object id &aux (data (list (cons "id" id)
                                                 (cons "label" (cleavir-ir-graphviz:label object))))
                                     (parent-id (gethash object owners)))
               (unless (or (null parent-id)
                           (equal id parent-id))
                 (push (cons "parent" parent-id) data))
               (push (make-instance 'cytoscape:element
                                    :groups "nodes"
                                    :data data
                                    :classes (cons "parent" (class-list object)))
                     elements))
                     parent-ids)



    (setf (cytoscape:elements (cytoscape graph))
          (nconc (cytoscape:elements (cytoscape graph))
                 (generation-sort elements)))))

(defmethod initialize-instance :after ((graph hir-graph) &rest initargs &key &allow-other-keys)
  (let ((form (getf initargs :form)))
    (when form
      (graph-form graph form))))
