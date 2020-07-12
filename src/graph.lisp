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
                               :layout (make-instance 'jupyter-widgets:layout :height "800px")))
   (expand-command
     :accessor expand-command
     :initform (make-instance 'cytoscape:menu-command
                              :content "<span class='fa fa-expand fa-2x'></span>"))
   (collapse-command
     :accessor collapse-command
     :initform (make-instance 'cytoscape:menu-command
                              :content "<span class='fa fa-compress fa-2x'></span>"))
   (visibility-state
     :accessor visibility-state
     :initform 0)
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


(defun index (el)
  (cdr (assoc "index" (cytoscape:data el) :test #'equal)))


(defun source (el)
  (cdr (assoc "source" (cytoscape:data el) :test #'equal)))


(defun mask (el)
  (cdr (assoc "mask" (cytoscape:data el) :test #'equal)))


(defun state (el)
  (cdr (assoc "state" (cytoscape:data el) :test #'equal)))


(defun target (el)
  (cdr (assoc "target" (cytoscape:data el) :test #'equal)))


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
    (dolist (node nodes)
      (if (or (null (parent node))
              (member (parent node) results :key #'id :test #'equal))
        (push node results)
        (push node younger-nodes)))
    (cond
      ((= (length nodes) (length younger-nodes))
        (setq results (nconc younger-nodes results))
        (setq nodes nil))
      (t
        (setq nodes younger-nodes)))
    (go next)))

(defun item-id (item hash-table)
  (or (gethash item hash-table)
      (setf (gethash item hash-table) (jupyter:make-uuid))))


(defun make-edge (source target &key (mask 0) (state 0) label classes
                                &aux (data (list (cons "id" (jupyter:make-uuid))
                                                 (cons "mask" mask)
                                                 (cons "state" state)
                                                 (cons "source" source)
                                                 (cons "target" target))))
  (when label
    (push (cons "label" label) data))
  (make-instance 'cytoscape:element
                 :group "edges"
                 :data data
                 :classes classes))


(defun make-node (id &key (mask 0) (state 0) label classes parent
                     &aux (data (list (cons "id" id)
                                      (cons "mask" mask)
                                      (cons "state" state))))
  (when label
    (push (cons "label" label) data))
  (when parent
    (push (cons "parent" parent) data))
  (make-instance 'cytoscape:element
                 :group "nodes"
                 :data data
                 :classes classes))


; Need a little bit of name clarification here. node-ids is a hashtable mapping objects to
; their node representation id. parent-ids is map of objects to their parent representation
; (enter-instructions) and owner-ids is map of objects to their enclosing compound parent.
(defgeneric make-elements (object node-ids parent-ids owner-ids))


(defmethod make-elements (object node-ids parent-ids owner-ids)
  (let ((parent-id (gethash object parent-ids))
        (node-id (gethash object node-ids))
        (label (cleavir-ir-graphviz:label object))
        (owner (gethash object owner-ids))
        (classes (class-list object))
        elements)

    (when node-id
      (push (make-node node-id
                       :label label
                       :parent owner
                       :classes (if parent-id
                                  (cons "proxy" classes)
                                  classes))
            elements))

    (when parent-id
      (push (make-node parent-id
                       :label label
                       :parent (unless (equal owner parent-id)
                                 owner)
                       :classes (cons "parent" classes))
            elements))

    elements))


(defmethod make-elements ((object cleavir-ir:instruction) node-ids parent-ids owner-ids)
  (let (elements
        (id (gethash object node-ids)))
    (do ((successors (cleavir-ir:successors object) (cdr successors))
         (i 0 (1+ i)))
        ((null successors))
      (push (make-edge id (gethash (car successors) node-ids)
                       :classes (list "successor") :label (write-to-string i))
            elements))

    (do ((inputs (cleavir-ir:inputs object) (cdr inputs))
         (i 0 (1+ i)))
        ((null inputs))
      (push (make-edge (gethash (car inputs) node-ids) (gethash object node-ids)
                       :classes (list "input")
                       :label (cleavir-ir-graphviz:input-label object (car inputs) i))
            elements))

    (do ((outputs (cleavir-ir:outputs object) (cdr outputs))
         (i 0 (1+ i)))
        ((null outputs))
      (push (make-edge (gethash object node-ids) (gethash (car outputs) node-ids)
                       :classes (list "input")
                       :label (cleavir-ir-graphviz:output-label object (car outputs) i))
            elements))

    (nconc elements (call-next-method))))


(defmethod make-elements ((object cleavir-ir:enclose-instruction) node-ids parent-ids owner-ids)
  (cons (make-edge (gethash (cleavir-ir:code object) node-ids) (gethash object node-ids)
                   :classes (list "code"))
        (call-next-method)))


(defmethod make-elements ((object cleavir-ir:unwind-instruction) node-ids parent-ids owner-ids)
  (cons (make-edge (gethash (cleavir-ir:destination object) node-ids) (gethash object node-ids)
                   :classes (list "destination"))
        (call-next-method)))


(defun graph-form (graph form &key direct)
  (let* ((initial-instruction (make-hir form direct))
         (node-ids (make-hash-table :test #'eq))
         (parent-ids (make-hash-table :test #'eq))
         (owners (make-hash-table :test #'eq))
         (start-id (jupyter:make-uuid))
         (elements (list (make-instance 'cytoscape:element
                                        :group "nodes"
                                        :data (list (cons "id" start-id)
                                                    (cons "mask" 0)
                                                    (cons "state" 0)
                                                    (cons "label" "START"))
                                        :classes (list "start"))
                         (make-edge start-id (item-id initial-instruction node-ids)
                                    :classes (list "start")))))


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

    (maphash (lambda (object id)
               (setq elements
                     (nconc elements
                            (make-elements object node-ids parent-ids owners))))
             node-ids)

    (maphash (lambda (object id)
               (unless (gethash object node-ids)
                 (setq elements
                       (nconc elements
                              (make-elements object node-ids parent-ids owners)))))
             parent-ids)

    (setq elements (generation-sort elements))


    (do* ((parents (nreverse (mapcan (lambda (element)
                                        (when (member "parent" (cytoscape:classes element) :test #'equal)
                                          (list element)))
                                      elements))
                                      (cdr parents))
         (parent (car parents) (car parents))
         (pos 0 (1+ pos)))
         ((null parents))
      (let* ((parent-id (id parent))
             (children (mapcan (lambda (element)
                                 (when (equal (parent element) parent-id)
                                   (list element)))
                               elements))
             (children-ids (mapcar #'id children))
             new-edges)
        (setf (cytoscape:data parent) (nconc (cytoscape:data parent)
                                             (list (cons "index" pos))))
        (dolist (child children)
          (let ((pair (assoc "mask" (cytoscape:data child) :test #'equal)))
            (rplacd pair (logior (cdr pair) (ash 1 pos))))
          (let ((pair (assoc "state" (cytoscape:data child) :test #'equal)))
            (rplacd pair (logior (cdr pair) (ash 1 pos))))
          (setf (cytoscape:removed child) t))
        (dolist (element elements)
          (when (equal "edges" (cytoscape:group element))
            (let ((source-child-p (member (source element) children-ids :test #'equal))
                  (target-child-p (member (target element) children-ids :test #'equal)))
              (when (or source-child-p target-child-p)
                (cond
                  ((and source-child-p (not target-child-p))
                    (push (make-instance 'cytoscape:element
                                         :group "edges"
                                         :removed (cytoscape:removed element)
                                         :data (list (cons "id" (jupyter:make-uuid))
                                                     (cons "source" parent-id)
                                                     (cons "target" (target element))
                                                     (cons "mask" (logior (mask element) (ash 1 pos)))
                                                     (cons "state" (state element)))
                                         :classes (cytoscape:classes element))
                          new-edges))
                  ((and (not source-child-p) target-child-p)
                    (push (make-instance 'cytoscape:element
                                         :group "edges"
                                         :removed (cytoscape:removed element)
                                         :data (list (cons "id" (jupyter:make-uuid))
                                                     (cons "source" (source element))
                                                     (cons "target" parent-id)
                                                     (cons "mask" (logior (mask element) (ash 1 pos)))
                                                     (cons "state" (state element)))
                                         :classes (cytoscape:classes element))
                          new-edges)))
                (let ((pair (assoc "mask" (cytoscape:data element) :test #'equal)))
                  (rplacd pair (logior (cdr pair) (ash 1 pos))))
                (let ((pair (assoc "state" (cytoscape:data element) :test #'equal)))
                  (rplacd pair (logior (cdr pair) (ash 1 pos))))
                (setf (cytoscape:removed element) t)))))
        (setq elements (nconc elements new-edges))))

    (setf (cytoscape:elements (cytoscape graph))
          (nconc (cytoscape:elements (cytoscape graph))
                 elements))))

(defmethod initialize-instance :after ((graph hir-graph) &rest initargs &key &allow-other-keys)
  (cytoscape:on-menu-command-select (expand-command graph)
                                    (lambda (command-instance id)
                                      (declare (ignore command-instance))
                                      (expand-node graph id)))
  (cytoscape:on-menu-command-select (collapse-command graph)
                                    (lambda (command-instance id)
                                      (declare (ignore command-instance))
                                      (collapse-node graph id)))
  (setf (cytoscape:context-menus (cytoscape graph))
        (list (make-instance 'cytoscape:context-menu
                             :selector "node"
                             :commands (list (expand-command graph) (collapse-command graph)))))
  (let ((form (getf initargs :form)))
    (when form
      (graph-form graph form))))


(defun expand-node (graph node)
  (when (stringp node)
    (setq node (find node (cytoscape:elements (cytoscape graph)) :key #'id :test #'equal)))
  (setf (visibility-state graph) (logior (visibility-state graph) (ash 1 (index node))))
  (jupyter:inform :info nil "~A" (visibility-state graph))
  (dolist (element (cytoscape:elements (cytoscape graph)))
    (setf (cytoscape:removed element)
          (not (equal (logand (mask element) (visibility-state graph)) (state element)))))
  (cytoscape:layout (cytoscape graph)))


(defun collapse-node (graph node)
  (when (stringp node)
    (setq node (find node (cytoscape:elements (cytoscape graph)) :key #'id :test #'equal)))
  (setf (visibility-state graph) (logand (visibility-state graph) (lognot (ash 1 (index node)))))
  (jupyter:inform :info nil "~A" (visibility-state graph))
  (dolist (element (cytoscape:elements (cytoscape graph)))
    (setf (cytoscape:removed element)
          (not (equal (logand (mask element) (visibility-state graph)) (state element)))))
  (cytoscape:layout (cytoscape graph)))

