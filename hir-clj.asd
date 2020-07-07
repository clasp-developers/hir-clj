
(in-package :asdf-user)

(defsystem "hir-clj"
    :description "Generate cytoscape graphs for HIR"
    :version "0.0.1"
    :author "Christian Schafmeister <>"
    :licence "LGPL-2.0"
  :depends-on (
    :cytoscape-clj)
    :serial t
    :components
    ((:module src
      :serial t
      :components
        ((:file "packages")
         (:file "hir")))
     (:module styles
      :components
        ((:static-file "graphviz.css")
         (:static-file "flowchart.css")))))

