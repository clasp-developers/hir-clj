
(in-package :asdf-user)

(defsystem "hir-clj"
    :description "Generate cytoscape graphs for HIR"
    :version "0.0.1"
    :author "Christian Schafmeister <>"
    :licence "LGPL-2.0"
  :depends-on (:smarts
    :cytoscape-clj)
    :serial t
    :components
    ((:file "packages")
     (:file "hir")))

