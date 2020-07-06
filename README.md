# Render Cleavir HIR using cytoscape and display it in jupyterlab

## To generate a HIR graph

This compiles the form and captures the HIR during the compilation.
The HIR is stored in clasp-cleavir::*hir* until the next `hir-clj:draw-form-hir` invocation.

`(hir-clj:draw-form-hir '(lambda (x y) (+ x y)))`

## To immediately the same hir graph using graphviz for comparison

`(clasp-cleavir::draw-hir clasp-cleavir::*hir* "/tmp/hir.dot")`

