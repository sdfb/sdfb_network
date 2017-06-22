---
  output: 
  html_document: 
  self_contained: no
---

# Contents

### PGL_fit.R

The contains parallelized code for performing the boostrapped style processing of the document count matrices.

### PGL_tutorial.Rmd

An Rmarkdown file that provides an simplified overview of how the Possion Graphical Lasso modeling works with GLMNET.

### network_graph.R

Converts PGL_fit.R results into a igraph network object.

### export_graph.R

Provides methods for converting the igraph network object into additional respresentations, namely Gephi (.gefx)
  