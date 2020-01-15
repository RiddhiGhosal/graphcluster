graphcluster for High Dimensional Clustering
================

This is the first version of the graphcluster package. This vignette provides a guideline for using this package.

Setting up
----------

``` r
library(graphcluster)
set.seed(0)
```

Loading data
------------

graphcluster requires inputting the dataset in the form of a dataframe or a matrix

``` r
# Load Data
# path contains decompressed files 
load("C:\\Academics\\projects\\ml 2018 s.bandhyopadhyay\\data\\stevequake\\steve_quake.RData")
ncount=t(ncount)
```

Pre-processing
--------------

graphcluster uses the same pre-processing technique that was used in dropclust. See: <https://github.com/debsin/dropClust/blob/master/vignettes/vignette.Rmd>

Perform clustering parallely
----------------------------

Call the parallel\_cluster function with suitable parameters. The following is an example: system.time(elist\_p&lt;-parallel\_cluster(30,50,30,bench\_data,0.95))

After all iterations are over, make graphs for the points that were not sampled even once. A sample code is as follows:
=======================================================================================================================

l=length(elist\_p) sub=as.character("",l\*2) for(i in 1:l) { sub\[i\]=substring(elist\_p\[i\],1,10) sub\[i+l\]=substring(elist\_p\[i\],12,21) } usub=unique(sub) if(length(usub)!=nrow(bench\_data)) { reducedbench\_data &lt;- bench\_data\[!(rownames(bench\_data) %in% usub),\]

system.time(elist\_p1&lt;-parallel\_cluster(1,nrow(reducedbench\_data),30,reducedbench\_data,0.90)) elist\_p=append(elist\_p,elist\_p1)

} elist\_p2=append(elist\_p,elist\_p1) \# make the final graoh as follows. system.time(K&lt;-create\_graph\_comm(elist\_p, ann)) print(paste("Parallel Time",Sys.time()-s)) \#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#
