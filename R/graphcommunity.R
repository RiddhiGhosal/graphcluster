#'Form the final Graph communities
#'
#' @param edgelist The list of edges in the subgraphs
#' @param data_labels The data labels exported
#' @import data.table
#' @export
graphcommunity<-function(edgelist,data_labels){


  df = data.table(EDGES = edgelist)
  dt = as.data.table(df)
  dt[, c("col1", "col2","wts") := tstrsplit(EDGES, " ", fixed=TRUE)]
  dt[, wts:=as.numeric(wts)]

  g2=graph.data.frame(as.data.frame(dt[,.(col1,col2,wts)]), directed = FALSE)
  g2 = simplify(g2,  edge.attr.comb = list(weight="ignore"))

  V(g2)$color <-data_labels[match(names(V(g2)),names(data_labels))]

  comm <- cluster_louvain(g2, weights = NA)


  return(list(graph=g2,communities = comm,edge_density = edge_density(g2, loops=TRUE)))


}
