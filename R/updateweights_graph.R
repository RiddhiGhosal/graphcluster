#' Detects Clusters
#'
#' @param edgelist Edgelist as returned by gen_edge function
#' @param data_labels The data labels exported
#' @param prob The quantile below which edges are not formed
#' @return Returns graph density
#' @export
#' @import igraph
#' @importFrom stats quantile
updateweights_graph<-function(edgelist,data_labels,prob=0.6){

  #edgelist=elist
  column1=list()
  column2=list()
  weights =list()
  for (pairs in names(edgelist)){
    #a <- list(a, list(i))
    column1 = list(column1, list(strsplit(pairs," ")[[1]][1])) #extract node_id1
    column2 = list(column2, list(strsplit(pairs," ")[[1]][2])) #extract node_id2
    weights = list(weights,list(unname(edgelist[[pairs]])))
  }
  column1<-unlist(column1)
  column2<-unlist(column2)
  weights<-unlist(weights)

  # quantile(weights)
  #qt=4
  th<- quantile(weights,probs = prob)
  ids<-ifelse(weights > th, T, F)
  col1<-column1[ids]
  col2<-column2[ids]
  wts<-weights[ids]


  e = as.matrix(cbind(col1,col2))
  g = graph_from_edgelist(as.matrix(e), directed = FALSE)
  E(g)$weight<-wts
  V(g)$color <-data_labels[match(names(V(g)),names(data_labels))]

  comm <- cluster_louvain(g)
  #t<-table(comm$membership,V(g)$color)


  return(list(graph=g,communities = comm,edge_density = edge_density(g, loops=TRUE)))

}
