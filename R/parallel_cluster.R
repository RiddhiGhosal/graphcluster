#' Creates edge List in Parallel
#'
#' @param nbatches Number of batches to consider.
#' @param batch_len size of each batches
#' @param Ngenes Number of genes to consider for each batch
#' @param mat The preprocessed matrix with geans in each column and samples in every row.
#' @param prob The quantile below which edges are not formed
#' @return Returns the edgelist
#'
#' @import parallel
#' @import foreach
#' @import doParallel
#' @importFrom utils combn
#' @importFrom stats quantile dist
#' @export

parallel_cluster<-function(mat,nbatches,batch_len,Ngenes,prob=0.6){

  # nbatches = 3000
  # batch_len = 10
  # Ngenes = 300
  # mat = bench_data


  print(paste("Number of cores detected",detectCores()))
  # Create cluster with desired number of cores
  #cl <- makeCluster(detectCores()-2)
  # Register cluster
  registerDoParallel(detectCores()-1)

  getDoParWorkers()

  set.seed(100)

  biglist<-foreach(i = 1:nbatches, .combine=list, .inorder=FALSE, .multicombine = TRUE ) %dopar% {

    N = ncol(mat)


    batch_ids = sort(sample(1:dim(mat)[1],batch_len,replace = FALSE))
    batch_mat = as.matrix(mat[batch_ids,sample(1:N,Ngenes)])
    cor_mat  = cor(t(batch_mat),method = "pearson")
    cor_mat = as.matrix(dist(1-cor_mat))

    cor_mat[cor_mat==0] <- 0.0001
    cor_mat <- 1/cor_mat

    pairs = combn(rownames(cor_mat), 2)

    k= list()
    v= list()
    for (pair_id in 1:dim(pairs)[2]){

      key = paste(c(pairs[,pair_id]), collapse = " ")
      k= list(k,list(key))
      value  = cor_mat[pairs[1,pair_id],pairs[2,pair_id]]
      v= list(v,list(value))
    }

    k= unlist(k)
    v= unlist(v)
    th = quantile(v, probs = prob)
    ids<-ifelse(v > th, T, F)
    k=k[ids]
    v=v[ids]

    edgesubset = list()
    for(i in 1:length(k)){
      edgesubset = list(edgesubset,list(paste(k,v)))
    }

    return(unlist(edgesubset))
  }
  biglist = unlist(biglist)

  stopImplicitCluster()

  gc()
  return(biglist)
}
