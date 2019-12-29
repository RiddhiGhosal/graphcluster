

#' Creates edge List
#'
#' @param nbatches Number of batches to consider.
#' @param batch_len size of each batches
#' @param Ngenes Number of genes to consider for each batch
#' @param mat The preprocessed matrix with geans in each column and samples in every row.
#' @importFrom stats cor dist
#' @importFrom hash hash has.key
#' @importFrom utils View combn
#' @return Returns the edgelist
#' @export
#'

gen_edge<-function(nbatches,batch_len,Ngenes,mat){
  # nbatches = no_of_batches
  # batch_len = batch_size
  # mat = bench_data



  edgelist <- hash() #store in hash table key:value pair where key ="node_id1,node_id2" and value = min weight
  for (i in 1:nbatches){

    #i =1
    N = ncol(mat)


    batch_ids = sample(1:dim(mat)[1],batch_len,replace = FALSE)
    batch_mat = as.matrix(mat[batch_ids,sample(1:N,Ngenes)])
    cor_mat  = cor(t(batch_mat),method = "pearson")
    cor_mat = as.matrix(dist(1-cor_mat))



    cor_mat[cor_mat==0] <- 0.0001


    cor_mat <- 1/cor_mat



    if(length(which(is.infinite(cor_mat)))>=1)
    {
      View(cor_mat)
      break;
    }


    pairs = combn(rownames(cor_mat), 2)

    for (pair_id in 1:dim(pairs)[2]){

      key = paste(c(pairs[,pair_id]), collapse = " ")

      value  = cor_mat[pairs[1,pair_id],pairs[2,pair_id]]

      edgelist[key] = ifelse(has.key(key,edgelist),  max(edgelist[[key]], value), value )
    }
  }
  return(edgelist)
}
