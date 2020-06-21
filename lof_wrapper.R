lof_wrapper <- function(xtrn, xtst, k = 5, fracrej = 0.01, cores=2){
  #local outlier factor: 
  #xtrn: labeled training set, should contain inliers only
  #xtst: labeled test set can contain both inliers and outliers 
  #k: number of neighbors to consider
  #fracrej: the fraction of training objects to reject as outliers.
  #cores: number of CPUs to run in parallel
  
  if(!require("Rlof")) install.packages("Rlof")
  library("Rlof")
  
  x <- xtrn[,-ncol(xtrn)]
  scores_training <- lof(x,k,cores)
  if (length(k)==1) scores_training <- as.matrix(scores_training)
  
  out <- list()
  for (z in seq_along(k)){
    ss <- sort(scores_training[,z], decreasing = TRUE)
    nrej <- ceiling(fracrej * length(ss))
    if (fracrej==1) thr <- -Inf
    else if (fracrej==0) thr <- Inf
    else thr <- ss[nrej]
    
    tmp <- rep(0,dim(xtst)[1])
    for (i in 1:dim(xtst)[1]){
      score <- lof(rbind(x,xtst[i,-ncol(xtst)]),k,cores)
      if(length(k)==1) score <- as.matrix(score)
      tmp[i] <- as.numeric(score[nrow(score),z] > thr)
    }
    out[[z]] <- tmp
  }
  return(out)
}
