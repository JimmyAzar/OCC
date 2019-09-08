kmeansocc <- function(xtrn, xtst, k = 10, fracrej = 0.05, seed=1){
  #training set should contain inliers only
  #test set can contain inliers and outliers
  #fracrej is the fraction of training objects to reject.
  #set seed to fix the resulting centroids

  if (!require("rdist")) install.packages("rdist") 
  library("rdist")
  
  set.seed(seed)
  tmp <- xtrn
  colnames(tmp) <- NULL
  trainDataMatrix <- as.matrix(tmp[,-ncol(tmp)])
  #Ensure outliers of training set are not used
  Ltrn <- xtrn[,ncol(xtrn)]
  trainDataMatrix <- trainDataMatrix[Ltrn==0,] #inliers

  #call k-means  
  cl <- kmeans(trainDataMatrix,k,iter.max = 100, nstart=10) #'nstart' repetitions
  centroids <- cl$centers 
  
  trainDistMat <- t(cdist(centroids,trainDataMatrix,metric="euclidean"))
  trainDistMatOrder <- t(apply(trainDistMat,1,order,decreasing = FALSE))
  d <- diag(trainDistMat[,trainDistMatOrder[,1]]) #distance to nearest centroid
  
  # compute threshold equivalent to fracrej
  nf <- length(d)
  dSorted <- sort(d,decreasing = FALSE)
  nobj_acc <- round((1-fracrej)*nf)
  if (nobj_acc==0) thr <- 0 
  else if (nobj_acc==nf) thr <- Inf 
  else thr <- dSorted[nobj_acc]
  
  tmp <- xtst
  colnames(tmp) <- NULL
  testDataMatrix <- as.matrix(tmp[,-ncol(tmp)])
  testDistMat <- t(cdist(centroids,testDataMatrix,metric="euclidean"))
  testDistMatOrder <- t(apply(testDistMat,1,order,decreasing = FALSE))
  d <- diag(testDistMat[,testDistMatOrder[,1]]) #distance to nearest centroid
  
  #Label test set
  Ltst <- rep(0,nrow(xtst))
  Ltst[d > thr] <- 1 #assign as outlier
  
  l <- list(Ltst,cl$cluster,cl$centers)
  return(l)
}
