somocc <- function(xtrn, xtst, gridSize = 4, fracrej = 0.05, seed=1){
  #training set should contain inliers only
  #test set can contain inliers and outliers
  #set seed to fix the resulting centroids
  #number of clusters = gridSize x gridSize
  #fracrej is the fraction of training objects to reject.
  
  if (!require("rdist")) install.packages("rdist") 
  if (!require("kohonen")) install.packages("kohonen")
  library("rdist")
  library("kohonen")
  
  set.seed(seed)
  tmp <- xtrn
  colnames(tmp) <- NULL
  trainDataMatrix <- as.matrix(tmp[,-ncol(tmp)])
  #Ensure outliers of training set are not used
  Ltrn <- xtrn[,ncol(xtrn)]
  trainDataMatrix <- trainDataMatrix[Ltrn==0,] #inliers
  
  som_grid <- somgrid(xdim = gridSize, ydim=gridSize, topo="hexagonal")
  som_model <- som(trainDataMatrix, grid=som_grid, rlen=100, alpha=c(0.05,0.01))
  centroids <- som_model$codes[[1]]
  
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
  
  l <- list(Ltst,centroids)
  return(l)
}