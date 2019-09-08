knnocc <- function(xtrn, xtst, k = 1, fracrej = 0.05){
  #k-nearest neighbor one-class classifier
  #training set should contain inliers only
  #test set can contain inliers and outliers
  #fracrej is the fraction of training objects to reject
  
  if (!require("rdist")) install.packages("rdist") 
  library("rdist")
  
  tmp <- xtrn
  colnames(tmp) <- NULL
  trainDataMatrix <- as.matrix(tmp[,-ncol(tmp)])
  #Ensure outliers of training set are not used (in case training set includes outliers)
  Ltrn <- xtrn[,ncol(xtrn)]
  trainDataMatrix <- trainDataMatrix[Ltrn==0,] #inliers
  
  trainDistMat <- pdist(trainDataMatrix,metric="euclidean") #distance matrix over training objects
  trainDistMatOrder <- t(apply(trainDistMat,1,order,decreasing = FALSE)) #sort by ascending order
  d <- diag(trainDistMat[,trainDistMatOrder[,k+1]]) #distance to knn for each training object (use k+1 since first element is always 0 i.e. distance to self)
  
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
  testDistMat <- t(cdist(trainDataMatrix,testDataMatrix,metric="euclidean")) #test objects as rows, training objects as columns
  testDistMatOrder <- t(apply(testDistMat,1,order,decreasing = FALSE)) #asending order
  f <- diag(testDistMat[,testDistMatOrder[,k]]) #distance to knn in training set for each test object

    #Label test set
  Ltst <- rep(0,nrow(xtst))
  Ltst[f > thr] <- 1 #assign as outlier (too distant)
  
  l <- list(Ltst) #return as list to be consistent with other classifier outputs
  return(l)
}
