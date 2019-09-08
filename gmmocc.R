gmmocc <- function(xtrn, xtst, k = 6, fracrej = 0.05, seed=1){
  #training set should contain inliers only
  #test set can contain inliers and outliers
  #set seed to fix the resulting centroids
  #Parameter k is the number of Gaussian components
  
  if (!require("rdist")) install.packages("rdist") 
  if (!require("mclust")) install.packages("mclust")
  library("rdist")
  library("mclust")
  
  set.seed(seed)
  tmp <- xtrn
  colnames(tmp) <- NULL
  trainDataMatrix <- as.matrix(tmp[,-ncol(tmp)])
  #Ensure outliers of training set are not used
  Ltrn <- xtrn[,ncol(xtrn)]
  trainDataMatrix <- trainDataMatrix[Ltrn==0,] #inliers
  
  mod <- densityMclust(data=trainDataMatrix,G=k,modelNames = c("VVV"))
  centers <- t(mod$parameters$mean) #centers are candidate modes overall
  d <- predict(mod,centers) #mixture model value at centers' locations
  largestVal <- max(d) #highest value at mode
  
  density <- mod$density
  Ptrn <- density/largestVal # in [0,1]
  
  # compute threshold equivalent to fracrej
  nr <- length(Ptrn) #number of target/training objects
  nobj_acc <- round((1-fracrej)*nr)
  po <- sort(Ptrn, decreasing = TRUE)
  if (nobj_acc==0) thr <- 1
  else if (nobj_acc==nr) thr<- 0
  else thr <- po[nobj_acc]
  
  tmp <- xtst
  colnames(tmp) <- NULL
  testDataMatrix <- as.matrix(tmp[,-ncol(tmp)])
  d <- predict(mod,testDataMatrix)
  Ptst <- d/largestVal
  
  #Label test set
  Ltst <- rep(0,nrow(xtst))
  Ltst[Ptst < thr] <- 1 #assign as outlier
  l <- list(Ltst,Ptst,mod)
  return(l)
}
