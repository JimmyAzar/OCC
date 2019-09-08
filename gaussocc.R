gaussocc <- function(xtrn, xtst, fracrej = 0.05){
  #Training set contains only inliers. 
  #Fracrej is the fraction of training objects to reject
  
  p <- ncol(xtrn)-1 #number of features
  n <- nrow(xtrn) #numer of training objects
  mu <- apply(xtrn[,-ncol(xtrn)],2,mean) #overall mean
  covar <- cov(xtrn[,-ncol(xtrn)]) #covariance matrix  
  X <- xtrn[,-ncol(xtrn)] - matrix(rep(mu,n),nrow=n,byrow=TRUE)
  D <- (as.matrix(X) %*% solve(covar))*X
  d <- apply(D,1,sum)
  
  #Relative densities of training objects
  ftrn <- 1/sqrt((2*pi)^p * det(covar)) * exp(-0.5*d) #densities
  Ptrn <- exp(-0.5*d) #probability of being target
  
  #Relative densities of test objects
  n <- nrow(xtst) #number of test objects
  Z <- xtst[,-ncol(xtst)] - matrix(rep(mu,n),nrow=n,byrow=TRUE) #using mean of training set
  Dz <- (as.matrix(Z) %*% solve(covar))*Z #using covariance of training set
  dz <- apply(Dz,1,sum)
  #S <- cov(xtst[,-ncol(xtst)])
  ftst <- 1/sqrt((2*pi)^p * det(covar)) * exp(-0.5*dz) #densities
  Ptst <- as.numeric(exp(-0.5*dz)) #probability of being target 
  
  #Compute threshold equivalent to fracrej
  nr <- length(Ptrn) #number of target/training objects
  nobj_acc <- round((1-fracrej)*nr)
  po <- sort(Ptrn, decreasing = TRUE)
  if (nobj_acc==0) thr <- 1
  else if (nobj_acc==nr) thr<- 0
  else thr <- po[nobj_acc]
  
  #Label test set
  Ltst <- rep(0,nrow(xtst))
  Ltst[Ptst < thr] <- 1 #assign as outlier
  
  l <- list(Ltst,Ptst)
  return(l)
}

