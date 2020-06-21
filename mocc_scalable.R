mocc_scalable <- function(xtrn, xq, k = 1, fracrej = 0.05, verbose = TRUE){
  #labeled training set xtrn should contain inliers only
  #labeled query points xq (test set)
  #method computes the distance from a test object to the median of its k-nearest neighbors. 
  #k is a vector of number of neighbors to try out. 
  #fracrej is the fraction of training objects to reject.
  
  if(!require("rdist")) install.packages("rdist")
  library("rdist")
  
  tmp <- xtrn
  colnames(tmp) <- NULL
  trainDataMatrix <- as.matrix(tmp[,-ncol(tmp)])
  #ensure outliers of training set are not used
  Ltrn <- xtrn[,ncol(xtrn)]
  trainDataMatrix <- trainDataMatrix[Ltrn==0,,drop=FALSE] #inliers

  colnames(xtrn) <- NULL
  
  #break into blocks to make scalable
  n <- dim(trainDataMatrix)[1]
  kmax <- max(k)
  
  Q <- array(0,dim=c(1000,dim(trainDataMatrix)[2],ceiling(n/1000)))
  flag <- 0 
  for (i in 1:ceiling(n/1000)){
    tmp <- trainDataMatrix[(1000*(i-1)+1):min(1000*i,n),,drop=FALSE]
    if(dim(tmp)[1] < 1000) {
      pad <- rbind(tmp,matrix(0,nrow=1000-dim(tmp)[1],ncol=dim(trainDataMatrix)[2]))
      Q[,,i] <- pad
      flag <- 1
      realsize <- dim(tmp)[1]
    }
    else Q[,,i] <- tmp
  }
  
  wideP <- numeric(0)
  corrected_order <- matrix(NA,nrow=1000*ceiling(n/1000),ncol=kmax*ceiling(n/1000))
  dist <- matrix(NA,nrow=1000*ceiling(n/1000),ncol=kmax*ceiling(n/1000))
  for (i in 1:(ceiling(n/1000))){
    if(verbose) print(paste("training: batch...",ceiling(n/1000)-i)) 
    for (j in 1:(ceiling(n/1000))){
      block <- cdist(Q[,,i],Q[,,j],metric="euclidean")
      if (j == ceiling(n/1000) & flag == 1) {
        #for knn part
        o <- t(apply(block[,1:realsize],1,order,decreasing=FALSE))
        sorted <- t(apply(block[,1:realsize],1,sort,decreasing=FALSE))
        mn <- min(kmax,realsize)
        corrected_order[(1000*(i-1)+1):(1000*i),(1+((j-1)*kmax)):(mn+((j-1)*kmax))] <- o[,1:mn,drop=FALSE] + (1000*(j-1))
        dist[(1000*(i-1)+1):(1000*i),(1+((j-1)*kmax)):(mn+((j-1)*kmax))] <- sorted[,1:mn,drop=FALSE]
      } 
      else {
        #for knn part
        o <- t(apply(block,1,order,decreasing=FALSE))
        sorted <- t(apply(block,1,sort,decreasing=FALSE))
        corrected_order[(1000*(i-1)+1):(1000*i),(1+((j-1)*kmax)):(kmax+((j-1)*kmax))] <- o[,1:kmax,drop=FALSE] + (1000*(j-1))
        dist[(1000*(i-1)+1):(1000*i),(1+((j-1)*kmax)):(kmax+((j-1)*kmax))] <- sorted[,1:kmax,drop=FALSE]
      }
    }
  }

  thr <- list()
  for (z in seq_along(k)){
    if (dim(dist)[2]==1) {
      O <- matrix(1,nrow=nrow(dist),ncol=1)
    } 
    else {
      O <- t(apply(dist,1,order,decreasing=FALSE))[,1:k[z],drop=FALSE]
    }
    nn <- matrix(NA,nrow=dim(O)[1],ncol=k[z])
    for (i in 1:dim(O)[1]){
      nn[i,] <- corrected_order[i,O[i,],drop=FALSE]
    }
    nn <- nn[1:dim(trainDataMatrix)[1],,drop=FALSE] #trim 
    
    relDist <- c()
    for (i in 1:nrow(nn)){
      xmed <- apply(trainDataMatrix[nn[i,], ,drop=FALSE],2,median) #compute median feature values
      relDist[i] <- sqrt(sum((trainDataMatrix[i,] - xmed)^2))
    }
    # compute threshold equivalent to fracrej
    nf <- length(relDist)
    relDistSorted <- sort(relDist,decreasing = FALSE)
    nobj_acc <- floor((1-fracrej)*nf)
    if (nobj_acc==0) {
      thr[[z]] <- -Inf
    } else if (nobj_acc==nf) {
      thr[[z]] <- Inf 
    } else thr[[z]] <- relDistSorted[nobj_acc]
  }
  
  #Compute score using query points  
  colnames(xq) <- NULL
  testDataMatrix <- as.matrix(xq[,-ncol(xq)])
  
  #break into blocks to make scalable
  ntst <- dim(testDataMatrix)[1]
  ntrn <- dim(trainDataMatrix)[1]
  
  Qtst <- array(0,dim=c(1000,dim(testDataMatrix)[2],ceiling(ntst/1000)))
  for (i in 1:ceiling(ntst/1000)){
    tmp <- testDataMatrix[(1000*(i-1)+1):min(1000*i,ntst),,drop=FALSE]
    if(dim(tmp)[1] < 1000) {
      pad <- rbind(tmp,matrix(0,nrow=1000-dim(tmp)[1],ncol=dim(testDataMatrix)[2]))
      Qtst[,,i] <- pad
    }
    else Qtst[,,i] <- tmp
  }
  
  Qtrn <- array(0,dim=c(1000,dim(trainDataMatrix)[2],ceiling(ntrn/1000)))
  flag <- 0
  for (i in 1:ceiling(ntrn/1000)){
    tmp <- trainDataMatrix[(1000*(i-1)+1):min(1000*i,ntrn),,drop=FALSE]
    if(dim(tmp)[1] < 1000) {
      pad <- rbind(tmp,matrix(0,nrow=1000-dim(tmp)[1],ncol=dim(trainDataMatrix)[2]))
      Qtrn[,,i] <- pad
      flag <- 1
      realsize <- dim(tmp)[1]
    }
    else Qtrn[,,i] <- tmp
  }
  
  corrected_order <- matrix(NA,nrow=1000*ceiling(ntst/1000),ncol=kmax*ceiling(ntrn/1000))
  dist <- matrix(NA,nrow=1000*ceiling(ntst/1000),ncol=kmax*ceiling(ntrn/1000))
  for (i in 1:(ceiling(ntst/1000))){
    if(verbose) print(paste("testing: batch...:",ceiling(ntst/1000)-i)) 
    for (j in 1:(ceiling(ntrn/1000))){
      block <- cdist(Qtst[,,i],Qtrn[,,j],metric="euclidean") 
      if (j == ceiling(ntrn/1000) & flag == 1) {
        #for knn part
        o <- t(apply(block[,1:realsize],1,order,decreasing=FALSE))
        sorted <- t(apply(block[,1:realsize],1,sort,decreasing=FALSE))
        mn <- min(kmax,realsize)
        corrected_order[(1000*(i-1)+1):(1000*i),(1+((j-1)*kmax)):(mn+((j-1)*kmax))] <- o[,1:mn,drop=FALSE] + (1000*(j-1))
        dist[(1000*(i-1)+1):(1000*i),(1+((j-1)*kmax)):(mn+((j-1)*kmax))] <- sorted[,1:mn,drop=FALSE]
      }
      else {
        #for knn part
        o <- t(apply(block,1,order,decreasing=FALSE))
        sorted <- t(apply(block,1,sort,decreasing=FALSE))
        corrected_order[(1000*(i-1)+1):(1000*i),(1+((j-1)*kmax)):(kmax+((j-1)*kmax))] <- o[,1:kmax,drop=FALSE] + (1000*(j-1))
        dist[(1000*(i-1)+1):(1000*i),(1+((j-1)*kmax)):(kmax+((j-1)*kmax))] <- sorted[,1:kmax,drop=FALSE]
      } 
    }
  }

  l <- list()
  for (z in seq_along(k)){
    if (dim(dist)[2]==1) {
      O <- matrix(1,nrow=nrow(dist),ncol=1)
    }
    else {
      O <- t(apply(dist,1,order,decreasing=FALSE))[,1:k[z],drop=FALSE]
    }
    nn <- matrix(NA,nrow=dim(O)[1],ncol=k[z])
    for (i in 1:dim(O)[1]){
      nn[i,] <- corrected_order[i,O[i,],drop=FALSE]
    }
    nn <- nn[1:ntst,,drop=FALSE]
  
    f <- c()
    for (i in 1:nrow(nn)){
      xmed <- apply(trainDataMatrix[nn[i,], ,drop=FALSE],2,median) #compute median feature values
      f[i] <- sqrt(sum((testDataMatrix[i,] - xmed)^2))
    }
    
    #Label test set
    Ltst <- rep(0,nrow(xq))
    Ltst[f > thr[[z]]] <- 1 #assign as outlier
    l[[z]] <- Ltst #return list to be consistent with other classifiers
  }
  return(l)
}