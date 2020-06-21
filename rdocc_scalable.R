rdocc_scalable <- function(xtrn, xq, k = 1, h = 1 , fracrej = 0.05, verbose = TRUE){
  #labeled training set xtrn should contain inliers only
  #labeled query points xq (test set)
  #method computes the following difference: ...
  #Parzen density using all training objects - average Parzen density of k nearest neighbors. 
  #k is a vector of number of neighbors to try out. 
  #h is the standard deviation of the Gaussian kernel. 
  #fracrej is the fraction of training objects to reject.
  
  if(!require("rdist")) install.packages("rdist")
  library("rdist")
  
  tmp <- xtrn
  colnames(tmp) <- NULL
  trainDataMatrix <- as.matrix(tmp[,-ncol(tmp)])
  #ensure outliers of training set are not used
  Ltrn <- xtrn[,ncol(xtrn)]
  trainDataMatrix <- trainDataMatrix[Ltrn==0,,drop=FALSE] #inliers
  
  #compute scores using training set: 
  #use all training objects to estimate density at training object locations 
  Ptrn <- numeric(0)
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
  K <- matrix(0,nrow=1000*ceiling(n/1000),ncol=ceiling(n/1000))
  for (i in 1:(ceiling(n/1000))){
    if(verbose) print(paste("training: batch...",ceiling(n/1000)-i)) 
    for (j in 1:(ceiling(n/1000))){
      block <- cdist(Q[,,i],Q[,,j],metric="euclidean")
      gblock <- 1/(h*sqrt(2*pi))*exp(-0.5*(block/h)^2) 
      if (j == ceiling(n/1000) & flag == 1) {
        s <- apply(gblock[,1:realsize],1,sum)
        #for knn part
        o <- t(apply(block[,1:realsize],1,order,decreasing=FALSE))
        sorted <- t(apply(block[,1:realsize],1,sort,decreasing=FALSE))
        mn <- min(kmax,realsize)
        corrected_order[(1000*(i-1)+1):(1000*i),(1+((j-1)*kmax)):(mn+((j-1)*kmax))] <- o[,1:mn,drop=FALSE] + (1000*(j-1))
        dist[(1000*(i-1)+1):(1000*i),(1+((j-1)*kmax)):(mn+((j-1)*kmax))] <- sorted[,1:mn,drop=FALSE]
      } 
      else {
        s <- apply(gblock,1,sum) #reduce block to column
        #for knn part
        o <- t(apply(block,1,order,decreasing=FALSE))
        sorted <- t(apply(block,1,sort,decreasing=FALSE))
        corrected_order[(1000*(i-1)+1):(1000*i),(1+((j-1)*kmax)):(kmax+((j-1)*kmax))] <- o[,1:kmax,drop=FALSE] + (1000*(j-1))
        dist[(1000*(i-1)+1):(1000*i),(1+((j-1)*kmax)):(kmax+((j-1)*kmax))] <- sorted[,1:kmax,drop=FALSE]
      }
      K[(1000*(i-1)+1):(1000*i),j] <- s  #fill column with the sum
    }
  }
  A <- apply(K,1,sum)
  A <- A[1:dim(trainDataMatrix)[1]]
  A <- A/dim(trainDataMatrix)[1] #ignore the padded zeros
  Ptrn <- h*sqrt(2*pi) * A  #prob. of inlier in [0,1]
  localP <- Ptrn 
  
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
    Ptst <- matrix(A[nn],nrow=dim(nn)[1],byrow = FALSE)
    Ptst <- h*sqrt(2*pi) * Ptst  #prob. of inlier in [0,1]
    wideP <- apply(Ptst,1,mean)
  
    relDist <- localP - wideP 
      
    # compute threshold equivalent to fracrej
    nf <- length(relDist)
    relDistSorted <- sort(relDist,decreasing = TRUE)
    nobj_acc <- floor((1-fracrej)*nf)
    if (nobj_acc==0) {
      thr[[z]] <- Inf
    } else if (nobj_acc==nf) {
      thr[[z]] <- -Inf 
    } else thr[[z]] <- relDistSorted[nobj_acc]
  }

  #Compute score using query points  
  Pq <- numeric(0)
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
    
  wideP <- numeric(0)
  corrected_order <- matrix(NA,nrow=1000*ceiling(ntst/1000),ncol=kmax*ceiling(ntrn/1000))
  dist <- matrix(NA,nrow=1000*ceiling(ntst/1000),ncol=kmax*ceiling(ntrn/1000))
  K <- matrix(0,nrow=1000*ceiling(ntst/1000),ncol=ceiling(ntrn/1000))
  for (i in 1:(ceiling(ntst/1000))){
    if(verbose) print(paste("testing: batch...:",ceiling(ntst/1000)-i)) 
    for (j in 1:(ceiling(ntrn/1000))){
      block <- cdist(Qtst[,,i],Qtrn[,,j],metric="euclidean") 
      gblock <- 1/(h*sqrt(2*pi))*exp(-0.5*(block/h)^2) 
      if (j == ceiling(ntrn/1000) & flag == 1) {
        s <- apply(gblock[,1:realsize],1,sum)
        #for knn part
        o <- t(apply(block[,1:realsize],1,order,decreasing=FALSE))
        sorted <- t(apply(block[,1:realsize],1,sort,decreasing=FALSE))
        mn <- min(kmax,realsize)
        corrected_order[(1000*(i-1)+1):(1000*i),(1+((j-1)*kmax)):(mn+((j-1)*kmax))] <- o[,1:mn,drop=FALSE] + (1000*(j-1))
        dist[(1000*(i-1)+1):(1000*i),(1+((j-1)*kmax)):(mn+((j-1)*kmax))] <- sorted[,1:mn,drop=FALSE]
        }
      else {
        s <- apply(gblock,1,sum) #reduce block to column
        #for knn part
          o <- t(apply(block,1,order,decreasing=FALSE))
        sorted <- t(apply(block,1,sort,decreasing=FALSE))
        corrected_order[(1000*(i-1)+1):(1000*i),(1+((j-1)*kmax)):(kmax+((j-1)*kmax))] <- o[,1:kmax,drop=FALSE] + (1000*(j-1))
        dist[(1000*(i-1)+1):(1000*i),(1+((j-1)*kmax)):(kmax+((j-1)*kmax))] <- sorted[,1:kmax,drop=FALSE]
      } 
      K[(1000*(i-1)+1):(1000*i),j] <- s  #fill column with the sum
    }
  }
  B <- apply(K,1,sum)
  B <- B[1:ntst]
  B <- B/ntrn #ignore the padded zeros
  Pq <- h*sqrt(2*pi) * B  #prob. of inlier in [0,1]
  localP <- Pq
    
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
    Ptst <- matrix(A[nn],nrow=dim(nn)[1],byrow=FALSE) #A not B
    Ptst <- h*sqrt(2*pi) * Ptst  #prob. of inlier in [0,1]
    wideP <- apply(Ptst,1,mean)
 
    f <- localP-wideP
    
    #Label test set
    Ltst <- rep(0,nrow(xq))
    Ltst[f < thr[[z]]] <- 1 #assign as outlier
    l[[z]] <- Ltst #return list to be consistent with other classifiers
  }
  return(l)
}