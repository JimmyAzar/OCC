mediantx <- function(X, k = 10, verbose =  TRUE){
  #Median transform
  #X: unlabeled data in standard format (matrix type) 
  #k: the number of neighbors 
  #Description: generates a new dataset of same size as X by replacing each object in X... 
  #...with the 'median' object from within its k-nearest neighbors. The median object is... 
  #...computed from the k-nearest neighbors by considering the median value for each feature.
  
  if (!require("rdist")) install.packages("rdist") 
  library("rdist")
  
  X <- as.matrix(X)
  n <- dim(X)[1] #number of objects
  p <- dim(X)[2] #number of features
  k <- k + 1 #increment k to account for object itself in distance matrix 
    
  #scalable version to avoid memory overload.
  if (n > 1000){
    #use blocks of at most 1000 objects at a time 
    Q <- array(0,dim=c(1000,p,ceiling(n/1000)))
    flag <- 0 #block contains 1000 objects
    for (i in 1:ceiling(n/1000)){
      tmp <- X[(1000*(i-1)+1):min(1000*i,n),,drop=FALSE]
      if(dim(tmp)[1] < 1000) {
        #pad the block with 0s to size 1000
        pad <- rbind(tmp,matrix(0,nrow=1000-dim(tmp)[1],ncol=p)) 
        Q[,,i] <- pad
        flag <- 1 #(final) block contains less than 1000 objects
        realsize <- dim(tmp)[1]
      }
      else Q[,,i] <- tmp
    }

    #each block within matrices 'corrected_order' and 'dist' is reduced to k columns.
    corrected_order <- matrix(NA,nrow=1000*ceiling(n/1000),ncol=k*ceiling(n/1000))
    dist <- matrix(NA,nrow=1000*ceiling(n/1000),ncol=k*ceiling(n/1000)) 
    for (i in 1:(ceiling(n/1000))){
      if(verbose) print(paste("training: batch...",ceiling(n/1000)-i)) 
      for (j in 1:(ceiling(n/1000))){
        block <- cdist(Q[,,i],Q[,,j],metric="euclidean")
        if (j == ceiling(n/1000) & flag == 1) {
          #block has less than 1000 objects
          o <- t(apply(block[,1:realsize],1,order,decreasing=FALSE))
          sorted <- t(apply(block[,1:realsize],1,sort,decreasing=FALSE))
          mn <- min(k,realsize) #in case last block contains less than k (non-padded) objects
          corrected_order[(1000*(i-1)+1):(1000*i),(1+((j-1)*k)):(mn+((j-1)*k))] <- o[,1:mn,drop=FALSE] + (1000*(j-1))
          dist[(1000*(i-1)+1):(1000*i),(1+((j-1)*k)):(mn+((j-1)*k))] <- sorted[,1:mn,drop=FALSE]
        } 
        else {
          #block is 1000 x 1000
          o <- t(apply(block,1,order,decreasing=FALSE))
          sorted <- t(apply(block,1,sort,decreasing=FALSE))
          corrected_order[(1000*(i-1)+1):(1000*i),(1+((j-1)*k)):(k+((j-1)*k))] <- o[,1:k,drop=FALSE] + (1000*(j-1))
          dist[(1000*(i-1)+1):(1000*i),(1+((j-1)*k)):(k+((j-1)*k))] <- sorted[,1:k,drop=FALSE]
        }
      }
    }
    #smallest k distances overall, out of finalist groups containing k candidates each
    if (dim(dist)[2]==1) {
      O <- matrix(1,nrow=nrow(dist),ncol=1)
    } 
    else {
      O <- t(apply(dist,1,order,decreasing=FALSE))[,2:k,drop=FALSE]
    }
    nn <- matrix(NA,nrow=dim(O)[1],ncol=k-1)
    for (i in 1:dim(O)[1]){
      nn[i,] <- corrected_order[i,O[i,],drop=FALSE] #order of nearest k neighbors in full distance matrix
    }
    nn <- nn[1:n,,drop=FALSE] #remove part corresponding to artificially padded section
    Y <- matrix(NA,n,p)
    for (i in 1:n){
      Y[i,] <- apply(X[nn[i,], ,drop=FALSE],2,median) #compute median feature values   
    }
  }
  #otherwise if n<=1000 objects, compute full distance matrix in one shot.
  else {
    DistMat <- pdist(X,metric="euclidean") #distance matrix 
    Y <- matrix(NA,n,p)
    for (i in 1:n){
      ordered_neighbors <- order(DistMat[i, ])
      nn <- ordered_neighbors[2:k] #include object itself as 1st nearest neighbor     
      Y[i,] <- apply(X[nn, ,drop=FALSE],2,median)    
    }
  }
  return(Y)
}
  