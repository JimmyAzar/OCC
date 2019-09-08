parzenocc <- function(xtrn, xtst, h = seq(0.01,10,length.out=50), fracrej = 0.05){
  #Parzen one-class classifier
  #Training set contains only inliers.
  #'h' is the standard deviation of the Gaussian kernel (specify as a single value or a range of values). 
  #fracrej is the fraction of training objects to reject.
  
  if (!require("rdist")) install.packages("rdist") 
  library("rdist")
  
  p <- ncol(xtrn)-1 #number of features
  n <- nrow(xtrn) #number of training objects
  d <- as.matrix(xtrn[,-ncol(xtrn)])
  Ltrn <- xtrn[,ncol(xtrn)]

  if (length(h)>1){ #if range of values for h is given, then:
    #find optimal h by maximizing LL
    s <- sample(1:n,size = floor(2/3*n),replace = FALSE) #2/3 for fitting, 1/3 for measuring LL
    insample <- d[s,]
    outsample <- d[-s,]
    LL <- numeric(0)
    for (w in seq_along(h)){
      P <- numeric(0)
      insample <- as.matrix(insample)
      testDistMat <- t(cdist(insample,outsample,metric="euclidean")) #distance matrix; outsamples as rows, insamples as columns
      g <- 1/(h[w]*sqrt(2*pi))*exp(-0.5*(testDistMat/h[w])^2)
      P <- apply(g,1,mean) 
      LL[w] <- sum(log(P)) #log-likelihood 
    }
    h <- h[which(LL==max(LL))] #optimal h that maximizes the log-likelihood
  }
    
  #compute training object probabilities of being inlier
  Ptrn <- numeric(0)
  trainDataMatrix <- d
  trainDistMat <- pdist(trainDataMatrix,metric="euclidean") #distance matrix across training objects
  g <- 1/(h*sqrt(2*pi))*exp(-0.5*(trainDistMat/h)^2)
  Ptrn <- apply(g,1,mean) #sum kernel contributions and divide by the number of kernels (training objects)
  
  Ptrn <- h*sqrt(2*pi) * Ptrn  #prob. of inlier in [0,1]
  highestValue <- max(Ptrn)
  Ptrn <- Ptrn/highestValue
  
  #compute test object probabilities of being inlier
  Ptst <- numeric(0)
  colnames(xtst) <- NULL
  testDataMatrix <- as.matrix(xtst[,-ncol(xtst)])
  testDistMat <- t(cdist(trainDataMatrix,testDataMatrix,metric="euclidean")) #test objects as rows, training objects as columns
  g <- 1/(h*sqrt(2*pi))*exp(-0.5*(testDistMat/h)^2)
  Ptst <- apply(g,1,mean) #sum kernel contributions and divide by the number of kernels (training objects)
  
  Ptst <- h*sqrt(2*pi) * Ptst  #prob. of inlier in [0,1]
  Ptst <- Ptst/highestValue #normalize scale using the highest possible value in training set; (largest contribution in sum of Gaussians occurs at location of some training object, i.e. at mode, not in between)
  if (fracrej==0) thr <- 0 #set threshold = 0
  else if (fracrej==1) thr <- 1 #set threshold = 1
  else thr <- quantile(Ptrn,fracrej) #note! Ptrn
  Ltst <- as.numeric(Ptst < thr) #label as outlier if P < threshold 
  
  l <- list(Ltst,Ptst,h) #return a list of hard labels, soft labels, and h
  return(l)
}
