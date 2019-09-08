gausscirc <- function(ntrn_inlier=100, ntst_inlier=20, ntst_outlier=20, show=TRUE, seed=1){
  #Generates two circularly symmetric Gaussian distributions in 2D with zero mean and with the identity matrix as covariance matrix.
  #The training set consists only of inliers; their number is ntrn_inliers objects per distribution.
  #The test set may consists of inliers (ntst_inlier objects per distribution) and should consist of outliers (ntst_outlier objects per distribution) drawn randomly from a uniform distribution.
  #Inliers are assumed to have a '0' label while outliers have a '1' label. 
  
  if (!require("MASS")) install.packages("MASS") 
  library("MASS")
  
  #generate training inliers
  set.seed(seed)
  x <- 0.3*mvrnorm(ntrn_inlier, mu = c(0,0), Sigma = matrix(c(1,0,0,1),nrow = 2))
  xin <- rbind(x+2,x-2)
  xin <- cbind(xin,rep(0,ntrn_inlier)) #add '0' labels for inliers
  
  #generate test inliers (if any)
  if (ntst_inlier!=0){
    set.seed(seed+1)
    z <- 0.3*mvrnorm(ntst_inlier, mu = c(0,0), Sigma = matrix(c(1,0,0,1),nrow = 2))
    zin <- rbind(z+2,z-2)
    zin <- cbind(zin,rep(0,ntst_inlier)) #add '0' labels for inliers
  }
  else{zin <- numeric(0)}
  
  #generate test outliers from uniform distribution between [-5,5]
  set.seed(seed)
  f1 <- runif(ntst_outlier, min = -5, max = 5) #uniform distribution for feature 1
  set.seed(seed+1)
  f2 <- runif(ntst_outlier, min = -5, max = 5) #uniform distribution for feature 2
  zout <- cbind(f1,f2,rep(1,ntst_outlier)) 
  
  xtrn <- as.data.frame(xin)
  colnames(xtrn) <- c("f1","f2","label")

  xtst <- as.data.frame(rbind(zin,zout)) 
  colnames(xtst) <- c("f1","f2","label")

  if(show){
    plot(xin,col="blue",pch=20,xlab = "feature 1", ylab = "feature 2",xlim = c(-5.2,5.2), ylim = c(-5.2,5.2))
    points(zin,pch=20,col="violetred")
    points(zout,pch=17,col="violetred")
    legend("bottomright",legend = c("training inlier","test inlier","test outlier"),
           col = c("blue","violetred","violetred"), pch = c(20,20,17), cex=0.8)
  }
  
  l = list(xtrn,xtst) #create list of outputs
  return(l)
}


