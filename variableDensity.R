variableDensity <- function(ntrn_inlier=100, ntst_inlier=20, ntst_outlier=20, show=TRUE, seed=1){
  #Generates two circularly symmetric Gaussian distributions in 2D with variable density.
  #The training set consists only of inliers; their number is ntrn_inliers objects per distribution.
  #The test set may consists of inliers (ntst_inlier objects per distribution) and should consist of outliers (ntst_outlier objects) drawn randomly from a uniform distribution.
  #Inliers are assumed to have a '0' label while outliers have a '1' label. 
  
  if (!require("MASS")) install.packages("MASS") 
  library("MASS")
  
  #generate training inliers
  set.seed(seed)
  x1 <- mvrnorm(ntrn_inlier, mu = c(-3,-3), Sigma = 0.3*matrix(c(1,0,0,1),nrow = 2))
  x2 <- mvrnorm(ntrn_inlier, mu = c(3,3), Sigma = 3*matrix(c(1,0,0,1),nrow = 2))
  xin <- rbind(x1,x2)
  xin <- cbind(xin,rep(0,dim(xin)[1])) #add '0' labels for inliers
  
  #generate test inliers (if any)
  if (ntst_inlier!=0){
    set.seed(seed+1)
    z1 <- mvrnorm(ntst_inlier, mu = c(-3,-3), Sigma = 0.3*matrix(c(1,0,0,1),nrow = 2))
    z2 <- mvrnorm(ntst_inlier, mu = c(3,3), Sigma = 3*matrix(c(1,0,0,1),nrow = 2))
    zin <- rbind(z1,z2)
    zin <- cbind(zin,rep(0,dim(zin)[1])) #add '0' labels for inliers
  }
  else{zin <- numeric(0)}
  
  #limits
  low <- min(min(rbind(xin,zin)))
  high <- max(max(rbind(xin,zin)))
  #generate test outliers from uniform distribution
  set.seed(seed)
  f1 <- runif(ntst_outlier, min = low, max = high) #uniform distribution for feature 1
  set.seed(seed+1)
  f2 <- runif(ntst_outlier, min = low, max = high) #uniform distribution for feature 2
  zout <- cbind(f1,f2,rep(1,ntst_outlier)) 
  
  xtrn <- as.data.frame(xin)
  colnames(xtrn) <- c("f1","f2","label")
  
  xtst <- as.data.frame(rbind(zin,zout)) 
  colnames(xtst) <- c("f1","f2","label")
  
  if(show){
    plot(xin,col="blue",pch=20,xlab = "feature 1", ylab = "feature 2",
         xlim = c(low,high), ylim = c(low,high))
    points(zin,pch=20,col="violetred")
    points(zout,pch=17,col="violetred")
    legend("bottomright",legend = c("training inlier","test inlier","test outlier"),
           col = c("blue","violetred","violetred"), pch = c(20,20,17), cex=0.8)
  }
  
  l = list(xtrn,xtst) #create list of outputs
  return(l)
}
