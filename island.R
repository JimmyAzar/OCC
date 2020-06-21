island <- function(ntrn_inlier=c(100,5), ntst_inlier=c(100,5), ntst_outlier=20, show=TRUE, seed=1){
  #Generates two Gaussian distributions in 2D: a wide distribution and an island.  
  #The training set consists only of inliers;
  #The test set may consist of inliers and outliers (ntst_outlier objects) drawn randomly from a uniform distribution.
  #Inliers are assumed to have a '0' label, while outliers have a '1' label. 
  
  if (!require("MASS")) install.packages("MASS") 
  library("MASS")
  
  #generate training inliers
  set.seed(seed)
  x1 <- mvrnorm(ntrn_inlier[1], mu = c(-3,-3), Sigma = 3*matrix(c(1,0,0,1),nrow = 2))
  x2 <- mvrnorm(ntrn_inlier[2], mu = c(6,6), Sigma = 0.3*matrix(c(1,0,0,1),nrow = 2))
  xin <- rbind(x1,x2)
  xin <- cbind(xin,rep(0,dim(xin)[1])) #add '0' labels for inliers
  
  #generate test inliers (if any)
  if (sum(ntst_inlier)!=0){
    set.seed(seed+1)
    z1 <- mvrnorm(ntst_inlier[1], mu = c(-3,-3), Sigma = 3*matrix(c(1,0,0,1),nrow = 2))
    z2 <- mvrnorm(ntst_inlier[2], mu = c(6,6), Sigma = 0.3*matrix(c(1,0,0,1),nrow = 2))
    zin <- rbind(z1,z2)
    zin <- cbind(zin,rep(0,dim(zin)[1])) #add '0' labels for inliers
  }
  else{zin <- numeric(0)}
  
  #generate test outliers from uniform distribution between [-5,5]
  set.seed(seed)
  f1 <- runif(ntst_outlier, min = -10, max = 10) #uniform distribution for feature 1
  set.seed(seed+1)
  f2 <- runif(ntst_outlier, min = -10, max = 10) #uniform distribution for feature 2
  zout <- cbind(f1,f2,rep(1,ntst_outlier)) 
  
  xtrn <- as.data.frame(xin)
  colnames(xtrn) <- c("f1","f2","label")
  
  xtst <- as.data.frame(rbind(zin,zout)) 
  colnames(xtst) <- c("f1","f2","label")
  
  if(show){
    plot(xin,col="blue",pch=20,xlab = "feature 1", ylab = "feature 2",
         xlim = c(-10,10), ylim = c(-10,10), asp=1)
    points(zin,pch=20,col="violetred")
    points(zout,pch=17,col="violetred")
    legend("bottomright",legend = c("training inlier","test inlier","test outlier"),
           col = c("blue","violetred","violetred"), pch = c(20,20,17), cex=0.8)
  }
  l = list(xtrn,xtst) #create list of outputs
  return(l)
}
