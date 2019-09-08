boomerang <- function(Ntrn=c(50,50), Ntst = c(20,20), ntst_outlier=100, s=1, 
                      show=TRUE,seed=1){
  #Generate a boomerang-shaped training set with the number of objects in each branch described by Ntrn.
  #The number on test set inliers in each branch is given by Ntst.
  #The number of test set outliers is ntst_outlier drawn randomly from a uniform distribution. 
  #Parameter s controls the width of the branches.
  
  if (!require("MASS")) install.packages("MASS") 
  library("MASS")
  
  r <- 5  #size of boomerang
  
  #generate training inliers
  set.seed(seed)
  domaina = 0.125*pi + runif(n = Ntrn[1], min = 0, max =1) * 1.25*pi
  t1 <- mvrnorm(n=Ntrn[1], mu = c(0,0), Sigma = matrix(c(1,0,0,1),nrow = 2))*s 
  a <- cbind(r*sin(domaina),r*cos(domaina)) + t1
  
  domainb = 0.375*pi - runif(n = Ntrn[2], min = 0, max = 1) * 1.25*pi
  t2 <- mvrnorm(n = Ntrn[2], mu = c(0,0), Sigma = matrix(c(1,0,0,1),nrow = 2))*s 
  b <-  cbind(r*sin(domainb),r*cos(domainb)) + t2 + matrix(c(-0.75*r,-0.75*r),nrow=Ntrn[2],ncol=2)
  
  #combine the two branches into a training set
  xin <- rbind(a,b)
  xin <- cbind(xin,rep(0,nrow(xin)))
  xtrn <- as.data.frame(xin)
  colnames(xtrn) <- c("f1","f2","label") 
  
  #get boundary limits
  mn <- apply(xtrn[,1:2],2,min)
  mx <- apply(xtrn[,1:2],2,max)
  xmin <- mn[1]; ymin <- mn[2]
  xmax <- mx[1]; ymax <- mx[2]
  low <- min(xmin,ymin)-1 #add margin
  high <- max(xmax,ymax)+1 #add margin
  
  #generate test inliers (if any)
  if (sum(Ntst)!=0){
    set.seed(seed+1)
    domaina = 0.125*pi + runif(n = Ntst[1], min = 0, max =1) * 1.25*pi
    t1 <- mvrnorm(n=Ntst[1], mu = c(0,0), Sigma = matrix(c(1,0,0,1),nrow = 2))*s 
    a <- cbind(r*sin(domaina),r*cos(domaina)) + t1
    
    domainb = 0.375*pi - runif(n = Ntst[2], min = 0, max = 1) * 1.25*pi
    t2 <- mvrnorm(n = Ntst[2], mu = c(0,0), Sigma = matrix(c(1,0,0,1),nrow = 2))*s 
    b <- cbind(r*sin(domainb),r*cos(domainb)) + t2 + matrix(c(-0.75*r,-0.75*r),nrow=Ntst[2],ncol=2)
    
    zin <- rbind(a,b)
    zin <- cbind(zin,rep(0,nrow(zin))) #add labels '0'
  }
  else{zin <- numeric(0)}
  
  #generate test outliers from uniform distribution
  set.seed(seed+2)
  f1 <- runif(ntst_outlier, min = low, max = high) #uniform distribution for feature 1
  set.seed(seed+3)
  f2 <- runif(ntst_outlier, min = low, max = high) #uniform distribution for feature 2
  zout <- cbind(f1,f2,rep(1,ntst_outlier)) #add labels '1'
  
  #combine into a test set
  xtst <- as.data.frame(rbind(zin,zout)) 
  colnames(xtst) <- c("f1","f2","label")
  
  if(show){
    plot(xin,col="blue",pch=20,xlab = "feature 1", ylab = "feature 2",xlim = c(low,high), ylim = c(low,high))
    points(zin,pch=20,col="violetred") #add test set inliers
    points(zout,pch=17,col="violetred") #add test set outliers
    legend("bottomright",legend = c("training inlier","test inlier","test outlier"),
           col = c("blue","violetred","violetred"), pch = c(20,20,17), cex=0.8)
  }
  
  l = list(xtrn,xtst) #create list of outputs
  return(l)
}
