tune_iforest <- function(a_train,a_val,fracrej,verbose=TRUE){
  #a_train: labeled training set as data frame (0=inlier, 1=outlier)
  #a_val: labeled validation set as data frame
  #fracrej: range of values for reject fraction in [0,1]
  
  err <- rep(NA,length(fracrej))
  Yl <- a_train[a_train[,ncol(a_train)]==0,]
  x <- as.data.frame(Yl)
  y <- as.data.frame(a_val)
  colnames(y) <- colnames(x)
  for (j in seq_along(fracrej)){
    if (verbose) print(paste("counter:...",length(fracrej)-j))
    out <- iForest(x,y,fracrej=fracrej[j],sample_size = dim(x)[1])
    pred <- out[[1]]

    FP <- sum(pred==1 & a_val[,ncol(a_val)]==0)
    TN <- sum(pred==0 & a_val[,ncol(a_val)]==0)
    FN <- sum(pred==0 & a_val[,ncol(a_val)]==1)
    TP <- sum(pred==1 & a_val[,ncol(a_val)]==1)
      
    error <- (FP/(FP+TN)) + (FN/(FN+TP)) #= FPR + FNR
    error = error/2
    err[j] <- error
  }

  ind_opt <- which(err==min(err))#, arr.ind = TRUE)
  fracrej_opt <- fracrej[ind_opt[1]]

  l = list(fracrej_opt=fracrej_opt)
  return(l)
}