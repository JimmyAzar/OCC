tune_lof <- function(a_train,a_val,fracrej,k,verbose=TRUE){
  #a_train: labeled training set as data frame (0=inlier, 1=outlier)
  #a_val: labeled validation set as data frame
  #fracrej: range of values for reject fraction in [0,1]
  #k: range of values for number of neighbors 
  
  err <- matrix(NA,nrow=length(fracrej),ncol=length(k))
  Yl <- a_train[a_train[,ncol(a_train)]==0,]
  x <- as.data.frame(Yl)
  y <- as.data.frame(a_val)
  colnames(y) <- colnames(x)
  for (j in seq_along(fracrej)){
    if(verbose) print(paste("counter:...",length(fracrej)-j))
      out <- lof_wrapper(x, y, k = k, fracrej = fracrej[j], cores=1)
      for (l in seq_along(k)){
        pred <- out[[l]]

        FP <- sum(pred==1 & a_val[,ncol(a_val)]==0)
        TN <- sum(pred==0 & a_val[,ncol(a_val)]==0)
        FN <- sum(pred==0 & a_val[,ncol(a_val)]==1)
        TP <- sum(pred==1 & a_val[,ncol(a_val)]==1)
      
        error <- (FP/(FP+TN)) + (FN/(FN+TP)) #= FPR + FNR
        error = error/2
        err[j,l] <- error
    }
  }
  
  ind_opt <- which(err==min(err), arr.ind = TRUE)
  if (is.array(ind_opt)){
    fracrej_opt <- fracrej[ind_opt[1,1]]
    k_opt <- k[ind_opt[1,2]]
  }
  else {
    fracrej_opt <- fracrej[ind_opt[1]]
    k_opt <- k[ind_opt[2]]
  }
  
  l = list(fracrej_opt=fracrej_opt,k_opt=k_opt)
  return(l)
}