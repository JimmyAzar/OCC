tune_svm <- function(a_train,a_val,nu,gamma,verbose=TRUE){
  #a_train: labeled training set as data frame (0=inlier, 1=outlier)
  #a_val: labeled validation set as data frame
  #nu: range of values in ]0,1[
  #gamma: range of values for gamma
  
  err <- matrix(NA,nrow=length(nu),ncol=length(gamma))
  Yl <- a_train[a_train[,ncol(a_train)]==0,]
  x <- as.data.frame(Yl)
  y <- as.data.frame(a_val)
  colnames(y) <- colnames(x)
  for (j in seq_along(nu)){
    if(verbose) print(paste("counter:...",length(nu)-j))
    for (l in seq_along(gamma)){
      model <- svm(x = x[,-ncol(x)], y = x[,ncol(x)], type="one-classification",
                   kernel="radial", nu=nu[j], gamma = gamma[l])
      pred <- predict(model, y[,-ncol(y)])
      pred <- as.numeric(!pred)
      #err[i,j,l] <- sum(pred!=y[,ncol(y)])
        
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
    nu_opt <- nu[ind_opt[1,1]]
    gamma_opt <- gamma[ind_opt[1,2]]
  }
  else {
    nu_opt <- nu[ind_opt[1]]
    gamma_opt <- gamma[ind_opt[2]]
  }
  
  l = list(nu_opt=nu_opt,gamma_opt=gamma_opt)
  return(l)
}
