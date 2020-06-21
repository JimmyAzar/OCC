iForest <- function(xtrn, xtst, fracrej, sample_size = 256){
  #isolation forest: 
  #xtrn: labeled training set, should contain inliers only
  #xtst: labeled test set can contain inliers and outliers
  #fracrej is the fraction of training objects to reject as outliers.
  #sample_size: number of objects used to build a tree in the forest
  
  if(!require("solitude")) install.packages("solitude")
  library("solitude")
  
  isf <- isolationForest$new(sample_size = sample_size)  # initiate
  x <- as.data.frame(xtrn[,-ncol(xtrn)])
  isf$fit(x)
  #isf$scores #anomaly scores 
  #scores closer to 1 => outliers
  #round(head(sort(isf$scores$anomaly_score, dec = TRUE), 20), 2)
  s <- isf$predict(x) # scores for training set
  ss <- sort(s$anomaly_score, decreasing = TRUE)
  nrej <- ceiling(fracrej * length(ss))
  if (fracrej==1) thr <- -Inf
  else if (fracrej==0) thr <- Inf
  else thr <- ss[nrej]

  y <- as.data.frame(xtst[,-ncol(xtst)])
  s <- isf$predict(y) # scores for test set
  out <- (s$anomaly_score > thr)
  out <- as.numeric(out)
  return(list(out))
}
