#l <- gausscirc(ntrn_inlier = 200, ntst_inlier =  100, ntst_outlier = 100, 
#              show = TRUE, seed=1)

l <- boomerang(Ntrn = c(200,200), Ntst = c(100,100), ntst_outlier = 200, s=0.7, 
               show =TRUE, seed=1)
xtrn <- l[[1]]
xtst <- l[[2]]

#Split test set into validation set and final test set.
inliers <- which(xtst[,ncol(xtst)]==0)
outliers <- which(xtst[,ncol(xtst)]==1)
ind_in <- sample(inliers,floor(0.5*length(inliers))) #50% stratified split
ind_out <- sample(outliers,floor(0.5*length(outliers))) 
xval <- xtst[c(ind_in,ind_out),]
xtst <- xtst[-c(ind_in,ind_out),]

if (!require("e1071")) install.packages("e1071") 
if (!require("caret")) install.packages("caret") 
library(e1071)
library(caret)

#Optimize hyperparameters
steps <- seq(from = -15, to = 3, by = 2)
gamma <- 2^steps #range of values for gamma
nu <- seq(from = 0.05, to = 0.95, by = 0.05) #range of values for nu
error <- matrix(NA,nrow=length(gamma),ncol = length(nu))

for (i in seq_along(gamma)){
  for (j in seq_along(nu)){
    model <- svm(label ~ f1 + f2, data = xtrn, type="one-classification",
                 nu=nu[j],kernel="radial",gamma = gamma[i])
    pred <- predict(model, xval[,1:2]) #create predictions
    cf <- confusionMatrix(factor(as.numeric(!pred),levels=c(0,1)),factor(xval[,3],levels=c(0,1)))
    t <- cf$table
    TN <- t[1]; TP <- t[4]
    FN <- t[3]; FP <- t[2]
    FPR <- FP/(FP+TN)
    FNR <- FN/(FN+TP)
    if(is.nan(FPR)) FPR = 0
    if(is.nan(FNR)) FNR = 0
    error[i,j] <- sqrt(FPR^2 + FNR^2)  #false-rate error (i.e. distance to ROC upper left corner) 
    #error[i,j] <- (FPR+FNR)/2   #balanced error
  }
}

loc <- which(error == min(error), arr.ind = TRUE)
g <- gamma[loc[1]]
n <- nu[loc[2]]

#model using optimal parameters
model <- svm(label ~ f1 + f2, data = xtrn, type="one-classification",
             nu=n,kernel="radial",gamma = g)
print(model)

#Test the model on test set
pred <- predict(model, xtst[,1:2]) #create predictions on test set
cf <- confusionMatrix(factor(as.numeric(!pred),levels=c(0,1)),factor(xtst[,3],levels=c(0,1)))
print(cf)

#probabilistic output
out <- predict(model,xtst[,1:2], decision.values = TRUE) #positive distances = inliers, negative distances = outliers
out <- attr(out,"decision.values")
R <- max(out) #largest inlier distance to boundary, which is necessarily closed (hypershere in kernel space)
p <- exp(log(2)/R*(out-R))   #exp. function over [-\infty,R], passing (0,1/2) and (R,1)

#visualize decision boundary and training set
vizSvm(model, data = xtrn, resolution = 100)

#add final test objects (inliers and outliers)
shape <- c(20,17) #dot, triangle
points(xtst[,1:2],col="violetred",pch=shape[xtst[,3]+1]) #add test objects
legend("bottomright",legend = c("training inlier","test inlier","test outlier"), 
       col = c("blue","violetred","violetred"), pch = c(20,20,17), cex=0.8)

