l <- gausscirc(ntrn_inlier = 200, ntst_inlier =  100, ntst_outlier = 100, 
                show = TRUE, seed=1)

#l <- boomerang(Ntrn = c(400,400), Ntst = c(200,200), ntst_outlier = 400, s=0.7, 
#               show =TRUE, seed=1)
xtrn <- l[[1]]
xtst <- l[[2]]

#Split test set into validation set and final test set.
inliers <- which(xtst[,ncol(xtst)]==0)
outliers <- which(xtst[,ncol(xtst)]==1)
ind_in <- sample(inliers,floor(0.5*length(inliers))) #50% stratified split
ind_out <- sample(outliers,floor(0.5*length(outliers))) 
xval <- xtst[c(ind_in,ind_out),]
xtst <- xtst[-c(ind_in,ind_out),]

#Optimize fracrej parameter using validation set
R <- seq(1,0,length.out = 500) #fracrej in [0,1]
error_inliers <- error_outliers <- numeric(0) 
for (i in seq_along(R)){
  l <- gaussocc(xtrn,xval,fracrej = R[i])
  L <- l[[1]]
  error_outliers[i] <- sum(L==0 & xval[,ncol(xval)]==1)
  error_inliers[i] <- sum(L==1 & xval[,ncol(xval)]==0)
}

#Plot ROC curve
y <- 1 - (error_inliers / sum(xval[,ncol(xval)]==0)) #frac targets accepted
x <- (error_outliers / sum(xval[,ncol(xval)]==1)) #frac outliers accepted             
plot(x,y,type="l",lwd=2,xlab="fraction outliers accepted",ylab="fraction targets accepted") #ROC curve

#Find optimal point on ROC closest to upper left corner i.e. (0,1)
index <- which.min(x^2 + (y-1)^2)
points(x[index],y[index],pch=1,col="red",lwd=2)
fracrej_opt <- R[index] #optimal fracrej 
paste("Optimal fracrej =",fracrej_opt)

#run optimal model and test
if (!require("e1071")) install.packages("e1071") 
if (!require("caret")) install.packages("caret") 
library(e1071)
library(caret)

out <- gaussocc(xtrn, xtst, fracrej = fracrej_opt)
pred <- out[[1]]
cf <- confusionMatrix(factor(pred,levels=c(0,1)),factor(xtst[,ncol(xtst)],levels=c(0,1)))
print(cf)

#visualize decision boundary and training set
viz(data = xtrn, classifier = "gauss", fracrej = fracrej_opt, resolution = 100)

#Add final test objects (inliers and outliers)
shape <- c(20,17) #dot, triangle
points(xtst[,1:2],col="violetred",pch=shape[xtst[,3]+1]) #add test objects
legend("bottomright",legend = c("training inlier","test inlier","test outlier"), col = c("blue","violetred","violetred"), pch = c(20,20,17), cex=0.8)

