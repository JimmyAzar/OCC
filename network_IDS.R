rm(list=ls())
data_trn <- read.table("kddcup.data_10_percent_corrected", sep = ",")
data_tst <- read.table("corrected", sep = ",")
#save(data_trn, file = "train.RData")
#save(data_tst, file = "tst.RData")

data_trn[1,]

#rename features/columns
feature_names <- c("duration","protocol_type","service","flag","src_bytes","dst_bytes",
                   "land","wrong_fragment","urgent","hot","num_failed_logins","logged_in",
                   "num_compromised","root_shell","su_attempted","num_root",
                   "num_file_creations","num_shells","num_access_files",
                   "num_outbound_cmds","is_host_login","is_guest_login","count",
                   "srv_count","serror_rate", "srv_serror_rate","rerror_rate",
                   "srv_rerror_rate","same_srv_rate","diff_srv_rate","srv_diff_host_rate",
                   "dst_host_count","dst_host_srv_count","dst_host_same_srv_rate",
                   "dst_host_diff_srv_rate","dst_host_same_src_port_rate",
                   "dst_host_srv_diff_host_rate","dst_host_serror_rate",
                   "dst_host_srv_serror_rate","dst_host_rerror_rate",
                   "dst_host_srv_rerror_rate","attack")
colnames(data_trn) <- feature_names
colnames(data_tst) <- feature_names

#extract unique labels
ltrn <- unique(data_trn[,"attack"]) #training labels
ltst <- unique(data_tst[,"attack"]) #test labels
print(ltrn) 
print(ltst)

#print frequency table for labels
df <- as.data.frame(table(data_trn[,"attack"]))
colnames(df) <- c("label","frequency")
print(df,row.names = TRUE)

df <- as.data.frame(table(data_tst[,"attack"]))
colnames(df) <- c("label","frequency")
print(df,row.names = TRUE)

#add binary label for normal (0) or anomaly
Lb <- as.numeric(data_trn$attack != "normal.")
data_trn$Lb <- Lb
print(as.data.frame(table(Lb)), row.names = FALSE)

Lb <- as.numeric(data_tst$attack != "normal.")
data_tst$Lb <- Lb
print(as.data.frame(table(Lb)), row.names = FALSE)

#count duplicate rows/observations
d <- duplicated(data_trn)
length(d) #number of obs.
sum(d) #number of duplicates
distinct_trn <- length(d) - sum(d)

d_attack <- duplicated(data_trn[data_trn$Lb==1,])
length(d_attack)
sum(d_attack)
distinct_trn_attack <- length(d_attack) - sum(d_attack)

d_normal <- duplicated(data_trn[data_trn$Lb==0,])
length(d_normal)
sum(d_normal)
distinct_trn_normal <- length(d_normal) - sum(d_normal)

#print table summary
df <- data.frame(original_obs = c(length(d_attack),length(d_normal),length(d)), 
                 distinct_obs = c(distinct_trn_attack,distinct_trn_normal,distinct_trn))
df$reduction_rate <- (df$original_obs - df$distinct_obs)/df$original_obs * 100
row.names(df) <- c("Attacks","Normal","Total")
print(df)

#repeat for test set
d <- duplicated(data_tst)
length(d)
sum(d)
distinct_tst <- length(d) - sum(d)

d_attack <- duplicated(data_tst[data_tst$Lb==1,])
length(d_attack)
sum(d_attack)
distinct_tst_attack <- length(d_attack) - sum(d_attack)

d_normal <- duplicated(data_tst[data_tst$Lb==0,])
length(d_normal)
sum(d_normal)
distinct_tst_normal <- length(d_normal) - sum(d_normal)

#print table summary
df <- data.frame(original_obs = c(length(d_attack),length(d_normal),length(d)), 
                 distinct_obs = c(distinct_tst_attack,distinct_tst_normal,distinct_tst))
df$reduction_rate <- (df$original_obs - df$distinct_obs)/df$original_obs * 100
row.names(df) <- c("Attacks","Normal","Total")
print(df)

#remove duplicate rows/observations
d <- duplicated(data_trn)
data_trn <- data_trn[-which(d),]

d <- duplicated(data_tst)
data_tst <- data_tst[-which(d),]

#One-hot encoding of nominal features
if (!require("e1071")) install.packages("e1071") 
if (!require("caret")) install.packages("caret") 
library(e1071)
library(caret)

Strn <- data_trn[,-42] #remove attack var
Stst <- data_tst[,-42] #remove attack var
flg <- dim(Strn)[1]
Comb <- rbind(Strn,Stst)
dmy <- dummyVars(" ~ .", data = Comb)
Comb <- data.frame(predict(dmy, newdata = Comb))
colnames(Comb)

Strn <- Comb[1:flg,]
Stst <- Comb[-c(1:flg),]

#Binary classification w/ classifier

# #Split training set into validation set and training set.
# inliers <- which(Strn[,ncol(Strn)]==0)
# outliers <- which(Strn[,ncol(Strn)]==1)
# ind_in <- sample(inliers,floor(0.5*length(inliers))) #50% stratified split
# ind_out <- sample(outliers,floor(0.5*length(outliers))) 
# xtrn <- Strn[c(ind_in,ind_out),]
# xval <- Strn[-c(ind_in,ind_out),]
# xtst <- Stst 

if (!require("rpart")) install.packages("rpart")
library("rpart")
# model <- rpart(as.factor(xtrn$Lb) ~., data=xtrn)
# summary(model)
# predictions <- predict(model, xval[,-ncol(xval)], type = "class")
# confusionMatrix(predictions, as.factor(xval[,ncol(xval)]))

xtrn <- Strn
xtst <- Stst
model <- rpart(as.factor(xtrn$Lb) ~., data=xtrn)
predictions <- predict(model, xtst[,-ncol(xtst)], type = "class")
confusionMatrix(predictions, as.factor(xtst[,ncol(xtst)]))

# #Fitting the Naive Bayes model
# model <- naiveBayes(as.factor(Strn$Lb) ~., data=Strn)
# summary(model)
# predictions <- predict(model, Stst[,-ncol(Stst)])
# table(predictions, Stst[,ncol(Stst)])

#if (!require("randomForest")) install.packages("randomForest")
#library("randomForest")
#fit <- randomForest(Lb~., data=Strn)
#summary(fit)
#predictions <- predict(fit, Stst[,-ncol(Stst)])
#table(predictions, Stst[,ncol(Stst)])

#PCA-visualize training set in 3D
n <- dim(Strn)[1]
a <- Strn[,-ncol(Strn)]
s <- apply(a,2,sd)
constFeatures <- which(s==0)
a <- a[,-c(constFeatures)]
a <- (a-matrix(1,n,1)%*%apply(a,2,mean))/(matrix(1,n,1)%*%apply(a,2,sd))
b <- prcomp(a, retx = TRUE)#, center = TRUE, scale. = TRUE)
rv <- cumsum(b$sdev^2) / sum(b$sdev^2)
plot(rv,type="l",xlab="number of features",ylab="ratio of retained variance to total variance")
plot(b$x[,1],b$x[,2],col = xtrn$Lb+1)
X <- as.data.frame(b$x[,1:3])
X$Lb <- Strn$Lb

if (!require("rgl")) install.packages("rgl")
library("rgl")
plot3d(X[,1],X[,2],X[,3],col=X[,4]+1)

#Multi-class (Normal,DOS,Probe,R2L,U2R) classification      

#class indices (training set)
l <- data_trn[,"attack"]
iNormal <- l =="normal."
iProbe <- l %in% c("satan.","ipsweep.","nmap.",
                   "portsweep.","mscan.","saint.")
iDoS <- l %in% c("back.","land.","neptune.","pod.","smurf.",
                 "teardrop.","apache2.",".udpstorm.","processtable.","mailbomb.")
iU2R <- l %in% c("buffer_overflow.","loadmodule.","rootkit.","perl.","sqlattack.",
                 "xterm.","ps.")
iR2L <- l %in% c("guess_passwd.","ftp_write.","imap.","phf.","multihop.",
                 "warezmaster.","warezclient.","spy.","xlock.","xsnoop.",
                 "snmpguess.","snmpgetattack.","httptunnel.","sendmail.",
                 "named.","worm.")

A <- data_trn[,-43]
labels <- rep(NA,dim(A)[1]) #initialize labels column
labels[iNormal] <- 0
labels[iProbe] <- 1
labels[iDoS] <- 2
labels[iU2R] <- 3
labels[iR2L] <- 4
A$label <- labels

df <- as.data.frame(table(A$label))
colnames(df) <- c("Label", "Frequency")
rownames(df) <- c("Normal", "Probe","DoS","U2R","R2L")
print(df)

#class indices (test set)
l <- data_tst[,"attack"]
iNormal <- l =="normal."
iProbe <- l %in% c("satan.","ipsweep.","nmap.",
                   "portsweep.","mscan.","saint.")
iDoS <- l %in% c("back.","land.","neptune.","pod.","smurf.",
                 "teardrop.","apache2.",".udpstorm.","processtable.","mailbomb.")
iU2R <- l %in% c("buffer_overflow.","loadmodule.","rootkit.","perl.","sqlattack.",
                 "xterm.","ps.")
iR2L <- l %in% c("guess_passwd.","ftp_write.","imap.","phf.","multihop.",
                 "warezmaster.","warezclient.","spy.","xlock.","xsnoop.",
                 "snmpguess.","snmpgetattack.","httptunnel.","sendmail.",
                 "named.","worm.")

B <- data_tst[,-43]
labels <- rep(NA,dim(B)[1]) #initialize labels column
labels[iNormal] <- 0
labels[iProbe] <- 1
labels[iDoS] <- 2
labels[iU2R] <- 3
labels[iR2L] <- 4
B$label <- labels

df <- as.data.frame(table(B$label))
colnames(df) <- c("Label", "Frequency")
rownames(df) <- c("Normal", "Probe","DoS","U2R","R2L")
print(df)

#One-hot encoding of nominal features
ta <- A[,-c(42,43)] #remove labels
tb <- B[,-c(42,43)] #remove labels
flg <- dim(ta)[1]
Comb <- rbind(ta,tb)
dmy <- dummyVars(" ~ .", data = Comb)
Comb <- data.frame(predict(dmy, newdata = Comb))
colnames(Comb)

tA <- Comb[1:flg,]
tA$label <- A$label 
tB <- Comb[-c(1:flg),]
tB$label <- B$label

#Classification tree
xtrn <- tA
xtst <- tB

#Weigh against class sizes
w0 <- sum(xtrn$label == 0) / nrow(xtrn)
w1 <- sum(xtrn$label == 1) / nrow(xtrn)
w2 <- sum(xtrn$label == 2) / nrow(xtrn)
w3 <- sum(xtrn$label == 3) / nrow(xtrn)
w4 <- sum(xtrn$label == 4) / nrow(xtrn)

W <- rep(0,nrow(xtrn))
W[(xtrn$label == 0)] <- ceiling(1/w0) 
W[(xtrn$label == 1)] <- ceiling(1/w1) 
W[(xtrn$label == 2)] <- ceiling(1/w2)
W[(xtrn$label == 3)] <- ceiling(1/w3)
W[(xtrn$label == 4)] <- ceiling(1/w4) 

model <- rpart(as.factor(xtrn$label) ~., data=xtrn, weights = W)
predictions <- predict(model, xtst[,-ncol(xtst)], type = "class")
confusionMatrix(predictions, as.factor(xtst[,ncol(xtst)]))

