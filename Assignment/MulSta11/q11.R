library(MASS)
library(dplyr)
library(rpart)
library(e1071)

wdbc <- read.table("wdbc.txt",sep=",",stringsAsFactors = TRUE)
wdbc[wdbc==0] <- 0.001
wdbc[,c(-1,-2)] <- apply(X = wdbc %>% select(-V1, -V2),
                         FUN = log,MARGIN = 2)
wdbc <- wdbc[,-1]

# simple test, can ignore
## SVM with cross validation default
tune.svm(V2~., data = wdbc, cost = C, gamma = gam)
C <- c(10,80,100,200,500,1000)
gam <- c(0.00001,0.0001,0.001,0.01,0.1,0.4)
cv <- matrix(0, nrow=6, ncol=6)
for (i in 1:6) {
  for (j in 1:6) {
    svm_wdbc <- svm(V2~.-V1, data = wdbc, kernel="radial", 
                    cost = C[i], gamma = gam[j] ,cross = 10)
    cv[i,j] <- (100-svm_wdbc$tot.accuracy)/100
  }  
}
which(cv==min(cv),arr.ind = T)


# LDA
lda_wdbc <- lda(V2~.-V1,data = wdbc)
pre <- predict(lda_wdbc, data = wdbc)
mean(pre$class!=wdbc$V2) # misclassification rate

# classification tree
tre <-  rpart(V2~.-V1, data=wdbc)
res <- predict(tre,data=wdbc,type="class")
mean(res!=wdbc$V2) # misclassification rate

# more detail 
## SVM CV 10-fold
C <- c(10,80,100,200,500,1000)
gam <- c(0.00001,0.0001,0.001,0.01,0.04)

set.seed(1234)
N <- nrow(wdbc)
index <- sample(1:N, N, replace = F)

CV <- matrix(0, nrow = 6, ncol = 5) # 初始化，储存错误率
rownames(CV) <- c(10,80,100,200,500,1000)
colnames(CV) <- c(0.00001,0.0001,0.001,0.01,0.04)
CV <- as.data.frame(CV)

for (k in 1:6) {
  for (t in 1:5) {
    for (i in 1:10) {
    id <- (57*(i-1)+1):(57*i)
    valid_index <- index[id]
    train_index <- index[-id]
  
    train <- wdbc[train_index,]
    valid <- wdbc[valid_index,]
    valid <- na.omit(valid)
    out <- svm(V2~., data = train, kernel="radial", 
        cost = C[k], gamma = gam[t])
  
    pred <- predict(out, valid)
  
    A <- table(valid$V2, pred)
    CV[k,t] <- CV[k,t] +(sum(A)-sum(diag(A)))/nrow(valid)
    }
  CV[k,t] <- CV[k,t]/10
  }
}
CV
min(CV)
which(CV == min(CV), arr.ind=TRUE)

### margin plot eg. C=500, gamma=1e-04
train <- train[,-1]
out <- svm(V2~., data = train, kernel="radial", 
           cost = 500, gamma = 1e-04)
out
plot(out,data = train, V3~V4)

## LDA CV 10-fold
err_lda <-0
for (i in 1:10) {
  id <- (57*(i-1)+1):(57*i)
  valid_index <- index[id]
  train_index <- index[-id]
  
  train <- wdbc[train_index,]
  valid <- wdbc[valid_index,]
  valid <- na.omit(valid)
  out <- lda(V2~., data = train)
  
  pred <- predict(out, valid)
  
  A <- table(valid$V2, pred$class)
  err_lda<- err_lda +(sum(A)-sum(diag(A)))/nrow(valid)
}
err_lda<- err_lda/10
err_lda

## Classification tree CV 10-fold
err_t <-0
for (i in 1:10) {
  id <- (57*(i-1)+1):(57*i)
  valid_index <- index[id]
  train_index <- index[-id]
  
  train <- wdbc[train_index,]
  valid <- wdbc[valid_index,]
  valid <- na.omit(valid) # delete NA
  
  out <- rpart(V2~., method = "class",data = train)
  
  pred <- predict(out, valid, type="class")
  
  A <- table(valid$V2, pred)
  err_t<- err_t +(sum(A)-sum(diag(A)))/nrow(valid)
}
err_t <- err_t/10
err_t