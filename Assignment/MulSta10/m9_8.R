rm(list =ls())
library(MASS)
library(rpart)
library(rpart.plot)

vehicle <- read.table("vehicle3.txt", header = T)
vehicle <- vehicle[,-20]
vehicle$class <- factor(vehicle$class)

# partitioning
set.seed(1234)
ind <- sample(2,nrow(vehicle),replace = T,prob = c(0.7,0.3))
traintset <- vehicle[ind==1,]
testset <- vehicle[ind==2,]

# build the tree
ct_vehicle <- rpart(class~ .,data = traintset)
ct_vehicle
#summary(ct_vehicle)

## plot using rapart style
rpart.plot(ct_vehicle,type = 1,extra = 1,roundint = FALSE,
           cex=0.5,split.cex=1.4,legend.x = NA)

## plot
plot(ct_vehicle,uniform = T,branch = 0.6,margin = 0.1)
text(ct_vehicle,all = T,use.n = T,cex = 0.75,pos=3)

## misclassification
re <- predict(ct_vehicle, testset, type = "class")
mean(testset$class!=re)

## complexity parameter
printcp(ct_vehicle) 
plotcp(ct_vehicle)

## prune, in this case, not necessary
pr_cp <- ct_vehicle$cptable[,1]
prune_tree <- prune(ct_vehicle,cp=pr_cp[7])
prediction <- predict(prune_tree,testset, type = "class")
mean(testset$class!=prediction) # misclassification rate(prune)
plot(prune_tree,uniform = T,branch = 0.6,margin = 0.1)
text(prune_tree,all = T, use.n = T, cex = 0.75,pos=3)
