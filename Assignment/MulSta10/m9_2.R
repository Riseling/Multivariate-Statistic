rm(list =ls())
library(rpart)
library(rpart.plot)
library(ggplot2)
library(ggpubr)

# build the tree
cleveland <- read.table("cleveland.txt", header = TRUE,stringsAsFactors = T)
cleveland <- cleveland[,-15]
out <- rpart(diag~ .,data = cleveland)
out
rpart.plot(out,type = 1,extra = 1,roundint = FALSE)
summary(out)

## misclassification rate
prediction <- predict(out,data=cleveland,type = "class")
table(cleveland$diag,prediction)
mean(cleveland$diag!=prediction)

# The best split of age
Age <- sort(cleveland$age)
L_Buff <- c(); L_Sick <- c();B_Buff <- c();B_Sick <- c() # initialize
for (i in Age) {
  L_Buff <- c(L_Buff,sum(cleveland$diag[cleveland$age <= i]=='buff'))
  L_Sick <- c(L_Sick,sum(cleveland$diag[cleveland$age <= i]=='sick'))
  B_Buff <- c(B_Buff,sum(cleveland$diag[cleveland$age > i]=='buff'))
  B_Sick <- c(B_Sick,sum(cleveland$diag[cleveland$age > i]=='sick'))
} # storage all situations of split

tao_l <- 1 - (L_Buff/(L_Buff+L_Sick))^2 - (L_Sick/(L_Buff+L_Sick))^2 # impurity of left node
tao_r <- 1 - (B_Buff/(B_Buff+B_Sick))^2 - (B_Sick/(B_Buff+B_Sick))^2 # impurity of right node
tao <- 1-((L_Buff+B_Buff)/nrow(cleveland))^2 - ((L_Sick+B_Sick)/nrow(cleveland))^2 # impurity of parent node
delta <- tao - tao_l*((L_Buff+L_Sick)/nrow(cleveland)) - tao_r*((B_Buff+B_Sick)/nrow(cleveland)) # goodness-of-split

## plot
par(mfrow=c(1,2))
plot(Age,tao_l, type="l", col="blue",
     xlab="Age at split", ylab = expression(i(tau)))
points(Age,tao_r, type="l", col="red")
legend("bottom",legend = c("Left","Right"), 
       lty = 1,col=c("blue","red"),
       )

plot(Age,delta,type="l", col="red", 
     xlab="Age at split", ylab = "Goodness of split")

Age[which.max(delta)]

## ggplot2

p1 <- ggplot(data=NULL,aes(x=Age,y=tao_l))+ 
  geom_line(color="blue") + 
  geom_line(y=tao_r,color="red") +
  labs(x="Age at split", y = expression(i(tau)))+ 
  guides(fill=guide_colourbar(title="sad",
                              label = T,
                              label.position = 'bottom'))
  
p1  





