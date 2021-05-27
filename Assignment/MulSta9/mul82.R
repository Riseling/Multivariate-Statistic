library(MASS)
library(ggplot2)
library(ggpubr)

wine <- read.table("wine.train.txt")  # 最后一列表示酒的种类

lda_wine <- lda(V14~., data = wine)

# 预测结果
result <- predict(lda_wine, wine)
## 作图
pre_wine <- data.frame(result$x, type = result$class)

p1 <- ggplot(pre_wine, aes(x = LD1,y = LD2,colour = type)) + 
  geom_point() # 预测

p2 <- ggplot(pre_wine, aes(x = LD1,y = LD2,colour = factor(wine$V14))) + 
  geom_point() # 实际

ggarrange(p1,p2,nrow = 1, ncol = 2, 
          common.legend = T, legend = "bottom")
# 结果分析
table(wine$V14, result$class)
mean(result$class!=wine$V14)  # 误差率









