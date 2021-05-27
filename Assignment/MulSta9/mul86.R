library(MASS)
library(dplyr)
library(iris)
library(ggplot2)
data(iris)

# 数据变换
iris_adjust <- iris %>% mutate(Sepal.Shape = log(Sepal.Length/Sepal.Width),
                Petal.Shape = log(Petal.Length/Petal.Width))

# 转换后数据
boxplot(iris_adjust$Sepal.Shape, iris_adjust$Petal.Shape, 
        names = c("Sepal.Shape","Petal.Shape"))

# LDA
lda_iris <- lda(Species~Sepal.Shape+Petal.Shape, data = iris_adjust)
result <- predict(lda_iris, iris_adjust[,c(6,7)])
table(iris_adjust$Species, result$class)
mean(iris_adjust$Species!=result$class) # 误判率

# QDA
qda_iris <- qda(Species~Sepal.Shape+Petal.Shape, data = iris_adjust)
qda_result <- predict(qda_iris, iris_adjust[,c(6,7)])
table(iris_adjust$Species, qda_result$class)
mean(iris_adjust$Species!=qda_result$class) # 误判率

