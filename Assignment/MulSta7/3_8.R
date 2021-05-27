data <- read.table('Ex3.8_data.txt')
boy <- data[1:6,]
girl <- data[7:15,]
C <- matrix(c(1,0,-6,0,1,-4),nrow=2,byrow = T) #常数矩阵
n <- 6
m <- 9
p <- 3

m_b <- colMeans(boy)   #X的均值
m_y <- C %*% m_b   # Y的均值

A <- C %*% ((n-1)*cov(boy)) %*% t(C) #样本离差阵

# T^2样本统计量
T <- n*(n-1)*t(m_y) %*% solve(A) %*% m_y
T
F <- T/5*2
p <- 2
pf(F,p,n-p,lower.tail = FALSE)

m_b <- colMeans(boy)   #X的均值
m_g <- colMeans(girl)   #Y的均值
A_1 <- (n-1)*cov(boy)  #X的样本离差阵
A_2 <- (m-1)*cov(girl) #Y的样本离差阵
T2 <- (n+m-2)*n*m/(n+m)*t(m_b-m_g) %*% solve(A_1+A_2) %*% (m_b-m_g)
T2
F2 <- (n+m-p-1)/((n+m-2)*p)*T2
F2
pf(F2,p,n+m-p-1,lower.tail = FALSE)
