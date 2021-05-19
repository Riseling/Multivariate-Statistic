dt <- read.table("Ex3.12_data.txt")
A <- dt[1:5,]
B <- dt[6:9,]
C <- dt[10:13,]
n1 <- nrow(A)
n2 <- nrow(B)
n3 <- nrow(C)
n <- n1+n2+n3
k <- 3
p <- 3 
array_n <-c(n1,n2,n3) 

A1 <- (n1-1)*cov(A)
A2 <- (n2-1)*cov(B)
A3 <- (n3-1)*cov(C)
SA <- A1 +A2 + A3

M <- (n-k)*log(det(SA/(n-k))) - 
  (n1-1)*log(det(A1/(n1-1)))-
  (n2-1)*log(det(A2/(n2-1)))-
  (n3-1)*log(det(A3/(n3-1)))

M

d <- (2*p^2+3*p-1)/(6*(p+1)*(k-1))*
  (1/(n1-1)+1/(n2-1)+1/(n3-1)-1/(n-k))
d


f <- 1/2*p*(p+1)*(k-1)
f                       
(1-d)*M

pchisq((1-d)*M,f,lower.tail = FALSE)

n <- 5
m <- 4
p <- 3
m_b <- colMeans(A)   #X的均值
m_g <- colMeans(B)   #Y的均值
A_1 <- (n-1)*cov(A)  #X的样本离差阵
A_2 <- (m-1)*cov(B) #Y的样本离差阵
T2 <- (n+m-2)*n*m/(n+m)*t(m_b-m_g) %*% solve(A_1+A_2) %*% (m_b-m_g)
T2
F2 <- (n+m-p-1)/((n+m-2)*p)*T2
F2
pf(F2,p,n+m-p-1,lower.tail = FALSE)

#组间离差阵计算
total_mean <- colMeans(as.matrix(dt))
A1_mean <- colMeans(A)
A2_mean <- colMeans(B)
A3_mean <- colMeans(C)
SB <- n1*(A1_mean-total_mean) %*% t(A1_mean-total_mean) +
  n2*(A2_mean-total_mean) %*% t(A2_mean-total_mean) +
  n3*(A3_mean-total_mean) %*% t(A3_mean-total_mean)

delta <- det(SA)/(det(SA+SB))
delta

F3 <- (n-k-p+1)/p*(1-delta^(1/2))/delta^(1/2)
F3
pf(F3,2*p,2*(n-p-k+1),lower.tail = FALSE)


det(SA)
SA[1,1]*SA[2,2]*SA[3,3]
V <- det(SA)/(SA[1,1]*SA[2,2]*SA[3,3])
b <- 61/6
(-b)*log(V)
pchisq((-b)*log(V),3,lower.tail = FALSE)
