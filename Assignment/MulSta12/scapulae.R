library(cluster)
data <- read.table("primate.scapulae.txt",header = T)
primate <- data[,2:8]
# gamma不使用 genus代表属 使用2~7个变量

# single-linkage
out_s <- agnes(primate,stand = TRUE, method = "s")
plot(out_s,which=2)
rect.hclust(out_s,5)
clu_s <- cutree(out_s,5) 
er_s <- sum(abs(sort(table(data$classdigit))-sort(table(clu_s))))/2 # 误判数
er_s/nrow(primate) # 误判率

# average-linkage
out_a <- agnes(primate, stand = TRUE,method = "average")
plot(out_a,which=2)
rect.hclust(out_a,5)
clu_a <- cutree(out_a,5)
er_a <- sum(abs(sort(table(data$classdigit))-sort(table(clu_a))))/2 # 误判数
er_a/nrow(primate) # 误判率


# complete linkage
out_c <- agnes(primate, stand = TRUE,method = "complete")
plot(out_c,which=2)
rect.hclust(out_c,5)
clu_c <- cutree(out_c,5)
er_c <- sum(abs(sort(table(data$classdigit))-sort(table(clu_c))))/2 # 误判数
er_c/nrow(primate) # 误判率

out_mine <- h_cluster(primate,p=2,type="c")
out_mine[[1]][101]

cout <- list()
for (i in 1:5) {
  cout <- c(cout,list(which(clu_c==i)))
}
cout
