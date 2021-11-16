h_cluster <- function(data, p=2, type="s", stand= TRUE){
  # input:
  # data: a data.frame; row-sample, col-variable
  # p: decide the distance(default = 2, namely the Euclidean distance) 
  # type: "s" = single linkage(default); "a" = average linkage; 
  #       "c"= complete linkage
  # stand: default=T, standardized 
  
  # output:
  # out[[1]]: the cluster outcome
  ## out[[1]][n-i+1]: i-cluster solutions
  # out[[2]]: dissimilarities matrix D in each step
  
  if(stand==TRUE){
    data <- scale(data, scale = apply(data, 2, meanabsdev))
    } #standardized
  n <- nrow(data) # sample size
  D_list <- list() # storage dissimilarities matrix D
  kind <- as.character(c(1:n))  # bottom-up, represnt the kind
  out_clust <- list(1:n) # storage outcome
  
  # D initialize
  D <- matrix(0,n,n)
  colnames(D)<-c(1:n)
  rownames(D)<-c(1:n)
  for (i in 1:(n-1)) {
    for (j in (i+1):n) {
      D[i,j] <- (sum((abs(data[i,]-data[j,]))^p))^(1/p)
      D[j,i] <- D[i,j]
    }
  }
  if(type=="a"){D_ini <- D}
  D_list <- c(D_list,list(D))
  
  for (i in 1:(n-2)) {
    ind <- which(D == min(D[upper.tri(D)]), arr.ind = TRUE)
    new_cluster <-c(rownames(D)[ind[1]],colnames(D)[ind[2]])
    temp_clus <- toString(new_cluster)
    kind <- c(temp_clus,kind[-which(kind %in% new_cluster)])
    temp <- list()
    for (q in 1:length(kind)) {
      vec <- sort(as.numeric(unlist(strsplit(kind[q],split = ",")))) 
      # convert to number
      temp <- c(temp,list(vec))
    }
    out_clust <- c(out_clust,list(temp))
    
    D_temp  <- D
    D <- D[-which(rownames(D) %in% new_cluster), 
           -which(colnames(D) %in% new_cluster)]
    
    if(length(D)==1){
      out_clust <- c(out_clust,list(toString(kind)))
      D_list <- c(D_list,list(D))
      out <- list(out_clust,D_list)
      return(out)
      break} # end condition
    
    D <- rbind(0,D)
    D <- cbind(0,D)
    rownames(D)[1] <- temp_clus
    colnames(D)[1] <- temp_clus # new row and col both situated in first 
    
    if(type=="s"){
        for (k in kind[-which(kind %in% temp_clus)]) {
        D[1,k] <- min(D_temp[k, new_cluster])
        D[k,1] <- D[1,k]
      } 
    }# single linkage
    else if(type=="c"){
      for (k in kind[-which(kind %in% temp_clus)]) {
        D[1,k] <- max(D_temp[k, new_cluster])
        D[k,1] <- D[1,k]
      } 
    }# complete linkage
    else if(type=="a"){
      for (k in kind[-which(kind %in% temp_clus)]) {
        kk <- as.numeric(unlist(strsplit(k,split=",")))
        ij <- as.numeric(unlist(strsplit(new_cluster,split = ",")))
        D[1,k] <- sum(D_ini[ij,kk])/(length(ij)*length(kk))
        D[k,1] <- D[1,k]
      } 
    }# average linkage
    else {
      print("wrong") 
      break}
    D_list <- c(D_list,list(D))
  }
}
