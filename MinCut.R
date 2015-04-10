library("igraph")

f.getAD <- function(my.graph) {
  my.graph.adjMat <- get.adjacency(my.graph)
  A <- as.matrix(my.graph.adjMat)
  
  my.graph.degree <- degree(my.graph)
  len <- length(my.graph.degree)
  D <- matrix(0,nrow=len, ncol=len)
  for (i in 1:len) {
    D[i,i] = my.graph.degree[i]
  }
  return(list(A,D))
}

set.seed(1234)

#ring <- graph.ring(n=5)
g <- erdos.renyi.game(10, 1/3)

listAD <- f.getAD(g)
A <- listAD[[1]]
D <- listAD[[2]]
L <- D - A

e <- eigen(L)

#v1 <- e$vector[,]
#v1 <- as.matrix(v1)
#t(v1) %*% L %*% v1





