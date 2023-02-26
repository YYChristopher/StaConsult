y <- rnorm(100,mean = 100,sd=0.1)
bary <- mean(y)
iterations <- 50000
mu_0 <- 0
s_0 <- 100
a_0 <- 0.001
b_0 <- 0.001
theta <- matrix(nrow = iterations,ncol = 2)
cur.mu <- 0
cur.tau <- 4
cur.s <- sqrt(1/cur.tau)
for(t in 1:iterations){
  w <- s_0^2/(cur.s^2/n+s_0^2)
  m <- w*bary+(1-w)*mu_0
  s <- sqrt(w/n)*cur.s
  cur.mu <- rnorm(1,m,s)
  a <- a_0+0.5*n
  b <- b_0+0.5*sum((y-cur.mu)^2)
  cur.tau <- rgamma(1,a,b)
  cur.s <- sqrt(1/cur.tau)
  theta[t,] <- c(cur.mu,cur.s)
}
plot(c(1:iterations),theta[,1],xlab = "迭代次数",ylab = "均值迭代结果",type="l")
plot(c(1:iterations),theta[,2],xlab = "迭代次数",ylab = "标准差迭代结果",type="l")
mean(theta[,1])
mean(theta[,2])