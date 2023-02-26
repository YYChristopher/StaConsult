m <- 20000##样本链长度
xt <- numeric(m)
a <- 1
b <- 1##建议分布Beta(a,b)的参数
p_vec <- seq(from=0.01,to=0.99,by=0.01)
p_pre <- rep(0,length(p_vec))
p_mse <- rep(0,length(p_vec))
n <- 300##样本量
mu <- c(0,5)
sigma <- c(1,1)##正态分布参数，用来形成混合分布
##生成观察值
for(j in 1:length(p_vec)){
  i <- sample(1:2,size = n,replace = TRUE,prob = c(p_vec[j],1-p_vec[j]))
  x <- rnorm(n,mu[i],sigma[i])
  ##生成独立样本链
  u <- runif(m)
  y <- rbeta(m,a,b)##形成建议分布的抽样
  xt[1] <- 0.5
  for(i in 2:m){
    fy <- y[i]*dnorm(x,mu[1],sigma[1])+(1-y[i])*dnorm(x,mu[2],sigma[2])
    fx <- xt[i-1]*dnorm(x,mu[1],sigma[1])+(1-xt[i-1])*dnorm(x,mu[2],sigma[2])
    r <- prod(fy/fx)*(xt[i-1]^(a-1)*(1-xt[i-1])^(b-1))/(y[i]^(a-1)*(1-y[i])^(b-1))
    if(u[i]<=r)
      xt[i] <- y[i]
    else
      xt[i] <- xt[i-1]
  }
  p_pre[j] <- mean(xt[1001:m])
  p_mse[j] <- (p_pre[j]-p_vec[j])^2
}
plot(p_vec,p_mse,col=2,main="不同p值估计mse结果",type="l")


