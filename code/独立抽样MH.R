m <- 20000##样本链长度
xt <- numeric(m)
a <- 1
b <- 1##建议分布Beta(a,b)的参数
p <- 0.2##混合权重
n <- 300##样本量
mu <- c(0,5)
sigma <- c(1,1)##正态分布参数，用来形成混合分布
##生成观察值
i <- sample(1:2,size = n,replace = TRUE,prob = c(p,1-p))
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
plot(xt,type = "l",ylab = "p")
hist(xt[1001:m],main = "",xlab = "p",prob = TRUE)
print(mean(xt[1001:m]))