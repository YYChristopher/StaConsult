##
library(tseries)
path="C:\\Users\\user\\Documents\\统计实践与高维\\统计咨询与实践\\大报告\\code"
setwd(path)
source('随机游走Metropolis.R')
source('批均值法计算Monte Carlo误差.R')
n <- 5#设定t分布自由度
N <- 10000#抽样次数为2000次
sigma <- c(0.05,0.5,4,20)
x0 <- 25#设定数据链的初值
rw1 <- rw.Metropolis(n,sigma[1],x0,N)
rw2 <- rw.Metropolis(n,sigma[2],x0,N)
rw3 <- rw.Metropolis(n,sigma[3],x0,N)
rw4 <- rw.Metropolis(n,sigma[4],x0,N)
print(c(rw1$k,rw2$k,rw3$k,rw4$k))
index <- 1:10000
y1 <- rw1$x[index]
y2 <- rw2$x[index]
y3 <- rw3$x[index]
y4 <- rw4$x[index]
plot(index,y1,type="l",xlab = bquote(sigma*"=0.05"),main = "",ylab = "x")
plot(index,y2,type="l",xlab = bquote(sigma*"=0.5"),main = "",ylab = "x")
plot(index,y3,type="l",xlab = bquote(sigma*"=4"),main = "",ylab = "x")
plot(index,y4,type="l",xlab = bquote(sigma*"=20"),main = "",ylab = "x")
mcerror.batch(y1,batches=50)
mcerror.batch(y2,batches=50)
mcerror.batch(y3,batches=50)
mcerror.batch(y4,batches=50)
acf(y1)
acf(y2)
acf(y3)
acf(y4)
##发现sigma过小，图形为随机游走，数据链10000次迭代不收敛
##但是sigma大了，虽然收敛，但拒绝次数过多