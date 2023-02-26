path="C:\\Users\\user\\Documents\\统计实践与高维\\统计咨询与实践\\大报告\\code"
setwd(path)
library(tseries)
source('随机游走Metropolis.R')
source('批均值法计算Monte Carlo误差.R')
n_max <- 200##设定t分布的自由度范围为1-200
n_trans <- seq(from=1,to=n_max,by=1)##考虑t分布自由度的影响
rwreject <- rep(0,length(n_trans))
mcerrorlist <- rep(0,length(n_trans))
acflist <- rep(0,length(n_trans))
adflist <- rep(0,length(n_trans))
N <- 2000#抽样次数为2000次
sigma <- 5
x0 <- 25#设定数据链的初值
index <- 1:2000
for (i in 1:length(n_trans)) {
  rw <- rw.Metropolis(n_trans[i],sigma,x0,N)
  rwreject[i] <- rw$k
  y <- rw$x[index]
  mcerrorlist[i] <- mcerror.batch(y,batches = 50)
  acflist[i] <- acf(y)[20]
  adflist[i] <- adf.test(y)$p.value
}
plot(n_trans,rwreject,type="l",xlab = "模拟的t分布自由度",ylab = "抽样拒绝次数")
plot(n_trans,mcerrorlist,type="l",xlab = "模拟的t分布自由度",ylab = "Monto Carlo误差")
plot(n_trans,acflist,type="l",xlab = "模拟的t分布自由度",ylab = "acf检验值")
plot(n_trans,adflist,type="l",xlab = "模拟的t分布自由度",ylab = "adf检验p值")