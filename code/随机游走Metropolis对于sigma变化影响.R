path="C:\\Users\\user\\Documents\\统计实践与高维\\统计咨询与实践\\大报告\\code"
setwd(path)
library(tseries)
source('随机游走Metropolis.R')
source('批均值法计算Monte Carlo误差.R')
sigma_max <- 100##设定建议分布sigma值范围为1-100
sigma_trans <- seq(from=0.05,to=sigma_max,by=0.05)##考虑建议分布sigma值的影响
rwreject <- rep(0,length(sigma_trans))
mcerrorlist <- rep(0,length(sigma_trans))
adflist <- rep(0,length(sigma_trans))
n <- 4
N <- 2000#抽样次数为2000次
x0 <- 25#设定数据链的初值
index <- 1:2000
for (i in 1:length(sigma_trans)) {
  rw <- rw.Metropolis(n,sigma_trans[i],x0,N)
  rwreject[i] <- rw$k
  y <- rw$x[index]
  mcerrorlist[i] <- mcerror.batch(y,batches = 50)
  adflist[i] <- adf.test(y)$p.value
}
plot(sigma_trans,rwreject,type="l",xlab = "模拟的建议分布sigma值",ylab = "抽样拒绝次数")
plot(sigma_trans,mcerrorlist,type="l",xlab = "模拟的建议分布sigma值",ylab = "Monto Carlo误差")
plot(sigma_trans,adflist,type="l",xlab = "模拟的建议分布sigma值",ylab = "adf检验p值")