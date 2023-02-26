library(bsts)
path="C:\\Users\\user\\Documents\\统计实践与高维\\统计咨询与实践\\大报告\\code"
setwd(path)
source("Makeplots函数.R")
#数据导入
S <- read.table("D:/时间序列与金融统计/线性模型作业与报告/BP500.txt")
dataold <- c(S)
data <- c(S)
dataold <- as.data.frame(lapply(dataold,as.numeric))
data <- as.data.frame(lapply(data,as.numeric))
data[55,1] <- (data[54,1]+data[56,1])/2
data[212,1] <- (data[211,1]+data[213,1])/2
data[342,1] <- (data[341,1]+data[343,1])/2
data[464,1] <- (data[463,1]+data[465,1])/2
data[601,1] <- (data[600,1]+data[602,1])/2
d <- ts(dataold,frequency = 12,start=c(1970,3))
#数据处理
#figure1:导入收盘价数据
plot(d,col="red",xlab="时间",ylab = "对数收益率")
abline(h=0,col="blue")
#figure2:转对数收益率
logd<-diff(log(d[,5]))
plot(logd,col="red",xlab="时间",ylab="对数收益率")
abline(h=0,col="blue")
title("BP500对数收益率(未经异常值处理)随时间序列分布")
m <- ts(data,frequency = 12,start=c(1970,3))
plot(m,col="red",xlab="时间",ylab = "对数收益率")
abline(h=0,col="blue")
title("BP500对数收益率(经异常值处理)随时间序列分布")
y <- m
ss <- AddLocalLinearTrend(list(), y)
ss <- AddSeasonal(ss, y, nseasons = 12)
model <- bsts(y, state.specification = ss, niter = 500)
pred <- predict(model, horizon = 12, burn = 100)
plot(model)
plot(pred)
MakePlots(model)