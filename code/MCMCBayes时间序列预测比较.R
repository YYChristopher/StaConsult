library(bsts)
path="C:\\Users\\user\\Documents\\统计实践与高维\\统计咨询与实践\\大报告\\code"
setwd(path)
source("Makeplots函数.R")
#数据导入
data<-read.csv("D:/Rsoftware/时间序列与金融统计/非线性模型作业/data/S&P.csv")
d<-ts(data,frequency = 53,start = c(2016))
#数据处理
#figure1:导入收盘价数据
plot(d[,5])
x <- d[1:225,5]
ss2 <- AddSemilocalLinearTrend(list(), x)
model2 <- bsts(x, state.specification = ss2, niter = 500)
pred2 <- predict(model2, horizon = 12, burn = 100)
plot(model2)
plot(pred2)
plot(model2, "state",main = "state")
plot(model2, "components",main = "components")
plot(model2, "residuals",main = "residuals")
plot(model2, "prediction.errors",main = "prediction errors")
plot(model2, "forecast.distribution",main = "forecast distribution")
#MakePlots(model2)
pred2$mean
d[226:238,5]