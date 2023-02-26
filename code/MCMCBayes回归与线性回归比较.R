library(MCMCpack)
library(datasets)
data("swiss")
swiss.posterior1 <- MCMCregress(Fertility~Agriculture+Examination+Education+Catholic+Infant.Mortality,data=swiss)
summary(swiss.posterior1)#MCMC的Bayes线性回归
plot(swiss.posterior1[,2])

##最小二乘的普通线性回归：
swiss.lm <- lm(Fertility~Agriculture+Examination+Education+Catholic+Infant.Mortality,data=swiss)
summary(swiss.lm)