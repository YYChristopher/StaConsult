y <- 600
N <- 1000
iterations <- 1500
mu.theta <- 0
s.theta <- 100
prop.mu <- log(y/(N-y))
mle.var <- 1/y+1/(N-y)
w <- 1/(1+mle.var/s.theta^2)
prop.s <- sqrt(mle.var*w)
theta <- numeric(iterations)
acc.prob <- 0
current.theta <- 0
for(t in 1:iterations){
  prop.theta <- rnorm(1,prop.mu,prop.s)
  loga <- ((prop.theta*y-N*log(1+exp(prop.theta)))
           -(current.theta*y-N*log(1+exp(current.theta)))
           +dnorm(prop.theta,mu.theta,s.theta,log = TRUE)
           -dnorm(current.theta,mu.theta,s.theta,log = TRUE)
           +dnorm(current.theta,prop.mu,prop.s,log = TRUE)
           -dnorm(prop.theta,prop.mu,prop.s,log = TRUE))
  u <- runif(1)
  u <- log(u)
  if(u<loga){
    current.theta <- prop.theta
    acc.prob <- acc.prob+1
  }
  theta[t] <- current.theta
}
p <- exp(theta)/(1+exp(theta))
plot(theta,type = "l",main = "trace plot(theta)",xlab = "iterations")
plot(p,type = "l",main = "trace plot(pi)",xlab = "iterations",ylab = "pi")
plot(density(theta),main = "",xlab = "theta",ylab = "Posterior Density")
plot(density(p),main="",xlab="pi",ylab="Posterior Density")
summary(theta)
summary(p)
sd(theta)
sd(p)
quantile(theta,c(0.025,0.975))
quantile(p,c(0.025,0.975))