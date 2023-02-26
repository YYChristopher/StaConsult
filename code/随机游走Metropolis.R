rw.Metropolis <- function(n,sigma,x0,N){
  x <- numeric(N)
  x[1] <- x0
  u <- runif(N)
  k <- 0
  for(i in 2:N){
    y <- rnorm(1,x[i-1],sigma)
    if(u[i]<=(dt(y,n)/dt(x[i-1],n)))
      x[i] <- y
    else{
      x[i] <- x[i-1]
      k <- k+1
    }
  }
  return(list(x=x,k=k))
}