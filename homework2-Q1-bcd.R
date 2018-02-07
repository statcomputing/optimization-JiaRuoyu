#(b)----------------------------------------------------------------
q1=c(1.77,-0.23,2.76,3.80,3.47,56.75,-1.34,4.24,-2.44,3.29,3.71,-2.40,4.53,-0.07,-1.05,-13.87,-2.53,-1.75)

n = length(q1)

l <- function(a) {
  return(n*log(pi) + sum(log(1+(a-q1)^2))) #-log
}

l.grid <- function(a) {
  return(2*sum((a-q1)/(1+(a-q1)^2))) #-l.grid
}

l.hess <- function(a) {
  return(matrix(2*sum((1-(a-q1)^2)/(1+(a-q1)^2)^2),nrow=1)) #-l.hess
}

a.est <- function(starting) {
  ## MLE  
  theta <- nlminb(start=starting,l,l.grid,l.hess)$par
  return(theta)
}

# plot
a.value = seq(-2, 4, by=0.05)
log.like = c()
for (i in 1:length(a.value)){
  log.like[i] = -l(a.value[i])
}
plot(a.value, log.like, xlab="parameter",ylab="log-likelihood", type="l")

a.est(-11)
a.est(-1)
a.est(0)
a.est(1.5)
a.est(4)
a.est(4.7)
a.est(7)
a.est(8)
a.est(38)

#(c)------------------------------------------------------------------
a.est.fix <- function(starting) {
  alpha <- c(1,0.64,0.25)
  ## MLE
  a <- c()
  for (j in 1:3){
    l.hess.fix <- function(a) {
      return(matrix(1/alpha[j],nrow=1))
    }
    a[j] <- nlminb(start=starting,l,l.grid,l.hess.fix)$par
  }
  return(a)
}
a.est.fix(-11)
a.est.fix(-1)
a.est.fix(0)
a.est.fix(1.5)
a.est.fix(4)
a.est.fix(4.7)
a.est.fix(7)
a.est.fix(8)
a.est.fix(38)

#(d)---------------------------------------------------------------
a.est.fisher <- function(starting){
  ## MLE  
  theta <- starting
  delta <- 1
  while(abs(delta) >= 0.00001){
    theta1 <- theta 
    dl <- l.grid(theta)
    theta <- theta - dl/(n/2)
    delta <- theta - theta1
  }
  return(theta)
}

a.est.fisher(-11)
a.est.fisher(-1)
a.est.fisher(0)
a.est.fisher(1.5)
a.est.fisher(4)
a.est.fisher(4.7)
a.est.fisher(7)
a.est.fisher(8)
a.est.fisher(38)

