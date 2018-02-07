#(a)----------------------------------------------------------------
q2 = c(3.91,4.85,2.28,4.06,3.70,4.04,5.46,3.53,2.28,1.96,2.53,3.88,2.22,3.47,4.82,2.46,2.99,2.54,0.52)
a.value = seq(-pi, pi, by=0.05)
n = length(q2)
log.like = c()
for (i in 1:length(a.value)){
  log.like[i] = -n*log(2*pi)+sum(log(1-cos(q2-a.value[i])))
}
plot(a.value, log.like, xlab="parameter",
     ylab="log-likelihood", type="l")

#(b)---------------------------------------------------------------
a.est <- function(sample,starting) {
  n = length(sample)
  
  l <- function(a) {
    return(-n*log(2*pi) + sum(log(1-cos(sample-a)))) #log
  }
  
  l.grid <- function(a) {
    return(sum(sin(a-sample)/(1-cos(a-sample)))) #l.grid
  }
  
  l.hess <- function(a) {
    return(sum(1/(cos(a-sample)-1))) #l.hess
  }
  
  ## MLE  
  theta <- starting
  delta <- 1
  while(abs(delta) >= 1e-10){
    theta1 <- theta 
    dl <- l.grid(theta)
    ddl <- l.hess(theta)
    if (ddl==0){
      return(NULL)
    }
    theta <- theta - dl/ddl
    delta <- theta - theta1
  }
  
  return(c(theta,l(theta)))
}

q2 = c(3.91,4.85,2.28,4.06,3.70,4.04,5.46,3.53,2.28,1.96,2.53,3.88,2.22,3.47,4.82,2.46,2.99,2.54,0.52)
m = mean(q2)
theta0 = asin(m-pi)
theta0

#(c)--------------------------------------------------------------
a.est1 = a.est(q2,theta0)
a.est1[1]

#(d)--------------------------------------------------------------
a.est2 = a.est(q2,-2.7)
a.est3 = a.est(q2,2.7)
a.est2[1]
a.est3[1]

#(e)--------------------------------------------------------------
a.value = seq(-pi,pi,by = 2*pi/199); # set 200 equally spaced starting point
a.par = c()
a.mle = c()
for (i in 1:length(a.value)){
  a.par[i] = a.est(q2,a.value[i])[1]
  a.mle[i] = a.est(q2,a.value[i])[2]
}

ascend = order(a.mle)
a.mle.ascend = a.mle[ascend]
a.par.ascend = a.par[ascend]

# find the unique outcome
j = 1
p = 2
i = 1
e = c()
e[1] = a.mle.ascend[1]
while(j<=200){
  if(a.mle.ascend[i]!=a.mle.ascend[j]){
    e[p] = a.mle.ascend[j]
    p = p+1
    i = j
    j = j+1
  }else{j = j+1}
}

# classify the starting point according to the unique outcome
group = matrix(nrow = length(e),ncol = 46)
for (y in 1:length(e)){
  t = 1
  for (i in 1:200){
    if(a.mle.ascend[i]== e[y]){
      group[y,t] = round(a.value[ascend[i]],5)
      t = t+1
    }
  }
}
group[is.na(group)]=""
write.csv(group,'group.csv')