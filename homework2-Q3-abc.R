#(a)----------------------------------------------------------------
t <- c(0,8,28,41,63,69,97,117,135,154)
x <- c(2,47,192,256,768,896,1120,896,1184,1024)
f <- expression(2*K/(2+(K-2)*exp(-r*t)))

df <- function(K,r,t){
  dfk <- D(f,"K")
  dfr <- D(f,"r")
  K <- K 
  r <- r 
  t <- t
  a <- eval(dfk)
  b <- eval(dfr)
  c <- array(c(a,b),c(1,2))
  return(c)
}

Df <- function(K,r){
  a <- K
  b <- r
  m <- df(a,b,t[1])
  for(i in 2:10){
    c <- df(a,b,t[i])
    m <- rbind(m,c)
  }
  return(m)
}

Z <- function(K,r){
  a <- c()
  for(i in 1:10){
    a[i] <- x[i] - 2*K/(2+(K-2)*exp(-r*t[i]))
  }
  m <- array(a,c(10,1))
  return(m)
}

theta <- matrix(c(1200,0.2),nrow=2)
delta <- matrix(c(1,1),nrow=2)

while(crossprod(delta,delta)>=0.001){
  theta1 <- theta 
  a <- Df(theta[1,1],theta[2,1])
  z <- Z(theta[1,1],theta[2,1])
  theta <- theta + solve(t(a)%*%a)%*%t(a)%*%z
  delta <- theta - theta1
}

a.est <- theta
print(a.est)

#(b)---------------------------------------------------------------
f <- function(K,r){
  return(sum((x-2*K/(2+(K-2)*exp(-r*t)))^2))
}

z <- matrix(0,100,100,byrow=T)
for (i in 1:100){
  for (j in 1:100){
    K <- 600 + 8*j
    r <- 0 + 0.01*i
    z[j,i] <- f(K,r)
  }
}
contour(z)

#(c)--------------------------------------------------------------
l <- expression(log(1/(sqrt(2*pi)*sigema))-
                  (log((2*2+2*(K-2)*exp(-r*0))/(2*K)))^2/(2*sigema^2)+
                  log(1/(sqrt(2*pi)*sigema))-
                  (log((2*47+47*(K-2)*exp(-r*8))/(2*K)))^2/(2*sigema^2)+
                  log(1/(sqrt(2*pi)*sigema))-
                  (log((2*192+192*(K-2)*exp(-r*28))/(2*K)))^2/(2*sigema^2)+
                  log(1/(sqrt(2*pi)*sigema))-
                  (log((2*256+256*(K-2)*exp(-r*41))/(2*K)))^2/(2*sigema^2)+
                  log(1/(sqrt(2*pi)*sigema))-
                  (log((2*768+768*(K-2)*exp(-r*63))/(2*K)))^2/(2*sigema^2)+
                  log(1/(sqrt(2*pi)*sigema))-
                  (log((2*896+896*(K-2)*exp(-r*69))/(2*K)))^2/(2*sigema^2)+
                  log(1/(sqrt(2*pi)*sigema))-
                  (log((2*1120+1120*(K-2)*exp(-r*97))/(2*K)))^2/(2*sigema^2)+
                  log(1/(sqrt(2*pi)*sigema))-
                  (log((2*896+896*(K-2)*exp(-r*117))/(2*K)))^2/(2*sigema^2)+
                  log(1/(sqrt(2*pi)*sigema))-
                  (log((2*1185+1184*(K-2)*exp(-r*135))/(2*K)))^2/(2*sigema^2)+
                  log(1/(sqrt(2*pi)*sigema))-
                  (log((2*1024+1024*(K-2)*exp(-r*154))/(2*K)))^2/(2*sigema^2))

dl <- function(beita){
  dlk <- D(l,"K")
  dlr <- D(l,"r")
  dlsigema <- D(l,"sigema")
  K <- beita[1] 
  r <- beita[2] 
  sigema <- beita[3]
  a <- eval(dlk)
  b <- eval(dlr)
  c <- eval(dlsigema)
  return(c(a,b,c))
}

ddl <- function(beita){
  dlkk <- D(D(l,"K"),"K")
  dlkr <- D(D(l,"K"),"r")
  dlksigema <- D(D(l,"K"),"sigema")
  dlrr <- D(D(l,"r"),"r")
  dlrsigema <- D(D(l,"r"),"sigema")
  dlsigema2 <- D(D(l,"sigema"),"sigema")
  K <- beita[1] 
  r <- beita[2] 
  sigema <- beita[3]
  a <- c(eval(dlkk),eval(dlkr),eval(dlksigema),eval(dlkr),eval(dlrr),
         eval(dlrsigema),eval(dlksigema),eval(dlrsigema),eval(dlsigema2))
  m <- matrix(a,byrow=TRUE,nrow=3)
  return(m)
}

a <- matrix(c(1200,0.2,0.5),nrow=3)
delta <- matrix(c(1,1,1),nrow=3)
while(crossprod(delta,delta)>=0.001){
  b <- a
  c <- matrix(dl(a),nrow=3)
  d <- solve(ddl(a))
  a <- a - d%*%c
  delta <- a - b
}
a
solve(-ddl(a))