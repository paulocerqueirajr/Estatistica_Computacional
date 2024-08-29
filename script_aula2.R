## Métodos específicos:

set.seed(123456789)

# Uniforme(a,b)
n <- 1000
x <- runif(n, 0,1)
a <- 10
b <- 20
y <- (b-a)*x+a
par(mfrow=c(2,1))
hist(x)
hist(y)


# gerar dist Exp(lambda)

n <- 100
u <- runif(n)
x <- -log(1-u)
lambda <- 10
y <- (1/lambda)*x
par(mfrow=c(2,1))
hist(x)
hist(y)

rexp_paulo<- function(n=n, lambda=lambda){
  u <- runif(n)
  x <- -log(1-u)
  y <- (1/lambda)*x
  return(y)  
}


## Gerar de uma ditribuição Gamma(n,lambda)

rgama_paulo<- function(n=n, forma=forma, lambda=lambda){
  #n <- 100
  #forma <- 4
  #lambda <- 2
  y <- vector(mode="numeric", length = n)
  for(i in 1:n){
    u <- runif(forma)
    x <- -log(1-u)
    y[i] <- sum((1/lambda)*x)  
  }
  return(y)  
}

## Gerando de uma dist. normal:
## Box-Muller

n<- 1000
u1 <- runif(n)
u2 <- runif(n)

x <- sqrt(-log(u1))*cos(2*pi*u2)
y <- sqrt(-log(u1))*sin(2*pi*u2)
par(mfrow=c(1,2))
hist(x)
hist(y)

mu <-10
sigma <- 2 

x <- (sigma*x)+mu
hist(x)

## Gerando de uma qui-quadrado

n  <- 100
gl <- 2
x  <- vector(mode = "numeric", length = n) 
for(i in 1:n){
  u <- runif(2)
  x[i] <- -log(prod(u))
}
hist(x)











