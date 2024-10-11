# Código: Métodos de otimização
# Autor: Paulo Jr
# Data: 29/09/2024


#-- Iniciando --#

#- Funções importantes:


f <- function(theta){
  return((theta^2)-sin(theta))
}

h <- function(theta){
  return(2*theta-cos(theta))
}

hlinha <- function(theta){
  return(2+sin(theta))
}

#- Ilustrando as funções:


theta.val <- seq(0, pi/4, length.out=1000)
h.val     <- h(theta = theta.val)
hl.val     <- hlinha(theta = theta.val)
par(mfrow=c(1,2))
plot(x = theta.val, y=h.val, type="l")
plot(x = theta.val, y=hl.val, type="l")

#- Iniciando o MNR:

e <- 0.000001
theta.vet <- NULL
theta.vet[1] <- pi/8 # Chute inicial
difer <- 1
i <- 1

while(difer > e){
  theta.vet[i+1] <- theta.vet[i] - (h(theta.vet[i])/hlinha(theta.vet[i]))
  theta.vet[i] - solve(H(theta.vet[i])%*%U(theta.vet[i]))
  difer          <- abs(theta.vet[i+1]-theta.vet[i])
  i              <-i+1
  cat("Iter:", i, ", valor de theta:", theta.vet[i], "\n")
}


#- Usando a função f:


theta.val <- seq(0, pi/4, length.out=1000)
f.val     <- f(theta = theta.val)
par(mfrow=c(1,1))
plot(x = theta.val, y=f.val, type="l")


#- Otimizando:


optimize(f=f, interval = c(0, pi/4), 
         maximum = FALSE, tol = 0.0001)


## Método NR (caso multidimensional)

# gerando os dados:

set.seed(1234567890)

n <- 1000
x <- rnorm(n, mean = 10, sd = 4)

## Vetor escore:

U <- function(theta, dados){
  
  U_mu    <- (sum(dados-theta[1])/theta[2])
  U_sigma <-  (-0.5*length(dados)/theta[2]) + sum((dados-theta[1])^2)/(2*(theta[2]^2))
  
  return(c(U_mu, U_sigma))
  
}

U(theta=c(10, 16), dados=x)

## Matriz Hessiana:

H <- function(theta, dados){
  
  m_2    <- -length(dados)/theta[2]
  sig_2  <- (0.5*length(dados)/(theta[2]^2)) - sum((dados-theta[1])^2)/(theta[2]^3)
  mu_sig <- -(1/(theta[2]^2))*sum(dados-theta[1])
  sig_mu <- -(1/(theta[2]^2))*sum(dados-theta[1])
  
  return(matrix(c(m_2, mu_sig, sig_mu, sig_2), nrow=2, ncol = 2))
}

H(theta=c(10, 16), dados=x)

## Iniciando NR

theta0 <- c(5, 5) # Chute inicial
dif <- 1 #Diferença
erro <- 10^(-6) # Tolerancia
i <- 1 # contador

while( dif>erro ){
  
  H0 <- H(theta = theta0, dados = x)
  U0 <- U(theta = theta0, dados = x) 
  prodHU <- solve(H0, U0)
  theta1 <- theta0 - prodHU
  dif <- max(abs(theta1-theta0))
  
  theta0 <- theta1
  i<- i+1
  cat("Iter:", i, "est:", theta1, "\n")
  #if(i==10)break
}

## Estimativa:

cat("mu:", theta1[1],"-", "sigma2:", theta1[2])



## Modelo Weibull:

set.seed(1234567890)

n <- 1000
x <- rweibull(n, shape = 2, scale = 1)

## Vetor escore:

U <- function(theta, dados){
  
  U_a   <- (length(dados)/theta[1]) + sum(log(dados)) - 
    length(dados)*log(theta[2]) - (theta[2]^theta[1])*sum((dados^theta[1])*log(dados/theta[2]))
  U_sigma <- (-length(dados)*theta[1]/theta[2]) + (theta[1]/(theta[2]^(theta[1]+1)))*sum(dados^theta[1]) 
  
  return(c(U_a, U_sigma))
  
}

U(theta=c(2, 1), dados=x)

## Matriz Hessiana:

H <- function(theta, dados){
  
  a_2   <- (-length(dados)/(theta[1]^2)) - (theta[2]^theta[1])*sum((dados^theta[1])*(log(dados/theta[2])^2))
  sig_2 <- (length(dados)*theta[1])/(theta[2]^2) - ((theta[1]+1)*theta[1]/(theta[2]^(theta[1]+2)))*sum(dados^theta[1])
  s1    <- sum((theta[1]*dados^(theta[1]))/(theta[2]^(theta[1]+1))*log(dados/theta[2]))
  s2    <- (1/theta[2])*sum((dados/theta[2])^theta[1])
  a_sig <- -length(dados)/theta[2] + s1 +s2 
  sig_a <- -length(dados)/theta[2] + s1 +s2 
  
  return(matrix(c(a_2, a_sig, sig_a, sig_2), nrow=2, ncol = 2))
}

H(theta=c(2, 1), dados=x)



## Iniciando NR

theta0 <- c(3, 2) # Chute inicial
dif <- 1 #Diferença
erro <- 10^(-6) # Tolerancia
i <- 1 # contador

while( dif>erro ){
  
  H0 <- H(theta = theta0, dados = x)
  U0 <- U(theta = theta0, dados = x) 
  prodHU <- solve(H0, U0)
  theta1 <- theta0 - prodHU
  dif <- max(abs(theta1-theta0))
  
  theta0 <- theta1
  i<- i+1
  cat("Iter:", i, "est:", theta1, "\n")
  #if(i==10)break
}

## Estimativa:

cat("a:", theta1[1],"-", "sigma:", theta1[2])








