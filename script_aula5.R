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
x <- rnorm(n, mean = 10, sd = 2)

U <- function(theta, dados){
  #theta[2] <- theta[2]^2
  U_mu    <- (1/theta[2])*(sum(dados-theta[1]))
  U_sigma <-  (-0.5*(1/theta[2])) + (0.5*(1/(theta[2]^2)))*(sum((dados-theta[1])^2))
  return(c(U_mu, U_sigma))
}

#U(theta=c(10, 16), dados=x)


H <- function(theta, dados){
  #theta[2] <- theta[2]^2
  m_2    <- -length(dados)/theta[2]
  sig_2  <- (0.5*(1/(theta[2]^2)))-(1/(theta[2]^3))*(sum((dados-theta[1])^2))
  mu_sig <- -(1/(theta[2]^2))*sum(dados-theta[1])
  sig_mu <- -(1/(theta[2]^2))*sum(dados-theta[1])
  
  return(matrix(c(m_2, mu_sig, sig_mu, sig_2), ncol = 2))
}

#H(theta=c(10, 16), dados=x)


theta0 <- as.matrix(c(9, 10))
dif <- c(1, 1)
erro <- 10^(-6)
i <- 1
while( sum( dif>erro )>0 ){
  
  H.inv0 <- solve(H(theta = theta0, dados = x))
  U0  <- U(theta = theta0, dados = x) 
  theta1 <- theta0 - (H.inv0%*%U0)
  dif <- abs(theta1-theta0)
  
  theta0 <- theta1
  i<- i+1
  cat("Iter:", i, "est:", theta1, "\n")
  #if(i==10)break
}








