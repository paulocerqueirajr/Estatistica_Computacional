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

n <- 100
x <- rnorm(n, mean = 10, sd = 4)

U <- function(theta, dados){
  U_mu    <- (1/theta[2])*(sum(dados)-n*theta[1])
  U_sigma <- -(2*n*pi)/2 + (0.5*(theta[2]^(-2)))*(sum((dados-theta[1])^2))
  return(c(U_mu, U_sigma))
}

U(theta=c(10, 16), dados=x)


H <- function(theta, dados){
  m_2    <- -n/theta[2]
  sig_2  <- -(1/(theta[2]^3))*(sum((dados-theta[1])^2))
  mu_sig <- -(1/(theta[2]^2))*(sum((dados-theta[1])^2))
  sig_mu <- -(1/(theta[2]^2))*(sum((dados-theta[1])^2))

  return(matrix(c(m_2, mu_sig, sig_mu, sig_2), ncol = 2))
}

H(theta=c(10, 16), dados=x)










