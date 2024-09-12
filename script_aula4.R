## Aula 3:

## Integração numérica:

## Método do trapézio
f <- function(x){
  x*sin(x)
}

trapezio <- function(integrando, a, b, ...){
  Int <- ((integrando(a, ...) + integrando(b, ...))/2)*(b-a)
  return(Int)
}

trapezio(integrando = f, a=0, b=pi/4)
pracma::trapzfun(f=f, a = 0, b = pi/4, maxit = 0)




## Método da quadratura Gauss-Hermite

library(statmod)

gauss.hermite <- function(integrando, n.pontos, ...){
  pontos <- gauss.quad(n.pontos, kind="hermite")
  integral <- sum(pontos$weights*integrando(pontos$nodes,...)/exp(-pontos$nodes^2))
  return(integral)
}

gauss.hermite(f, n.pontos = 50)



