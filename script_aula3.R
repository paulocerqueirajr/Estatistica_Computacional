

# Metodo da transformação inversa:
# Caso discreto:

## Fixando a semente:

set.seed(123456789)

# Exemplo 1:

x <- c(1,2,3,4)
p <- c(0.2, 0.15, 0.25, 0.4)
n <- 1000
x.gen <- rep(NA, n)

for(i in 1:n){
  u <- runif(1)
  if(u<=p[1]){
    x.gen[i] <- 1
  }
  if(  p[1] <= u && (u <= p[1]+p[2])){
    x.gen[i] <- 2
  }
  if(  p[1]+p[2] <= u && (u <= p[1]+p[2]+p[3])){
    x.gen[i] <- 3
  }
  if( p[1]+p[2]+p[3]<=u){
    x.gen[i] <- 4
  }
}

x.gen
table(x.gen)
par(mfrow=c(1,1))
barplot(prop.table(table(x.gen)))

# Exemplo 2:

n <- 10
u <- runif(1000,0,1)
amostra <- floor(n*u) + 1
table(amostra)
prop.table(table(amostra))

## Exemplo 3:

n <- 5000
p <- c(0.11,0.12,0.09,0.08,0.12,
       0.10,0.09,0.09,0.10,0.10)
amostra<- NULL
k <- 0
rej <- 0
while(k <= n-1){
  u.1 <- runif(1,0,1)
  x <- floor(10*u.1) + 1
  u.2 <- runif(1,0,1)
  pass1 <- (10*p[x])/1.2
  if (u.2 <= pass1){ 
    ((k <- k + 1) & (amostra[k] <- x))
  }else{
    rej <- rej+1
  }
}
length(amostra)

tabela<- table(amostra)
tabela
prop.table(tabela)






