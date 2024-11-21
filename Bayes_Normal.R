require(runjags)

set.seed(432104)

n <- 1000
x <- rnorm(n, 0, 5)



## Usando o modelo já implemetado:

model.Gauss <-"
  model {
    for (i in 1:n){
      x[i] ~ dnorm(mu, phi)
    }
  mu ~ dnorm(0,.0001)
  phi <- pow(sigma, -2)
  sigma ~ dunif(0,100)
}
"

dados <- list(x=x, n=n)

inits.gen <- list(mu=1, sigma=100)  

param<- c("mu", "sigma", "phi") 

runjags.options(method = "rjags") ## sets it back to run everything on just one core

### Run JAGS
### --------------------

jagsfit <- run.jags(model = model.Gauss,
                    monitor = param,
                    data = dados, inits = inits.gen,
                    adapt = 1, n.chains = 1, thin = 1,
                    burnin = 1000, sample = 10000)

plot(jagsfit)


require(runjags)

set.seed(432104)

n <- 1000
x <- rnorm(n, 0, 5)


# Implementando a função de verossimilhança:


model.Gauss.imp <-"
  model {
    for (i in 1:n){
      loglik[i]  <- -0.5*log(2*pi)-0.5*log(pow(sigma,2))-0.5*pow(sigma,-2)*pow(x[i]-mu,2)
      dummy[i] ~ dpois( -loglik[i] + const )
    }
  mu ~ dnorm(0,.0001)
  phi <- pow(sigma, -2)
  sigma ~ dunif(0,100)
}
"

dados <- list(x=x, n=n, pi=pi, const=10000, dummy=rep(0, n))

inits.gen <- list(mu=1, sigma=100)  

param<- c("mu", "sigma", "phi") 

runjags.options(method = "runjags") ## sets it back to run everything on just one core

### Run JAGS
### --------------------

jagsfit <- run.jags(model = model.Gauss.imp,
                    monitor = param,
                    data = dados, inits = inits.gen,
                    adapt = 1, n.chains = 1, thin = 1,
                    burnin = 1, sample = 1000)

plot(jagsfit)

jagsfit$summary


par(mfrow=c(3,1))
plot(1:1000, unlist(jagsfit$mcmc[,"mu"]), type="l", ylab="mu")
plot(1:1000, unlist(jagsfit$mcmc[,"sigma"]), type="l", ylab="sigma")
plot(1:1000, unlist(jagsfit$mcmc[,"tau"]), type="l", ylab="phi")



