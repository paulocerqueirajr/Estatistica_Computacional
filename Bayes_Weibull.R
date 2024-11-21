require(runjags)

set.seed(432104)

n <- 1000
x <- rweibull(n, shape = 1,  scale=1)

## Usando o modelo já implemetado:

model.Weibull <-"
  
  # verossimilhança:
  model {
    for (i in 1:n){
      x[i] ~ dweib(v, lambda)
    }
  # dist. a priori:
  
  v ~ dgamma(0.001, 0.001)
  lambda ~ dgamma(0.001, 0.001)
}
"

model.Weibull.imp <-"
  model {
  # Verossimilhança:
    for (i in 1:n){
      loglik[i]  <- log(v)+log(lambda)+ (v-1)*log(x[i])-lambda*pow(x[i],v)
      dummy[i] ~ dpois( -loglik[i] + const )
    }
  #Prioris:
  v ~ dgamma(0.001, 0.001)
  lambda ~ dgamma(0.001, 0.001)
}
"

#dados <- list(x=x, n=n)
dados <- list(x=x, n=n, const=10000, dummy=rep(0, n))

inits.gen <- list(v=3, lambda=2)  

param<- c("v", "lambda") 

runjags.options(method = "rjags") ## sets it back to run everything on just one core

### Run JAGS
### --------------------

jagsfit <- run.jags(model = model.Weibull.imp,
                    monitor = param,
                    data = dados,
                    adapt = 1, n.chains = 2, thin = 2,
                    burnin = 100, sample = 1000)

plot(jagsfit)
jagsfit$summaries
