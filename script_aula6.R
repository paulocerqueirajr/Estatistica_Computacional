## Simulação MC

#--- Modelo normal:

# Definindo os Parâmetros Verdadeiros
set.seed(123456789)  # Para replicabilidade
true_mu <- 5       # Média verdadeira
true_sigma <- 2    # Desvio padrão verdadeiro
n <- 500            # Tamanho da amostra
n_simulations <- 1000  # Número de simulações

# Inicializando vetores para armazenar as estimativas de mu e sigma^2
mu_estimates <- numeric(n_simulations)
sigma2_estimates <- numeric(n_simulations)
pc_mu <- numeric(n_simulations)
pc_sigma2 <- numeric(n_simulations)


# Simulação de Monte Carlo
for (i in 1:n_simulations) {
  # Gerando uma amostra da distribuição normal
  sample <- rnorm(n, mean = true_mu, sd = true_sigma)
  
  # Calculando estimativas para a média e variância amostral
  mu_estimates[i] <- mean(sample)
  sigma2_estimates[i] <- var(sample)  # var() em R calcula a variância amostral (divisão por n-1)
  
}


# Análise dos Resultados

mean_mu_estimate <- mean(mu_estimates)
mean_sigma2_estimate <- mean(sigma2_estimates)

sd_mu_estimate <- sd(mu_estimates)
sd_sigma2_estimate <- sd(sigma2_estimates)

bias_mu_estimate <- mean(mu_estimates-true_mu)
bias_sigma2_estimate <- mean(sigma2_estimates-true_sigma)

eqm_mu_estimate <- sd(mu_estimates) + (mean_mu_estimate-true_mu)^2
eqm_sigma2_estimate <- sd(sigma2_estimates) + (mean_sigma2_estimate-(true_sigma^2))^2

media <- c(mean_mu_estimate, mean_sigma2_estimate)
desvio <- c(sd_mu_estimate, sd_sigma2_estimate)
vicio <- c(bias_mu_estimate, bias_sigma2_estimate)
eqm <- c(eqm_mu_estimate, eqm_sigma2_estimate)

tabela <- rbind(media, desvio, vicio, eqm)
colnames(tabela) <- c("mu", "sigma2")  
tabela

# Visualização das Distribuições das Estimativas
par(mfrow = c(1, 2))  # Dividir a área de plotagem em 2 gráficos
hist(mu_estimates, breaks = 30, main = "Distribuição das Estimativas de Mu",
     xlab = "Estimativas de Mu", col = "skyblue", border = "white")
hist(sigma2_estimates, breaks = 30, main = "Distribuição das Estimativas de Sigma^2",
     xlab = "Estimativas de Sigma^2", col = "lightgreen", border = "white")


# boxplot
par(mfrow = c(1, 2))  # Dividir a área de plotagem em 2 gráficos
boxplot(mu_estimates, main = "Distribuição das Estimativas de Mu",
     ylab = "Estimativas de Mu")
boxplot(sigma2_estimates, main = "Distribuição das Estimativas de Sigma^2",
     ylab = "Estimativas de Sigma^2")


## Modelo Weibull

# Função de log-verossimilhança

logWeibull <- function(theta, dados){
  a <- theta[1]
  s <- theta[2]
  n <- length(dados)
  x <- dados
  
  l <- n*log(a)+n*log(s)+(a-1)*sum(log(x))-s*sum((x)^a) 
  return(-l)
}

## Modelo Weibull:

set.seed(1234567890)
n   <- 1000 # tamanho da amostra
a.p <- 2  # forma
s.p <- 1  # escala
sim <- 100 # Simulações
n.par <- 2

# vetores de armazenamento:

par.vet    <- matrix(NA, ncol=n.par, nrow=sim) 
ep.par.vet <- matrix(NA, ncol=n.par, nrow=sim) 

# Iniciando o SMC:

for( i in 1:sim){
  
  x <- rweibull(n, shape = a.p, scale = 1/(s.p^a.p))  # dados gerados
  theta0 <- c(3, 2) # Chute inicial
  est <- optim(par = theta0, fn = logWeibull, gr =NULL , method ="BFGS" , 
               hessian = TRUE, dados=x)
  
  par.vet[i,] <- est$par
  ep.par.vet[i,] <- sqrt(diag(solve(est$hessian)))
  
}


# Análise dos Resultados

mean_par <- apply(par.vet, 2, mean)
ep_par   <- apply(ep.par.vet, 2, mean)
bias_par <- apply(par.vet-c(a.p, s.p), 2, mean)
eqm_par  <- apply(  ((ep.par.vet)^2) + (par.vet-c(a.p, s.p))^2, 2, mean)

tabela <- rbind(mean_par, ep_par, bias_par, eqm_par)
colnames(tabela) <- c("a", "s")  
tabela


# Visualização das Distribuições das Estimativas
par(mfrow = c(1, 2))  # Dividir a área de plotagem em 2 gráficos
hist(par.vet[,1], breaks = 30, main = "Distribuição das Estimativas de a",
     xlab = "Estimativas de a", col = "skyblue", border = "white")
hist(par.vet[,2], breaks = 30, main = "Distribuição das Estimativas de s",
     xlab = "Estimativas de s", col = "lightgreen", border = "white")






