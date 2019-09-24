library(runjags)
library(coda)

# Data
N <- 1000
beta0 <- 1  # intercept
beta1 <- 1  # slope
x <- rnorm(n=N)  # standard Normal predictor
mu <- beta0*1 + beta1*x  # linear predictor function
lambda <- exp(mu)  # CEF
y <- rpois(n=N, lambda=lambda)  # Poisson DV
dat <- data.frame(x,y)  

fit <- summary(glm(y ~ x, family = poisson(link = "log"), data = dat))
fit

## Parameters to monitor 
parameters <- c("b")

inits.list <- list(list(b = c(1,1)),
                   list(b = c(1,1)))

data.list <- list(y = dat$y, x = dat$x, n.obs=N,
                  mu.beta=rep(0,2),  
                  tau.beta=diag(.0001,2))

post.runjags <- run.jags(model = "RCode/PoissonRegression", data = data.list, inits = inits.list,
                         n.chains = 2,burnin=500,
                         thin = 15, sample=1000,
                         monitor = parameters, modules = "glm")

summary(post.runjags)
plot(post.runjags,plot.type = c("trace"))
plot(post.runjags,plot.type = c("hist"))
plot(post.runjags,plot.type = c("autocorr"))

