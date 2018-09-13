library(runjags)
library(coda)
library(MASS)
library(pscl)

# Data
N <- 1000
beta0 <- 1
beta1 <- 1
x <- rnorm(n=N)
mu <- beta0*1 + beta1*x
lambda <- exp(mu)
r <- 2
y <- rnegbin(n=N, mu=lambda, theta=r)
dat <- data.frame(x,y)

fit <- summary(glm.nb(y~x,data=dat))
fit

## Parameters to monitor 
parameters <- c("b","r")

inits.list <- list(list(b = c(1,1)),
                   list(b = c(1,1)))

data.list <- list(y = dat$y, x = dat$x, n.obs=N)

post.runjags <- run.jags(model = "NegBinRegression", data = data.list, inits = inits.list,
                         n.chains = 2,burnin=500,
                         thin = 1, sample=1000,
                         monitor = parameters,modules = "glm")

summary(post.runjags)
plot(post.runjags,plot.type = c("trace"))
plot(post.runjags,plot.type = c("hist"))
plot(post.runjags,plot.type = c("autocorr"))

