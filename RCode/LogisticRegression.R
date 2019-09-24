library(runjags)
library(coda)

# Data
stm <- read.table("RCode/data/stm.txt",header=TRUE)
y <- stm$ce
x <- cbind(rep(1,length(y)),stm$gender,stm$xcen)

n.obs <- length(y)

## Parameters to monitor 
parameters <- c("b")

summary(glm(y ~ X2 + X3, family = binomial, data = data.frame(y,x)) )

inits.list <- list(list(b = c(0,0,0)),
                   list(b = c(0,0,0)))

data.list <- list(y=y, x = x, n.obs=n.obs, p = ncol(x))

post.runjags <- run.jags(model = "RCode/LogisticRegression", data = data.list, inits = inits.list,
                         n.chains = 2,burnin=500,
                         thin = 1, sample=2000,
                         monitor = parameters,modules = "glm")

summary(post.runjags,confidence = c(0.95))
plot(post.runjags,plot.type = c("trace"))
plot(post.runjags,plot.type = c("hist"))
plot(post.runjags,plot.type = c("autocorr"))

