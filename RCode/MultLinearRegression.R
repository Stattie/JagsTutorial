library(runjags)
library(coda)

# Data
data_tbbmc <- read.table("RCode/data/osteop.txt",header=TRUE)
bmi <- data_tbbmc$bmi
age <- data_tbbmc$age
tbbmc <- data_tbbmc$tbbmc

n.obs <- length(tbbmc)

x <- cbind(rep(1,n.obs),bmi,age)
  
## Parameters to monitor 
parameters <- c("b", "sig2")

summary(lm(data = data.frame(tbbmc,bmi,age),formula = tbbmc ~ bmi+age))

inits.list <- list(list(b = c(1,0,0), tau = 1),
                   list(b = c(1.1,0.1,0.1), tau = 1.5))

data.list <- list(y=tbbmc, x = x, n.obs=n.obs,
                  p = ncol(x))

post.runjags <- run.jags(model = "RCode/MultLinearRegression_vectorized", data = data.list, inits = inits.list,
                         n.chains = 2,burnin=500,
                         thin = 1, sample=2000,
                         monitor = parameters,modules = "glm")

summary(post.runjags,confidence = c(0.95))
plot(post.runjags,plot.type = c("trace"),vars = c("b[2]"))
plot(post.runjags,plot.type = c("hist"))
plot(post.runjags,plot.type = c("autocorr"))

