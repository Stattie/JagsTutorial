library(runjags)
library(coda)

# Data
data_tbbmc <- read.table("data/osteop.txt",header=TRUE)
bmi <- data_tbbmc$bmi
age <- data_tbbmc$age
tbbmc <- data_tbbmc$tbbmc

n.obs <- length(tbbmc)


## Parameters to monitor 
parameters <- c("b1","b2","sig2")

summary(lm(data = data.frame(tbbmc,bmi,age),formula = tbbmc~.))

inits.list <- list(list(b0 = 1, b1 = 0, b2 = 0, tau = 1),
                   list(b0 = 1.1, b1 = 0.1, b2 = 0.1, tau = 1.5))

data.list <- list(y=tbbmc, x1 = bmi, x2 = age,n.obs=n.obs)

post.runjags <- run.jags(model = "MultLinearRegression", data = data.list, inits = inits.list,
                         n.chains = 2,burnin=500,
                         thin = 1, sample=2000,
                         monitor = parameters)

summary(post.runjags,confidence = c(0.95))
plot(post.runjags,plot.type = c("trace"))
plot(post.runjags,plot.type = c("hist"))
plot(post.runjags,plot.type = c("autocorr"))

