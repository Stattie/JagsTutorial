library(runjags)
library(coda)

# Data
data_tbbmc <- read.table("data/osteop.txt",header=TRUE)
bmi <- data_tbbmc$bmi
tbbmc <- data_tbbmc$tbbmc

n.obs <- length(tbbmc)

## Parameters to monitor 
parameters <- c("b1","sig2")

inits.list <- list(list(b0 = 0.5, b1 = 0.1, tau = 10,
                        .RNG.name = "base::Mersenne-Twister",
                        .RNG.seed = 1234),
                   list(b0 = 0.8, b1 = 0.2, tau = 5,
                        .RNG.name = "base::Mersenne-Twister",
                        .RNG.seed = 1235))

data.list <- list(y=tbbmc,x=bmi,n.obs=n.obs)

post.runjags <- run.jags(model = "LinearRegression", data = data.list, inits = inits.list,
                         n.chains = 2, adapt=0,burnin=500,
                         thin = 1, sample=2000, method = "parallel",
                         monitor = parameters)

summary(post.runjags,confidence = c(0.95))
plot(post.runjags,plot.type = c("trace"))
plot(post.runjags,plot.type = c("hist"))
plot(post.runjags,plot.type = c("autocorr"))

#Coda plots
par(mfrow=c(2,2)) # plot figures in 2x2 format if function allows
traceplot(as.mcmc(post.runjags)) # trace plots
cumuplot(as.mcmc(post.runjags),ask=FALSE) # running mean plots
acfplot(as.mcmc(post.runjags)) # autocorrelation function plot
autocorr(as.mcmc(post.runjags)) # autocorrelation values
crosscorr.plot(as.mcmc(post.runjags)) # cross-correlation output
densplot(as.mcmc(post.runjags)) # density plots of the marginal posteriors
effectiveSize(as.mcmc(post.runjags)) # effective size
HPDinterval(as.mcmc(post.runjags)) # HPD intervals of all parameters
