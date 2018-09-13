library(runjags)
library(coda)

#Example 4.2 Lesaffre & Lawson (2012)

ALP <- read.table("data/ALP.txt",header=TRUE)
head(ALP)
str(ALP)
summary(ALP)

#Transform data
ALP$alkfos_tr <- 100/sqrt(ALP$alkfos)
hist(ALP$alkfos_tr)

# Historical Data
hist_alp <- ALP[ALP$artikel==1,"alkfos_tr"]
ybar <- mean(hist_alp)
sig2 <- var(hist_alp)
ybar; sig2

# Current Data
alp <- ALP[ALP$artikel==0,"alkfos_tr"]

file.show("NormalDistribution.txt")


#Data list
n.obs <- length(alp)
data.list <- list(y=alp,n.obs=n.obs)

# Parameters to monitor 
parameters <- c("mu","sig2")

#Initial values
inits.list <- list(list(mu = 5.5, tau = 2.5),
                   list(mu = 5.2, tau = 2.2))

inits.list <- function()(list(mu=rnorm(1,5.5,1),
                         tau=runif(1,2,3)))

post.sim <- run.jags(model = "NormalDistribution", data = data.list, inits = inits.list,
                         n.chains = 2, adapt = 0,burnin = 500,
                         thin = 1,sample = 5000,
                         monitor = parameters)

summary(post.sim ,confidence = c(0.95)) #default is 95%
print(post.sim)


plot(post.sim ,plot.type = c("trace"),vars = "mu")
plot(post.sim ,plot.type = c("hist"))
plot(post.sim ,plot.type = c("autocorr"))


samples <- as.mcmc(post.sim)
#Coda plots
par(mfrow=c(2,2)) # plot figures in 2x2 format if function allows
traceplot(samples) # trace plots
cumuplot(samples,ask=FALSE) # running mean plots
acfplot(samples) # autocorrelation function plot
autocorr(samples) # autocorrelation values
crosscorr.plot(samples) # cross-correlation output
densplot(samples) # density plots of the marginal posteriors
effectiveSize(samples) # effective size
HPDinterval(samples) # HPD intervals of all parameters



