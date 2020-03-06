#################################################
#B_01b.concepts in R
# CMEE MSc
# 24 Feb 2020 Mon 
# Author: YUAN ZHANG 
#################################################

#prior
mu <- 2
tau <- 0.5

x <- seq(-4,10,0.01)
plot(x=x, dnorm(x=x, mean=mu, sd=tau), ylim=c(0,2.0),
     type="l", lty=1, ylab="Density", xlab=expression(theta), main="")
legend(x="topleft", legend=c(expression(pi(theta)),
                             expression(f(y~"|"~theta)), expression(p(theta~"|"~y))), lty=1:3)


plot(x=x, dnorm(x=x, mean=mu, sd=tau), ylim=c(0,2.0),
     type="l", lty=1, ylab="Density", xlab=expression(theta), main="")
legend(x="topleft", legend=c(expression(pi(theta)),
                             expression(f(y~"|"~theta)), expression(p(theta~"|"~y))), lty=1:3) # prior

# likelihood
y <- 6
sigma <- 1
points(x=x, y=dnorm(x=y, mean=x, sd=sigma), type="l", lty=2)

# prior
plot(x=x, dnorm(x=x, mean=mu, sd=tau), ylim=c(0,2.0),
     type="l", lty=1, ylab="Density", xlab=expression(theta), main="")
legend(x="topleft", legend=c(expression(pi(theta)),
                             expression(f(y~"|"~theta)), expression(p(theta~"|"~y))), lty=1:3) # prior

# likelihood
points(x=x, y=dnorm(x=y, mean=x, sd=sigma), type="l", lty=2) # likelihood

# posterior
B <- sigma^2/(sigma^2+tau^2)
postMean <- B*mu + (1-B)*y
postVar <- B*tau^2
points(x=x, y=dnorm(x=x, mean=postMean, sd=sqrt(postVar)), type="l", lty=3)