
library(psych)
library(MASS)
library(psychometric)
library(psych)
op <- par(no.readonly = TRUE)







set.seed(1234)


par(mfrow=c(4, 2))

doPlot <- function(r){
  Sigma <- matrix(c(1, r, r, 1), 2, 2)
  plot(mvrnorm(n = 100, m = c(0, 0), Sigma=Sigma, empirical=TRUE), 
       main=paste0("r = ", r), xlab="", ylab="", cex.axis=1, cex.main=1,
       yaxt="n", xaxt="n")
  }

sapply(c(0, 0.1, 0.3, 0.5, -0.6, -0.8, 0.9, 0.95), doPlot)

#Massive outlier.
r <- 0
Sigma <- matrix(c(1, r, r, 1), 2, 2)
d <- mvrnorm(n = 100, m = c(0, 0), Sigma=Sigma, empirical=TRUE)
d <- rbind(d, c(100,100))
plot(d)

#Nonlinear relationship
x <- rnorm(100)
y <- x^2
plot(x, y)

#One value
x <- rnorm(100)
y <- c(rep(0, 99), 0.1)
plot(x, y)

#



#Correlation plots




rm(list=ls())



z0 <- fisherz(0)
zneg5 <- fisherz(-0.5)
zpos2 <- fisherz(0.2)
zpos9 <- fisherz(0.9)

n <- 30



sd <- 1/(sqrt(n - 3))

x <- seq(-1000,1000,length=100000)

hx0 <- dnorm(z0,x,sd)
xr <- fisherz2r(x)

hx0 <- ifelse(abs(hx0) < 0.0001, NA, hx0  )
plot( xr, hx0, type="l", xlim=c(-1, 1), xlab="r", ylab="Density",
      yaxt="n", lwd=2)

hxneg5 <- dnorm(zneg5,x,sd)
hxneg5 <- ifelse(abs(hxneg5) < 0.0001, NA, hxneg5  )
lines(  xr, hxneg5, type="l", lwd=2, lty=2)



hxpos9 <- dnorm(zpos9,x,sd)
hxpos9 <- ifelse(abs(hxpos9) < 0.0001, NA, hxpos9  )
lines(  xr, hxpos9, type="l", lty=3, lwd=2)



hp <- read.csv("https://dl.dropboxusercontent.com/u/24381951/arcdata/housePrices.csv")

names(hp)


