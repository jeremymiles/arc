
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

hp <- read.csv("http://raw.github.com/jeremymiles/arc/master/housePrices.csv")


raters <- read.csv("http://raw.github.com/jeremymiles/arc/master/houseRaters.csv")




z0 <- fisherz(0)
zneg5 <- fisherz(-0.5)
zpos2 <- fisherz(0.2)zpos9 <- fisherz(0.9)

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




names(hp)


library(psych)
alpha(d[1:6])
alpha(d[1:12])

library(CTT)
mean(sapply(1:1000, function(x) {
    x <- sample(1:12, 6)
  r <- spearman.brown( cor(apply(d[x], 1, sum), apply(d[-x], 1, sum)), input=2)$r.new
  }
) )

library(QuantPsyc)
install.packages("psychometric") 
library(psychometric) 
hp <- read.csv("http://raw.github.com/jeremymiles/arc/master/housePrices.csv")

r <- cor(hp$list.price, hp$beds)
n <- nrow(hp)
CIr(r=r, n=n)

r <- 
n <- 
CIr(r=cor(hp$list.price, hp$beds), n=nrow(hp))

cor(hp$list.price, hp$sqft, method="kendall")
cor.test(hp$list.price, hp$sqft, method="kendall")
cor(hp[1:6, ]$list.price, hp[1:6, ]$sqft, method="kendall")


cor(hp$list.price, hp$sqft, method="spearman")
cor(rank(hp$list.price), rank(hp$sqft))
cor(hp$list.price, hp$sqft)
cor.test(hp$list.price, hp$sqft, method="spearman")
cor.test(hp$list.price, hp$sqft, method="spearman", exact=FALSE)

cor(hp$list.price, hp$sqft, method="kendall")
cor.test(hp$list.price, hp$sqft, method="kendall")



shortHP <- hp[ 1:10, ]

cor.test(shortHP$beds, shortHP$list.price )

CIr(0.7608877, n=10)



#load sem for polychorics
install.packages("polycor")
library(polycor)
polychor(hp$beds, hp$parking.spots)
cor(hp$beds, hp$parking.spots)



ghq <- read.csv("http://raw.github.com/jeremymiles/arc/master/ghq.csv")
