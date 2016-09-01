

hp <- read.csv("http://raw.github.com/jeremymiles/arc/master/housePrices.csv")


x <- hp[["list.price"]]
mean(x)
sd(x)

library(stats4)
LL <- function(mu, sigma) {
  
  
   R = dnorm(x, mu, sigma)
  -sum(log(R))
}

 mle(LL, start = list(mu = 100, sigma=100))

 x <- 0
 mu <- 2
 sigma <- 1
 dnorm(2, 0, 1)
 # ML chart
 x <- seq(-5, 5, 0.1)
 plot(x, dnorm(x, 0, 1), type = "l", ylab = "Density", xlab = "x")
 # Draw line at density for 1
 lines(c(-5, 0),
       c(dnorm(0, 0, 1) , dnorm(0, 0, 1)),
       lty = 2)
 lines(c(0, 0),
       c(dnorm(0, 0, 1), 0 ),
       lty = 2)
  
 lines(c(-5, 1), c(dnorm(1, 0, 1) , dnorm(1, 0, 1)), lty = 3)  
 lines(c(1, 1),
       c(dnorm(1, 0, 1), 0 ),
       lty = 3)
 
 
 lines(c(-5, 2), c(dnorm(2, 0, 1) , dnorm(2, 0, 1)), lty = 4)  
 lines(c(2, 2),
       c(dnorm(2, 0, 1), 0 ),
       lty = 4)
 
text( x=-3, y=0.38, paste0("Density at x = 0: ", round(dnorm(0, 0, 1), 3)))
text( x=-3, y=0.22, paste0("Density at x = 1:\n ", round(dnorm(1, 0, 1), 3)))
text( x=-3.5, y=0.08,  paste0("Density at x = 2: \n", round(dnorm(2, 0, 1), 3)))



x <- hp[["list.price"]]
mean(x)
sd(x)
(x <- head(hp$list.price, 10))
dnorm(x, mean = mean(x), sd = sd(x))
log(dnorm(x, mean = mean(x), sd = sd(x)))


summary(glm(factor(hp$parking.type) ~ 1, family="binomial"))


## Simple example
prod(print(dnorm(1:3, 1, 1)))
prod(print(dnorm(1:3, 1.5, 1)))
prod(print(dnorm(1:3, 2.00, 1)))


sd(1:4)


dnorm(2, 2, 2)




x <- seq(0, 10, 0.01)
plot(x, -sin(x)*x, type="l", yaxt="n", xaxt="n", xlab="Maximum Likelihood Estimate", ylab="Negative Log Likelihood")
arrows(x0=2, y1=-sin(2)*2, y0=3)
arrows(x0=8, y1=-sin(8)*8, y0=0)
text(x=2, y=3, pos=3,  labels="Gets stuck here")
text(x=8, y=0, pos=3,  labels="Should be here")