

#Chapter 6

library(car)

#power

rm(list=ls())



regPower <- function(k, n, R2, alpha=0.05, covs=0){
  
  df1 <- k
  df2 <- n - k  - 1
  
  u <- df1
  v <- df2
  f2 <- R2 / (1-R2)
  F = f2 * df2/df1
  lambda <- f2 * (u + v + 1)
  cat("Lambda =", round(lambda,3), "\n")
  criticalF <- qf(p=alpha, df1=k, df2=n-k-covs-1, lower.tail=FALSE)
  cat("Critical F =", round(criticalF,3), "\n")
  power <- (pf(q=criticalF, df1=k, df2=n-k-covs-1, ncp=lambda, lower.tail=FALSE))
  cat("Power =", round(power,3), "\n")
  invisible(power)
}




regPower(k=2, n=20, R2=0.26)
             , alpha=0.05, covs=3)
regPower(2, 20, 0.5, alpha=0.05)




d <- data.frame(n=rep(10:1000, 3), R2 = c(rep(0.02, 991), rep(0.13, 991), rep(0.26, 991)))
d$preds1 <- 1
d$preds2 <- 2
d$preds3 <- 3
d$preds4 <- 4
d$preds5 <- 5
d$preds6 <- 6
d$preds10 <- 10
d$preds20 <- 20


d$power1 <- apply(d, 1,  function(x) regPower(n=x["n"], k=x["preds1"], R2 = x["R2"]))
d$power2 <- apply(d, 1,  function(x) regPower(n=x["n"], k=x["preds2"], R2 = x["R2"]))
d$power3 <- apply(d, 1,  function(x) regPower(n=x["n"], k=x["preds3"], R2 = x["R2"]))
d$power4 <- apply(d, 1,  function(x) regPower(n=x["n"], k=x["preds4"], R2 = x["R2"]))
d$power5 <- apply(d, 1,  function(x) regPower(n=x["n"], k=x["preds5"], R2 = x["R2"]))
d$power6 <- apply(d, 1,  function(x) regPower(n=x["n"], k=x["preds6"], R2 = x["R2"]))
d$power10 <- apply(d, 1,  function(x) regPower(n=x["n"], k=x["preds10"], R2 = x["R2"]))
d$power20 <- apply(d, 1,  function(x) regPower(n=x["n"], k=x["preds20"], R2 = x["R2"]))
  


#draw the power charts
par(mfrow=c(2, 2), mar=c(5, 4, 2, 2))
with(d[d$R2 == 0.02 & d$n < 601, ], plot(n, power1, type="l", xlab="", ylab="Power", lwd=1, main="One Predictor", ylim=c(0.2, 1)))
with(d[d$R2 == 0.13 & d$n < 601, ], lines(n, power1,   ylab="Power", lwd=2, lty=2))
with(d[d$R2 == 0.26 & d$n < 601, ], lines(n, power1,   lwd=4))
legend("bottomright", legend=c("Small effect", "Medium effect", "Large effect"), lwd=c(1, 2, 4), lty=c(1, 2, 1))

with(d[d$R2 == 0.02 &  d$n < 601,], plot(n, power2, type="l", xlab="", ylab="", lwd=1, 
                                         main="Two Predictors", ylim=c(0.2, 1)))
with(d[d$R2 == 0.13 &  d$n < 601,], lines(n, power2,  xlab="Sample size", ylab="Power", lwd=2, lty=2))
with(d[d$R2 == 0.26 &  d$n < 601,], lines(n, power2,  xlab="Sample size", ylab="Power", lwd=4))

with(d[d$R2 == 0.02 &  d$n < 601,], plot(n, power3, type="l", xlab="Sample size", ylab="Power", lwd=1, 
                                         main="Three Predictors", ylim=c(0.2, 1)))
with(d[d$R2 == 0.13 & d$n < 601,], lines(n, power3,  xlab="Sample size", ylab="Power", lwd=2, lty=2))
with(d[d$R2 == 0.26 &  d$n < 601,], lines(n, power3,  xlab="Sample size", ylab="Power", lwd=4))

with(d[d$R2 == 0.02 &  d$n < 601,], plot(n, power4, type="l", ylab="", xlab="Sample size", lwd=1, 
                                         main="Four Predictors", ylim=c(0.2, 1)))
with(d[d$R2 == 0.13 &  d$n < 601,], lines(n, power4,  xlab="Sample size", ylab="Power", lwd=2, lty=2))
with(d[d$R2 == 0.26 &  d$n < 601,], lines(n, power4,  xlab="Sample size", ylab="Power", lwd=4))



with(d[d$R2 == 0.02 &  d$n < 601,], plot(n, power5, type="l", ylab="", xlab="Sample size", lwd=1, 
                                         main="Five Predictors", ylim=c(0.2, 1)))
with(d[d$R2 == 0.13 &  d$n < 601,], lines(n, power5,  xlab="Sample size", ylab="Power", lwd=2, lty=2))
with(d[d$R2 == 0.26 &  d$n < 601,], lines(n, power5,  xlab="Sample size", ylab="Power", lwd=4))
legend("bottomright", legend=c("Small effect", "Medium effect", "Large effect"), lwd=c(1, 2, 4), lty=c(1, 2, 1))

with(d[d$R2 == 0.02 &  d$n < 601,], plot(n, power6, type="l", ylab="", xlab="Sample size", lwd=1, 
                                         main="Six Predictors", ylim=c(0.2, 1)))
with(d[d$R2 == 0.13 &  d$n < 601,], lines(n, power6,  xlab="Sample size", ylab="Power", lwd=2, lty=2))
with(d[d$R2 == 0.26 &  d$n < 601,], lines(n, power6,  xlab="Sample size", ylab="Power", lwd=4))


with(d[d$R2 == 0.02 &  d$n < 601,], plot(n, power10, type="l", ylab="", xlab="Sample size", lwd=1, 
                                         main="Ten Predictors", ylim=c(0.2, 1)))
with(d[d$R2 == 0.13 &  d$n < 601,], lines(n, power10,  xlab="Sample size", ylab="Power", lwd=2, lty=2))
with(d[d$R2 == 0.26 &  d$n < 601,], lines(n, power10,  xlab="Sample size", ylab="Power", lwd=4))


with(d[d$R2 == 0.02 &  d$n < 601,], plot(n, power20, type="l", ylab="", xlab="Sample size", lwd=1, 
                                         main="Twenty Predictors", ylim=c(0.2, 1)))
with(d[d$R2 == 0.13 &  d$n < 601,], lines(n, power20,  xlab="Sample size", ylab="Power", lwd=2, lty=2))
with(d[d$R2 == 0.26 &  d$n < 601,], lines(n, power20,  xlab="Sample size", ylab="Power", lwd=4))





### noncentrality power charts

par(mfrow=c(2, 1))
f <-seq(0.1,20,0.01)
df1 <- 1
df2 <- 18
R2 <- 0.26

N <- df1 + df2 + 1

fDensity <- sapply(f, function(x) df(x, df1=df1, df2=df2)) 
plot(f, fDensity, type="l", xlab="F", ylab="", ylim=c(0, 0.6), 
  main=paste0("(Central) F Distribution, with ", df1, ", ", df2,  " df"))

criticalF <-  qf(p=0.05, df1=df1, df2=df2, lower.tail=FALSE)

x <- c(criticalF, criticalF,  f[f>=criticalF], max(f), max(f))
y <- c(0, df(criticalF, df1=df1, df2=df2), fDensity[f>criticalF], 0, 0)

polygon(x=x,y=y, col="grey")
text(10, 0.08, "Shaded area is  5%")
arrows( 5, 0.30 , criticalF, df(criticalF, df1=df1, df2=df2) )
text(5, 0.35, paste0("Critical value for F is ", round(criticalF, 2)))


f2 <- R2/(1-R2)
lambda <- f2 * (N)
ncpFDensity <- sapply(f, function(x) df(x, df1=df1, df2=df2, ncp=lambda)) 

plot(f, ncpFDensity,  type="l", xlab="F", ylab="",  ylim=c(0, 0.6),
     main=paste0("Non-Central F Distribution, with ", df1, ", ",df2," df and lambda=", round(lambda, 2)))

x <- c(criticalF, criticalF,  f[f>=criticalF], max(f), max(f))
y <- c(0, df(criticalF, df1=df1, df2=df2, ncp=lambda), ncpFDensity[f>criticalF], 0, 0)

polygon(x=x,y=y, col="grey", fillOddEven=TRUE)
arrows( 5, 0.3 , criticalF, df(criticalF, df1=df1, df2=df2, ncp=lambda) )
text(5, 0.35, paste0("Critical value for F is ", round(criticalF, 2)))

resPower <- regPower(k = df1, n=N, R2=R2)

text(10, 0.2, paste0("Shaded area is ", round(resPower * 100), "%"))




regPower <- function(k, n, R2, alpha=0.05, covs=0){
  
  df1 <- k
  df2 <- n - k  - 1
  
  u <- df1
  v <- df2
  f2 <- R2 / (1-R2)
  F = f2 * df2/df1
  lambda <- f2 * (u + v + 1)
  cat("Lambda =", round(lambda,3), "\n")
  criticalF <- qf(p=alpha, df1=k, df2=n-k-covs-1, lower.tail=FALSE)
  cat("Critical F =", round(criticalF,3), "\n")
  power <- (pf(q=criticalF, df1=k, df2=n-k-covs-1, ncp=lambda, lower.tail=FALSE))
  cat("Power =", round(power,3), "\n")
  invisible(power)
}




regPower(k=1, n=20, R2=0.26)
regPower(k=1, n=20, R2=0.26, alpha=0.01)
regPower(k=1, n=20, R2=0.26, covs=5)

, alpha=0.05, covs=3)
regPower(2, 20, 0.5, alpha=0.05)

hp <- read.csv("http://raw.github.com/jeremymiles/arc/master/housePrices.csv")
hp <- hp[-51, ]



fit1 <- lm(log(last.sale.price) ~ log(original.list.price)  + beds + baths + sqft  + parking.spots +year.built , data=hp)
round(summary(fit1)$coefficients[, c(1, 2, 4)], 5)

round(cor(fit1$model[, 2:7]), 2)

round(cbind(as.data.frame(vif(fit1)), 1/vif(fit1)), 2)


round(summary(fit1)$coefficients, 3)
