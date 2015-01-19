
#Draw normal distribution


x <- seq(-4, 4, length=1000)
hx <- dnorm(x)


colors <- c("red", "blue", "darkgreen", "gold", "black")
labels <- c("df=1", "df=3", "df=8", "df=30", "normal")

plot(x, hx, type="l", lty=1, yaxt="no", ylab="Frequency", xlab="Value of X",
       cex=3 , main=NULL, frame=F, axes=F)
axis(side=1, labels=FALSE)
axis(side=2, labels=FALSE)

hxw <- hx * 1000
fx <- unlist(apply(cbind(x, hxw), 1,  function(x) rep(x[1], round(x[2] ))))
hist(fx, breaks = 20 , main=NULL, xaxt="no", yaxt="no",ylab="Density", xlab="Value of X" )
axis(side=1, labels=FALSE)
axis(side=2, labels=FALSE)


par(mfrow=c(1, 2))

x <- seq(0, 8, length=1000)
hx <- df(x, df1=3, df=2144)
plot(x, hx, , type="l", lty=1, yaxt="no", ylab="Frequency", xlab="Value of X",
     cex=3 , main=NULL, frame=F, axes=F)
axis(side=1, labels=FALSE)
axis(side=2, labels=FALSE)
title("Positive skewed distribution")

x <- seq(0, 1, length=1000)
hx <- dbeta(x, 5, 2)
plot(x, hx, , type="l", lty=1, yaxt="no", ylab="Frequency", xlab="Value of X",
     cex=3 , main=NULL, frame=F, axes=F)
axis(side=1, labels=FALSE)
axis(side=2, labels=FALSE)
title("Negative skewed distribution")
