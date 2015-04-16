
#Draw normal distribution
library(e1071) 
library(psych)

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




read.csv("kurtosis.csv")
kurtosis(rbeta(shape1=1, shape2=1, n=1000))
hist(rbeta(shape1=1, shape2=1, n=1000))
kurtosis(rt(df=2, n=10000))

kurt <- read.csv("http://raw.github.com/jeremymiles/arc/master/kurtosis.csv")

with(kurt, plot(x, Neg, colour="grey", type="l"))
with(kurt, lines(x, Pos, colour="black"))
with(kurt, lines(x, Normal, colour="black"))

par(mfrow=c(3, 1))
plot(dt(seq(-5, 5, 0.01), 0.8), type="l", xlab=" ",ylab=" ", yaxt="n", xaxt="n" , main="Positive Kurtosis" )
plot(dnorm(seq(-5, 5, 0.01)), type="l", xlab=" ",ylab=" ", yaxt="n", xaxt="n" , main="Normal")
plot(seq(250, 750, 1), dbeta(seq(0, 1, 0.002), shape1=2, shape2=2), type="l", xlim=c(0, 1000), xlab=" ",ylab=" ", yaxt="n", xaxt="n", main="Negative Kurtosis" )



kurtosis(rbeta(shape1=2, shape2=2, n=1000), type=2)
kurtosis(rt(df=0.8, n=10))
)

x1 <- rnorm(20)
x2 <- rchisq(1, n=20)
y <- rt(1, n=20)^2
fit <- lm(y ~ x1 + x2)
res <- resid(fit)
round(cor(cbind(x1, x2, y, res)), 3)

y <- c(1, rep(28, 19))
fit <- lm(y ~ x1 + x2)
res <- resid(fit)
round(cor(cbind(x1, x2, y, res)), 3)


set.seed(1234)
x1 <- rnorm(10) + 1
x2 <- rnorm(100) * 2 
t.test(x1, x2, var.equal = FALSE)
t.test(x1, x2, var.equal = TRUE)


reactionTimes <- read.csv("http://raw.github.com/jeremymiles/arc/master/reactionTimes.csv", header=FALSE )

hist(reactionTimes$V1, main="",  xlab="Reaction Time (Seconds)")

mean(reactionTimes$V1)
sd(reactionTimes$V1)
skewness(reactionTimes$V1)
kurtosis(reactionTimes$V1, type=2)  #Note type=2 to match SPSS

reactionTimes$logV1 <- log(reactionTimes$V1)
hist(reactionTimes$logV1,main="",  xlab="Log Reaction Time (Seconds)")

mean(log10(reactionTimes$V1))

mean(reactionTimes$logV1)
sd(reactionTimes$logV1)
skewness(reactionTimes$logV1)
kurtosis(reactionTimes$logV1, type=2)  #Note type=2 to match SPSS

exp(mean(reactionTimes$logV1))

#Positively skewed distribution
set.seed(1234)
x <- rchisq(10000, 1.5) * 10
par(mfrow=c(2, 2))
hist(x, breaks=25, yaxt="n", xlab="")
hist(log(x ), breaks=25, xlab="")
hist(log(x  + 1), breaks=25, xlab="")
hist(log(x  + 1000), breaks=25, xlab="")
par(mfrow(c=1, 1))


#Negatively skewed distribution
set.seed(4321)
x <- -( rf(10000, 8, 6)) * 5   + 30

par(mfrow=c(2, 1))
x <- ifelse(x < 0, NA, x)
hist(x, breaks=20, xlab="",  ylab="",yaxt="n")
hist((x)^2, breaks=20, xlab="", ylab="", yaxt="n")


par(mfrow=c(4, 1))
set.seed(56789)
y1 <- rnorm(500) + 5
y2 <- rnorm(500) + 9
y <- c(y1, y2)
hist(y, breaks=60, main = "All", xlim=c(2, 12), xlab="")
hist(y1, main="X = 1", breaks=30, xlim=c(2, 12), xlab="")
#abline(v=mean(y1), lwd=4)  I decided i didn't like this lines
hist(y2, main="X = 2", breaks=30, xlim=c(2, 12), xlab="")
#abline(v=mean(y2), lwd=4)
hist( c(y1 - mean(y1), y2-mean(y2)), main="Residuals", xlab="", xlim=c(-6, 6), breaks=30 )



#
library(QuantPsyc)
hp <- read.csv("http://raw.github.com/jeremymiles/arc/master/housePrices.csv")

par(mfrow=c(2, 1))
hist(hp$list.price, breaks=30, main="Histogram of listing price ($000)", xlab="", xlim=c(0, 2000))
hist(log(hp$list.price), breaks=30, main="Histogram of log transformed listing price ($000)", xlab="")
fit1 <- lm(list.price~sqft + year.built + last.sale.price, data=hp)


par(mfrow=c(2, 2))
summary(fit1)
hist(resid(fit1), breaks=60, main="Histogram of residuals (model 1)", xlab="", cex=0.8, cex.lab=1.25)

fit2 <- lm(list.price~sqft + year.built, data=hp)
summary(fit2)
fit2.resids <- resid(fit2)
hist(fit2.resids)
hist(resid(fit2), breaks=60, main="Histogram of residuals (model 2) - \n last sale price removed", xlab="", cex=0.8, cex.lab=1.25)


fit3 <- lm(log(hp$list.price) ~ sqft + year.built, data=hp)
summary(fit3)
hist(resid(fit3), breaks=60, main="Histogram of residuals (model 3) - \n log transformed list price", xlab="", cex=0.8, cex.lab=1.25)


fit4 <- lm(log(hp$list.price + 1000) ~ sqft + year.built, data=hp)
summary(fit4)
hist(resid(fit4), breaks=60, main="Histogram of residuals (model 4) - \n log transformed list price + 1000", xlab="", cex=0.8, cex.lab=1.25)

#Find the house with the huge residual
which(  max(resid(fit2)) == resid(fit2))

hp.nooutlier <- hp[-51, ]

fit5 <- lm(log(list.price + 1000) ~ sqft + year.built, data=hp.nooutlier)
summary(fit5)
round(coef(fit5), 5)

temp <- data.frame(sqft=1000, year.built=2000)
exp(predict(fit5, temp))-1000


par(mfrow=c(1, 1))
plot(hp.nooutlier$sqft , exp(predict(fit5))-1000, pch=16 , ylab="Price ($000)", xlab="Square feet")
points(hp.nooutlier$sqft ,hp.nooutlier$list.price, col="gray46", cex=0.5)
legend("bottomright", legend=c("Actual Price", "Predicted Price"), inset=0.1, col=c("gray46", "black"),
        pch=c(1, 16))

hist(resid(fit5), breaks=60, main="Histogram of residuals (model 5) - \n log transformed list price + 1000 \n outlier removed", xlab="", cex=0.8, cex.lab=1.25)



fit6 <- lm(log(list.price + 1000) ~ log(sqft) + year.built, data=hp.nooutlier)
summary(fit6)


##Bootstrapping
library(boot)

boot.example <- data.frame("orig" = 1:10)
boot.example$sample1 <- sample(boot.example$orig, 10, TRUE)
boot.example$sample1 <- boot.example$sample1 [order(boot.example$sample1 )]
boot.example$sample2 <- sample(boot.example$orig, 10, TRUE)
boot.example$sample2 <- boot.example$sample2 [order(boot.example$sample2 )]
boot.example$sample3 <- sample(boot.example$orig, 10, TRUE)
boot.example$sample3 <- boot.example$sample3 [order(boot.example$sample3 )]
boot.example$sample4 <- sample(boot.example$orig, 10, TRUE)
boot.example$sample4 <- boot.example$sample4 [order(boot.example$sample4 )]
boot.example$sample5 <- sample(boot.example$orig, 10, TRUE)
boot.example$sample5 <- boot.example$sample5 [order(boot.example$sample5 )]
boot.example



#boostrapping example
library(simpleboot)
fit7 <- lm(list.price ~  year.built + sqft + beds + baths, data=hp.nooutlier )
summary(fit7)

hist(resid(fit7), breaks=50)
kurtosis(resid(fit7), type=2)
boxplot(resid(fit7))

fit7.boot <- lm.boot(fit7, R=10000)
summary(fit7.boot)
plot(fit7.boot)


tCrit <- qt(0.025, df= fit7$df.residual,  lower.tail=FALSE)


upperCIs  <- summary(fit7.boot)$orig.lm$coefficients  + tCrit * summary(fit7.boot)$stdev.params
lowerCIs <- summary(fit7.boot)$orig.lm$coefficients  - tCrit * summary(fit7.boot)$stdev.params


pValues <- pt(abs(summary(fit7.boot)$orig.lm$coefficients) /   summary(fit7.boot)$stdev.params, df=271, lower.tail=FALSE) *2



#Get standard errors
ses <- as.data.frame(round(cbind(summary(fit7)$coefficients[,2], summary(fit7.boot)$stdev.params), 3))
names(ses) <- c("Normal theory", "Bootstrap")
ses

#Get confidence intervals
round(cbind(confint(fit7), lowerCIs, upperCIs), 3)

#get p-values
round(data.frame("Normal Theory"= summary(fit7)$coefficients[,4], "Boostrapped"=pValues), 3)

round(data.frame(perc(fit7.boot)[1,], perc(fit7.boot)[2,], lowerCIs,  upperCIs), 4)




##Heteroscedasticity




with(hp, plot(list.price, sqft))




fit8 <- lm(list.price ~ sqft, data=hp.nooutlier)
with(hp.nooutlier, plot(sqft, list.price, xlab="Size of house (square feet)", ylab="List price"))
abline(fit8)
#Show that the graph looks the same with the predicted value instead of the predictor.
with(hp.nooutlier, plot(predict(fit8), list.price, xlab="Size of house (square feet)", ylab="List price"))
abline(fit8)

plot(predict(fit8), resid(fit8), xlab="Predicted Value", ylab="Residual" )
abline(h=0)

#Make some artificial plots
#No heteroscedasticity
library(MASS)  #Load the MASS package to get the MVRnorm function
s <- matrix(c(1, 0, 0, 1), nrow=2)

par(mfrow=c(2, 1))
noHet <- data.frame(pred=runif(500) * 10, out=rnorm(500))
noHet$resid <- resid(lm(out ~ pred, data=noHet))

with(noHet, plot(pred, resid, xlab="Predicted Value", ylab="Residual"))
  abline(h=0)

het <- noHet
het$resid <- het$resid * (het$pred + 1)
with(het, plot(pred, resid, xlab="Predicted Value", ylab="Residual"))
abline(h=0)

par(mfrow=c(1, 1))


#White's test
install.packages("bstats")
library(bstats)
white.test(fit8)

#sandwich estimator
install.packages("car")
library(car)

hcvar.fit8 <- hccm(fit8)
se.fit8 <- sqrt(diag(hcvar.fit8))
se.fit8

#Get confidence intervals 
tCrit <- qt(0.025, df= fit8$df.residual,  lower.tail=FALSE)
upperCI  <- summary(fit8)$coefficients[,1]  + tCrit * se.fit8
lowerCI <- summary(fit8)$coefficients[,1]  - tCrit * se.fit8

#Get p-values
pValues <- round(pt(abs(summary(fit8)$coefficients[,1]) /   se.fit8, df=fit8$df.residual, lower.tail=FALSE) *2, 3)






fit8.boot <- lm.boot(fit8, R=10000)
summary(fit8.boot)


#Heteroscedastic sailing plot
plot(matrix(c(100, 0,
       5, 0), nrow=2
        ), type="l", ylim=c(0, 100), xlab="$", ylab="Days spent sailing")
lines(c(0, 100), c(5, 80), lty=2)
lines(c(0, 100), c(2.5, 42.5), lwd=3)


legend(x=0, y=80, legend=c("Like sailing", "Don't like sailing", "Average line"), lty=c(2, 1, 1), lwd=c(1, 1, 3))



fit9 <- lm(log(list.price+1000) ~ sqft, data=hp.nooutlier)
summary(fit9)
fit9 <- lm(list.price ~ beds, data=hp.nooutlier)
with(hp.nooutlier, plot(beds, list.price, xlab="Number of bedrooms",  ylab="List price", ylim=c(0, 1000)))
abline(fit9)

#look at the means
as.data.frame(round(tapply(hp.nooutlier$list.price, hp.nooutlier$beds, mean), 1))

#Residual plot for linearity
plot(predict(fit9), resid(fit9), xlab="Predicted Values", ylab="Residuals")
abline(h=0)


#

fit10 <- lm(hp.nooutlier$list.price ~ sqft + year.built + beds + lot.size + parking.spot, data=hp.nooutlier)
summary(fit10)
plot(predict(fit10), resid(fit10), xlab="Predicted Values", ylab="Residuals")
scatter.smooth(predict(fit10), resid(fit10), xlab="Predicted Values", ylab="Residuals")
abline(h=0)


#clustered data
y <- c(rep(3, 3),rep(4, 4), rep(2, 2), rep(5, 5), rep(2, 2), rep(4,4))
mean(y)
sd(y)

#ecological fallacy
par(mfrow=c(2, 1))
topeka <- cbind(c(3, 4, 5), c(100, 250, 500))
manhattan <- cbind(c(1, 2, 3), c(900, 1000, 1200))
plot(manhattan, ylim=c(0, 1400), xlim=c(1, 5), pch=16, xlab="Number of bedrooms", ylab="Price")
points(topeka)  
abline(lm(topeka[,2] ~ topeka[,1]), lty=2)
abline(lm(manhattan[,2] ~ manhattan[,1]))
mean(manhattan)

legend("bottomleft", legend=c("Manhattan", "Topeka"), lty=c(1, 2), pch=c(16, 1))

both <- rbind(topeka, manhattan)
plot(manhattan, ylim=c(0, 1400), xlim=c(1, 5), pch=16, xlab="Number of bedrooms", ylab="Price")
points(topeka)
abline(lm(both[,2] ~ both[,1]))
legend("bottomleft", legend=c("Manhattan", "Topeka", "Fit line"), lty=c(NA, NA, 1), pch=c(16, 1, NA))


abline(lm(both[,2] ~ both[,1]), lwd=2)

