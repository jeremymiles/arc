

rm(list=ls())






#Chapter 2

library(psychometric)
library(QuantPsyc)
library(psych)
library(lmPerm)
library(Hmisc)
library(boot)
library(simpleboot)

CIr(r=0.361, n=30)

#Use set seed to ensure we get the same results every time.
set.seed(12345)
s10 <- sapply(1:10, function(x) (rnorm(mean=100, s=15, n=10)))
s100 <- sapply(1:10, function(x) (rnorm(mean=100, s=15, n=100)))

m10 <- apply(s10, 2, mean)
m100 <- apply(s100, 2, mean)

round(cbind(m10, m100), 2)

sd(m10) ""
sd(m100)

sd <- 1
m <- 0

x <- seq(-4,4,length=100)*sd + m
hx <- dnorm(x,m,sd)

plot(x, hx, type="n", xlab="IQ Values", ylab="Density",
     main="Normal Distribution", axes=FALSE)




barplot(  rep(1/6, 6), ylim=c(0, 0.2), names=1:6, xlab="Die Roll", ylab="Probability")


cord.x <- c(-4,seq(-4,-1.96,0.01),-1.96) 
cord.y <- c(0,dnorm(seq(-4,-1.96,0.01)),0) 
curve(dnorm(x,0,1),xlim=c(-4,4), xlab="Standardized Value", ylab="Density")
polygon(cord.x,cord.y,col='grey')

cord.x <- c(4,seq(4,1.96,-0.01),1.96) 
cord.y <- c(0,dnorm(seq(4,1.96,-0.01)),0) 
#curve(dnorm(x,0,1),xlim=c(-4,4), xlab="Standardized Value", ylab="Density")
polygon(cord.x,cord.y,col='grey')

text(x=0, y = 0.1, pos=3, "0.025 probability of \n a value below -1.96\nand 0.025 above 1.96")
arrows( -1, 0.1, -1.96, 0.025)
arrows( 1, 0.1, 1.96, 0.025)


points(120, 0.02, type="p", pch=19, cex=2)
arrows(120, 0.02, 100, 0.02)
arrows(120, 0.02, 107, 0.02, lwd=3)


hp <- read.csv("https://dl.dropboxusercontent.com/u/24381951/arcdata/housePrices.csv")

 sd(hp$list.price) / sqrt(length(hp$list.price))

new.list.price <- c(hp$list.price, NA)
sd(new.list.price, na.rm=TRUE) / sum(!is.na(new.list.price))

new.list.price[!is.na(new.list.price)]

sd(new.list.price[!is.na(new.list.price)]) / sqrt(length(new.list.price[!is.na(new.list.price)]))


describe(hp$list.price)

fit1 <- lm(hp$list.price ~ 1)
summary(fit1)

t.test(hp$list.price)

length(hp[, 1])

#sample first 100 houses.
lp.1.100 <- hp$list.price[1:100]
get CIs
t.100 <- t.test(lp.1.100)
t.all <- (t.test(hp$list.price))

plot(as.factor(c("All", "First 100")), c(mean(hp$list.price),
                                         (mean(lp.1.100))), 
     ylim=c(200, 400), type="p", ylab="List Price ($000)", xlab="Sample")
arrows(x0=1,  y0=mean(hp$list.price), x1=1, y1=t.all$conf.int[1], angle=90)
arrows(x0=1,  y0=mean(hp$list.price), x1=1, y1=t.all$conf.int[2], angle=90)
arrows(x0=2,  y0=mean(lp.1.100), x1=2, y1=t.100$conf.int[1], angle=90)
arrows(x0=2,  y0=mean(lp.1.100), x1=2, y1=t.100$conf.int[2], angle=90)



#create t table
df <- c(10, 20, 30, 50, 100, 200, 276)
round(cbind( 
  (sapply(df, function(x) qt(0.025, df=x))),
  (sapply(df, function(x) qt(0.0050, df=x))) ), 4)


t.test(hp$list.price)
fit1 <- lm(hp$list.price ~ 1)
confint(fit1)

mean(hp$list.price) 

pp <- qt(0.025, df=sum(!is.na(hp$list.price)))
se <- sd(hp$list.price, na.rm=TRUE) / sqrt(sum(!is.na(hp$list.price)))
mean(hp$list.price, na.rm=TRUE) + se * pp
mean(hp$list.price, na.rm=TRUE) - se * pp





#get standard error of list price

d <- 300 - hp$list.price
se.lp <- sd(hp$list.price) / sqrt(length(hp$list.price))
t.lp <- 

  t <- mean(d) / se.lp

df <- sum(!is.na(d)) - 1

2 * pt(t, df, lower.tail=FALSE)

t.test(hp$list.price, mu=300)

 fit2 <- lm(list.price ~ sqft, data=hp)

summary(fit2)
confint(fit2)

#Get confidenceintervals

sqft <- seq(min(hp$sqft), max(hp$sqft))

preds.fit2 <- predict(fit2,  newdata=data.frame(sqft),interval="confidence") 
head(preds.fit2)

preds.fit2 <- as.data.frame(preds.fit2)

plot(hp$sqft, hp$list.price, xlab="Area (square feet)", 
     ylab="List Price ($000)", pch=19, cex=0.6)
lines(sqft, preds.fit2$fit)

lines(sqft, preds.fit2$lwr, lty=2)
lines(sqft, preds.fit2$upr, lty=2)

polygon(c(sqft,rev(sqft)),c(preds.fit2$lwr,rev(preds.fit2$upr)),col="grey", density=20) 

hp.1.10 <- hp[1:10, ]
lmp.fit3 <- lmp(list.price ~ sqft, data=hp.1.20, perm="Exact")
fit3 <- lm(list.price ~ sqft, data=hp.1.20)
summary(lmp.fit3)
summary(fit3)



#Sample from 1:10 with replacement
sample(1:10, 10, replace=TRUE)

#Boostrap the mean
jmean <- function(x, i)  return(mean(x[i]))

jmean(hp$list.price)

set.seed(12345)
bootmeans <- boot(data=hp$list.price, statistic=jmean, R=1000)



summary(bootmeans)
hist(bootmeans$t, main="", xlab="Mean")
head(round(bootmeans$t, 2), n=10)

sd(bootmeans$t)

bootmeans$t[order(bootmeans$t)][25]
bootmeans$t[order(bootmeans$t)][975]


fit1 <- lm(list.price ~ sqft, data=hp )
fit1.boot <- lm.boot(fit1, R=10000)


summary(fit1.boot)

perc(fit1.boot)