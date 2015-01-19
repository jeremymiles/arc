
#First load some libraries. 
#It's good practice to load what you need at the start of the file.
#We'll be using functions from these libraries.
#MASS should be installed with R, you might need to install QuantPsyc.
library(MASS)
library(QuantPsyc)

#create d, the data
d <- c(2, 4, 1, 0, 3)

#Calculate mean
mean(d)

sd(d)
#calculate error when model is 1
sqrt(sum((d - 1)^2) / 4)
#calculate error when model is 2
sqrt(sum((d - 4)^2) / 4)
#calculate error when model is 4
     sqrt(sum((d - 4)^2) / 5)

sd(d)

mean(d)

#draw chart of the relationship between error and the model.
#generate range of values for model

possibleModels <- seq(0, 5, 0.05)

#for each possible model, calculate the error
#we'll use the sapply() function, because it's quick
errors <- sapply(possibleModels, function(x) sqrt(sum((d - x)^2) / length(d)) )

plot(possibleModels, errors, type="l", xlab="Value for Model", ylab="Error")
abline( v=2, lty=3)
abline( h=1.41, lty=3)


#draw chart showing relationship between mean and error

modelVals <- seq(0, 4, 0.01)
errorVals <- sapply(modelVals, function(x) sqrt(sum((d - x)^2) / 4))

plot(modelVals, errorVals, type="l", ylab="Values for Model", xlab="Amount of Error")
abline(h=1.58)
abline(v=2.0)


#draw the normal dist

lb=80; ub=120

mean=100; sd=15
lb=80; ub=120

x <- seq(-4,4,length=100)*sd + mean
hx <- dnorm(x,mean,sd)

plot(x, hx, type="n", xlab="Score", ylab="Density",
      axes=TRUE)

lines(x, hx)

abline(v=100, untf = FALSE, lwd=4)
abline(v=107, untf = FALSE, lwd=1)
text(x=100, y = 0.015, pos=2, "True \n Mean")
text(x=107, y = 0.005, pos=4, "Sample \n Mean")
points(120, 0.02, type="p", pch=19, cex=2)
arrows(120, 0.02, 100, 0.02)
arrows(120, 0.02, 107, 0.02, lwd=3)



#Calculation of error for values of model other than mean
x <- c(2, 4, 1, 0, 3)

sd.wrong <- sqrt((sum(x - 1)^2)/4)



setwd("C:/Users/Jeremy/Dropbox/public/arcdata")

write.csv(hp, "housePrices.csv")
library(foreign)

library(psych)


hp <- read.csv("https://github.com/jeremymiles/ARC2/blob/master/housePrices.csv")



hp <- subset(hp, !is.na(sqft))

describe(hp)

table(hp$parking.type)
hp$parking.type <- ifelse(hp$parking.type == "Garage", "Garage", "Other")

plot(hp$sqft, hp$list.price, xlab="Area (square feet)", ylab="Price ($000)")
abline(lm(list.price~sqft, data=hp)) 


describe(hp)


x <- 0:10
y <- x + 2
plot (x, y, type="l", xlim=c(0, 10), ylim=c(1, 12))



shortHP <- head(hp[c("list.price", "sqft")], n=5)
mean(shortHP$list.price) 



sum(preds - shortHP$list.price)^2
sum(mean(shortHP$list.price) - shortHP$list.price)^2


#Get the first five cases of hp, and call it shortHP
shortHP <- head(hp[c("list.price", "sqft")], n=5)
plot(shortHP$sqft, shortHP$list.price, xlab="Area (square feet)", ylab="Price ($000)", ylim=c(180, 280))
abline(h=mean(shortHP$list.price))
for(loop in 1:nrow(shortHP)){
  with(shortHP, arrows(sqft[loop] ,mean(list.price), sqft[loop], list.price[loop] ) ) 
}
text(1250, mean(shortHP$list.price)-3.5, paste("Mean list price =", round(mean(shortHP$list.price),1), sep=""))
text(shortHP$sqft[2] + 10, mean(shortHP$list.price[2] + 4), paste("List price = ",shortHP$list.price[2] , sep="" ))
text(shortHP$sqft[2] - 45, sum( shortHP$list.price[2], mean(shortHP$list.price) )/2 , 
     paste("Residual = \n", shortHP$list.price[2], " - ", round(mean(shortHP$list.price),1) , " = \n ",
           round(shortHP$list.price[2] -  mean(shortHP$list.price),1) , sep="" )
    )




# Now draw the graph again with line of best fit.
csummary(preds)
for(loop in 1:nrow(shortHP)){
  with(shortHP, arrows(sqft[loop] ,preds[loop], sqft[loop], list.price[loop] ) ) 
}

text(1350, 190, "slope = 58.485 + 0.139 x sqft")

text(shortHP$sqft[2]-45 , sum( (shortHP$list.price[[2]]), preds[[2]] )/2, #mean doesn't work - not sure why
     paste("Residual = \n", shortHP$list.price[2], " - ", round(preds[2], 1), " = \n ", shortHP$list.price[2] - 
             round(preds[2],1) , sep="" )
  )


text(shortHP$sqft[2],  preds[[2]] - 10,
     paste("Predicted value  = \n", round(preds[1]), sep="" ))
     

tablex <- cbind(shortHP, rep(mean(shortHP$list.price), 5),resid(intOnly), preds, resids )


#calculate SSE
sseMean <- sum((mean(shortHP$list.price) - shortHP$list.price)^2)
sseReg <- sum((preds - shortHP$list.price)^2)


meanLP <- mean(shortHP$list.price) 
sum((meanLP - shortHP$list.price)^2)


fit1 <- lm(list.price ~ sqft, data=shortHP)
summary(fit1)


fit0 <- lm(list.price ~ 1, data=shortHP)
summary(fit0)


#graph for the constant
plot(shortHP$sqft, shortHP$list.price, xlab="Area (square feet)",
     ylab="Price ($000)", ylim=c(0, 280), xlim=c(0, 1500), , axes=FALSE)
axis(x, pos=0)
abline(lm(list.price~sqft, data=shortHP)) 
arrows(100, 40, 0, 58)
text(180, 35, "When x = 0, y = 58.48")
axis(2, pos=0)
axis(1, pos=0)

#Create intercept
shortHP$b0 <- 1
summary(lm(list.price~sqft + b0 + 0, data=shortHP))

shortHP$preds <- predict(fit1)
shortHP$resids <- resid(fit1)
shortHP

#basic plot
plot(shortHP$sqft, shortHP$list.price)
#add axis labels
plot(shortHP$sqft, shortHP$list.price, xlab="Area (square feet)",
     ylab="Price ($000)")
#add regression line
abline(lm(list.price~sqft, data=shortHP)) 


#predicted values and residuals

fit1 <- lm(list.price ~ sqft, data=shortHP)
shortHP$preds <- predict(fit1)
shortHP$preds

shortHP$resids <- resid(fit1)
shortHP$resid



shortHP$zlist.price <- scale(shortHP$list.price)
shortHP$zsqft <- scale(shortHP$sqft)

shortHP[c("list.price", "zlist.price", "sqft", "zsqft")]



plot(shortHP$zsqft, shortHP$zlist.price, xlab="Standardized Area",
     ylab="Standardized List Price")

#add regression line
abline(lm(zlist.price~zsqft, data=shortHP)) 
arrows(0, 0, 1, 0,code=0, lty=2)
arrows(1, 0, 1, 0.701,code=0, lty=2)
text( 0.5,-0.1, "Move 1 unit on x axis")
text( 1.25,0.2, "Increase of 0.70 \n units on y axis")

lm(list.price ~ sqft, data=shortHP)

0.1385 * sd(shortHP$sqft) /  sd(shortHP$list.price) 

#Set up matrices for plots.
r00 <- matrix(c(1, 0, 0, 1), 2, 2)
r01 <- matrix(c(1, 0.1, 0.1, 1), 2, 2)
r03 <- matrix(c(1, 0.3, 0.3, 1), 2, 2)
r05 <- matrix(c(1, 0.5, 0.5, 1), 2, 2)
r07 <- matrix(c(1, 0.7, 0.7, 1), 2, 2)
r09 <- matrix(c(1, 0.9, 0.9, 1), 2, 2)

d00 <- as.data.frame(mvrnorm(n = 200, mu=c(0, 0), Sigma=r00, empirical=TRUE))
d01 <- as.data.frame(mvrnorm(n = 200, mu=c(0, 0), Sigma=r01, empirical=TRUE))
d03 <- as.data.frame(mvrnorm(n = 200, mu=c(0, 0), Sigma=r03, empirical=TRUE))
d05 <- as.data.frame(mvrnorm(n = 200, mu=c(0, 0), Sigma=r05, empirical=TRUE))
d07 <- as.data.frame(mvrnorm(n = 200, mu=c(0, 0), Sigma=r07, empirical=TRUE))
d09 <- as.data.frame(mvrnorm(n = 200, mu=c(0, 0), Sigma=r09, empirical=TRUE))

names(d00) <- c("x", "y")
names(d01) <- c("x", "y")
names(d03) <- c("x", "y")
names(d05) <- c("x", "y")
names(d07) <- c("x", "y")
names(d09) <- c("x", "y")

par(mfrow=c(3, 2))
plot(d00$x, d00$y, xlab="X", ylab="Y", main="r = 0.0", xlim=c(-3, 3), ylim=c(-3, 3))
#abline(lm(y~x, data=d00)) 
plot(d01$x, d01$y, xlab="X", ylab="Y", main="r = 0.1", xlim=c(-3, 3), ylim=c(-3, 3))
#abline(lm(y~x, data=d01)) 
plot(d03$x, d03$y, xlab="X", ylab="Y", main="r = 0.3", xlim=c(-3, 3), ylim=c(-3, 3))
#abline(lm(y~x, data=d03))
plot(d05$x, d05$y, xlab="X", ylab="Y", main="r = 0.5", xlim=c(-3, 3), ylim=c(-3, 3))
#abline(lm(y~x, data=d05)) 
plot(d07$x, d07$y, xlab="X", ylab="Y", main="r = 0.7", xlim=c(-3, 3), ylim=c(-3, 3))
#abline(lm(y~x, data=d07)) 
plot(d09$x, d09$y, xlab="X", ylab="Y", main="r = 0.9", xlim=c(-3, 3), ylim=c(-3, 3))
#abline(lm(y~x, data=d09)) 


par(mfrow=c(1, 1))

#correlation in R
cor(shortHP$list.price, shortHP$sqft)

fit1 <- lm(list.price~sqft, data=shortHP)
summary(fit1)
lm.beta(fit1)




###Section 4
##Section4.1 - covariance
hp <- read.csv("http://raw.github.com/jeremymiles/arc/master/housePrices.csv")
#Shorten the data to the first five - and only list.price and sqft
shortHp <- hp[1:5, c("list.price", "sqft")]

#Get the difference between each value and the mean.
shortHp$"x-xbar" <- shortHp$sqft - mean(shortHp$sqft)
shortHp$"y-ybar" <- shortHp$list.price - mean(shortHp$list.price)
mean(shortHp$sqft)
mean(shortHp$list.price)
shortHp$"x-xbar * y-ybar" <- shortHp$"x-xbar" * shortHp$"y-ybar"
round(shortHp, 2)


covXY <- sum(shortHp$"x-xbar * y-ybar") / 4
covXY / sqrt(var(shortHp$sqft )* var(shortHp$list.price ))
cor(shortHp$sqft, shortHp$list.price )

cov(shortHp$sqft, shortHp$list.price )

#Create temporary random error variable with mean 0 and add it to the list.price.
corXY <- covXY / (sqrt(var(shortHp$sqft) * var(shortHp$list.price)))

#Get the standard deviations, to calculate the slope.
sdx <- sd(shortHp$sqft)
sdy <- sd(shortHp$list.price)

b <- corXY * sdy / sdx
b
#Calculate the intercept
meany <- mean(shortHp$list.price)
meany
meanx <- mean(shortHp$sqft)
meanx
c <-  meany - b * meanx
c
