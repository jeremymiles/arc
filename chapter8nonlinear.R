
#Yerkes dodson law

x <- -10:10
y <- -x^2
plot(x, y, type="l", yaxt="n", xaxt="n", ylab="Performance", xlab="Arousal")

#Speed and kinetic energy 8.2
plot(1:10, (1:20)^2, type="l", yaxt="n", xaxt="n", ylab="Energy", xlab="Speed")

#6.2 learning curvep
plot(seq(1, 10, 0.1), -1/seq(1, 10, 0.1), type="l", yaxt="n", xaxt="n", ylab="Skill", xlab="Time")


#Figure 6.3
x <- 1:20

plot(x, x*3, type="p", xlab="y", ylab="y", lwd=2)
lines(x, x * 3)
lines(x, x*2, lty=2, lwd=2)
points(x, x*2)
lines(x, x*0.5, lty=3, lwd=2)
points(x, x*0.5)
lines(x, x + 10, lty=4, lwd=2)
points(x, x + 10)
lines(x, x *0.5 + 10, lty=5, lwd=2)
points(x, x *0.5 + 10)

legend(x=1,y=55, legend=c("x * 3", 
                          "x * 2",
                          "x * 0.5",
                          "x + 10",
                          "x * 0.5 + 10"), 
       lty=c(1:5), pch=rep(1, 5) )

#6.4 quadratic

plot(1:20, (1:20)^2, type="l", xlab="x", ylab="y")
points(1:20, (1:20)^2)

#6.5 
plot(1:20, (1:20) * 2 + (1:20)^2 * 1 + 50, xlab="x", ylab="2*x + 1*x^2 + 5", ylim=c(0, 500) )  
lines(1:20, (1:20) * 2 + (1:20)^2 * 1 + 50 )

#6.6
plot(1:20, (1:20) * 50 + (1:20)^2 * -1.2 + 50, xlab="x", ylab="50*x - 1.*x^2 + 50")  
lines(1:20, (1:20) * 50 + (1:20)^2 * -1.2 + 50,)

#6.7
plot(1:20, -0.5 * (1:20)  + 0.2 *  (1:20)^2  - 0.02 *  (1:20)^3 + 100, xlab="x", ylab="y")
lines(1:20, -0.5 * (1:20)  + 0.2 *  (1:20)^2  - 0.02 *  (1:20)^3 + 100, lty=1)
     
lines(1:20, 12 * (1:20) - 1.5  * (1:20)^2 + 0.06 * (1:20)^3 , lty=2)
points(1:20, 12 * (1:20) - 1.5  * (1:20)^2 + 0.06 * (1:20)^3 )


legend(x=2,y=80, legend=c("-0.5*x + 0.2*x^2 - 0.02*x^3 + 100", 
                          "12 * x - 1.5*x^2 + 0.06*x^3"),
       lty=c(1:2), pch=rep(1, 2) )

plot(1:20, log(1:20), xlab="x", ylab="log(x)")
lines(1:20, log(1:20))


#6.8
plot(1:20, log(1:20), xlab="x", ylab="log(x)")
lines(1:20, log(1:20))


plot(seq(0.1, 3, 0.1), 1/seq(0.1, 3, 0.1), xlab="x", ylab="1/x")
lines(seq(0.1, 3, 0.1), 1/seq(0.1, 3, 0.1))


##Example
#Download the data
hp <- read.csv("http://raw.github.com/jeremymiles/arc/master/housePrices.csv")
#Remove the outlier.
hp <- hp[-51, ]

#Figure 6.10
plot(hp$sqft, hp$list.price, xlab="House size (square feet)", ylab="List Price ($000)")


hp$s2 <- hp$sqft^2
hp$s3 <- hp$sqft^3
hp$s4 <- hp$sqft^4
hp$s5 <- hp$sqft^5
linear     <- lm(list.price ~ sqft               , data=hp)
quadratic  <- lm(list.price ~ sqft + s2          , data=hp)
cubic      <- lm(list.price ~ sqft + s2 + s3     , data=hp)
quartic    <- lm(list.price ~ sqft + s2 + s3 + s4    , data=hp)
quintic    <- lm(list.price ~ sqft  + s2 + s3 + s4 + s5, data=hp)




summary(linear)$coefficients
round(summary(quadratic)$coefficients, 6)
round(summary(cubic)$coefficients, 9)
summary(quartic)$coefficients
summary(quintic)$coefficients


summary(linear)
summary(quadratic)
summary(cubic)

signif(summary(cubic)$coefficients, 3)


summary(linear)$r.squared
summary(quadratic)$r.squared
summary(cubic)$r.squared
summary(quartic)$r.squared
summary(quintic)$r.squared


#Draw graphs
hp <- hp[order(hp$sqft),]
plot(hp$sqft, hp$list.price, xlab="Size of House (square feet)", ylab="Predicted Price (000)")
lines(hp$sqft, predict(linear   , newdata=hp), lwd=2)
lines(hp$sqft, predict(quadratic, newdata=hp), lwd=2, lty=2)
lines(hp$sqft, predict(cubic    , newdata=hp), lwd=2, lty=3)
legend("bottomright",legend=c("Linear", "Quadratic", "Cubic"), lty=c(1, 2, 3))


#load car package, for VIF function
install.packages("car")

#collinearity
round(cor(hp[c("sqft", "s2", "s3")]), 2)
cbind(round(vif(cubic), 1), round(1-1/vif(cubic), 3))


#Multiple non-linear
mlinear     <- lm(list.price ~ sqft +           year.built + lot.size + 
                    parking.spots, data=hp)
mquadratic  <- lm(list.price ~ sqft + s2 +      year.built + lot.size + 
                    parking.spots, data=hp)
mcubic      <- lm(list.price ~ sqft + s2 + s3 + year.built + lot.size + 
                    parking.spots, data=hp)


round(summary(mlinear)$coefficients, 4)
round(summary(mquadratic)$coefficients, 6)
round(summary(mcubic)$coefficients, 8)

summary(mlinear)
summary(mquadratic)
summary(mcubic)

##Interpreting cubic
#Year built
mean(hp$year.built)


#Get proportion with different parking spots
prop.table(table(hp$parking.spots))


hp.preds <- data.frame(sqft=800:7000, 
                       year.built=mean(hp$year.built),
                       parking.spots=2, 
                       lot.size=median(hp$lot.size)
                       )

head(hp.preds)

hp.preds$s2 <- hp.preds$sqft^2
hp.preds$s3 <- hp.preds$sqft^3

#get predicted values
hp.preds$linearPred <- predict(mlinear,    newdata = hp.preds)
hp.preds$quadPred   <- predict(mquadratic, newdata = hp.preds)
hp.preds$cubPred    <- predict(mcubic,     newdata = hp.preds)

plot(hp.preds$sqft, hp.preds$linearPred, type="l", xlab="House Size (Square Feet)", ylab="Predicted Price ($000)", lwd=2)
lines(hp.preds$sqft, hp.preds$quadPred, lty=2, lwd=2)
lines(hp.preds$sqft, hp.preds$cubPred, lty=3, lwd=2)
legend("bottomright",legend=c("Linear", "Quadratic", "Cubic"), lty=c(1, 2, 3))

mean(hp$list.price)

