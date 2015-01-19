

library(lmSupport)
library(ppcor)

gm_mean = function(x, na.rm=TRUE){
  exp(sum(log(x[x > 0]), na.rm=na.rm) / length(x))
}
x <- rnorm(1000000)
y <-    1:1000000 %% 1000

tapply(rep(y, 1000), x, mean)


#Figure 4.1
library(QuantPsyc)
hp <- read.csv("http://raw.github.com/jeremymiles/arc/master/housePrices.csv")

fit1 <- lm(list.price ~ sqft, data=hp)
summary(fit1)
confint(fit1)
fit2 <- lm(list.price ~ sqft + lot.size, data=hp)
summary(fit2)
confint(fit2)
anova(fit2)
fit2a <- lm(list.price ~   lot.size + sqft, data=hp)
anova(fit2a)

hp$resid <- resid(fit2)
hp$pred <- predict(fit2)

head(hp[c("sqft", "lot.size", "list.price", "resid", "pred")])

confint(fit2)
lm.beta(fit2)


fit0 <- lm(list.price~1, data=hp)
anova(fit0)
fit1 <- lm(list.price~sqft, data=hp)
summary(fit1)
plot(hp$sqft, hp$list.price, xlab="Area (square feet)", ylab="Price ($000)")
abline(lm(list.price~sqft, data=hp)) 
minVal <- which( resid(fit1) == min(resid(fit1)))
points(hp$sqft[minVal], hp$list.price[minVal],  cex=5 )

#Get the residuals from fit1 to regress onto lot.size. 
hp$rf1 <- resid(fit1)

fitRes <- lm(rf1 ~ lot.size, data=hp)
summary(fitRes)
fitRes$coefficients


with(hp, cor.test(beds, sqft))

fit3 <- lm(list.price~sqft, data=hp)
summary(fit3)

fit4 <- lm(list.price~ beds, data=hp)
confint(fit4)
summary(fit4)
fit5 <- lm(list.price ~ sqft +  beds, data=hp)
summary(fit5)

fit6 <- lm(list.price ~ parking.spots, data=hp)
summary(fit6)
fit7 <- lm(list.price ~ year.built + sqft + lot.size, data=hp)
summary(fit7)


fit5 <- lm(list.price~sqft + year.built, data=hp)
summary(fit5)


fit1 <- lm(list.price~sqft, data=hp)

fit5 <- lm(list.price ~ last.sale.price, data=hp)
fit6 <- lm(list.price ~ year.built + last.sale.price + parking.spots, data=hp)
summary(fit5)
summary(fit6)
lm.beta(fit5)
lm.beta(fit6)

#Demonstration that regression removes con

install.packages("lavaan")
library(semPlot)
library(lavaan)
set.seed(1234)
x1 <- rnorm(1000)
x2 <- rnorm(1000)  +  x1
y <-  rnorm(1000)  +  x1
d <- as.data.frame(cbind(x1, x2, y))
cor(d)
summary(lm(y ~ x1 + x2, data=d))
confint(lm(y ~ x1 + x2, data=d))


#Demonstrate effects of adding parameters on standard errors
fit6 <- lm(list.price ~ parking.spots, data=hp)
fit7 <- lm(list.price ~ parking.spots + year.built, data=hp)
summary(fit6)
summary(fit7)
anova(fit6, fit7)




###Calculate estimates using matrix algebra 
##Calculate using correlation matrix.
R <- cor(hp[c("sqft", "lot.size", "list.price") ])
Rxx <- as.matrix(R[1:2, 1:2])
Rxy <- R[1:2, 3]
solve(Rxx) %*% (Rxy)


##Cakculate estimates using covariance matrix
R <- cov(hp[c("sqft", "lot.size", "list.price") ])
Rxx <- as.matrix(R[1:2, 1:2])
Rxy <- R[1:2, 3]
solve(Rxx) %*% (Rxy)


###Calculate  estimates using matrix algebra from raw data.
shortHp <- hp[1:5, c("sqft", "lot.size", "list.price")]
augShortHp <- cbind(1, shortHp)
names(augShortHp)[1] <- "constant"

X <- as.matrix(augShortHp[1:3])
Y <- as.matrix(augShortHp[4])

solve(t(X) %*% X) %*% (t(X) %*% Y)

B=(X^' X)^(-1) X'Y
   
   
   
   
   
   #3D scatterplot
   
  d <- subset(hp[1:6,])
  
  
  hpshort <-subset(hp, hp$lot.size < 20000)
  s3d <- with(hpshort, scatterplot3d( lot.size , sqft,  list.price,pch=16, 
                type="h"), zlim=c(100, 500))
  fit4plot <- lm(list.price ~ lot.size +sqft , data=hp)
  s3d$plane3d(fit4plot, lty="solid")
  
  
  
  
  
  
  #Partial correlation
  fit6 <- lm(list.price ~ sqft, data=hp)
  summary(fit6)
  fit7 <- lm(list.price ~ sqft + parking.spots, data=hp)
  summary(fit7)
  anova(fit6, fit7)

  pcor(hp[c("list.price", "sqft", "parking.spots")])
  
  
  #hierarchical modelsl 
  
  h1.fit <- lm(list.price ~ year.built + lot.size, data=hp)
  summary(h1.fit)
  
  h2.fit <- lm(list.price ~ year.built + lot.size + sqft, data=hp)
  summary(h2.fit)

h2.formula <- update.formula(h1.fit, . ~ . + sqft )
h2.fit <- lm(h2.formula, data=hp)
summary(h2.fit)
modelCompare(h1.fit, h2.fit)

h3.formula <- update.formula(h2.fit, . ~ . + beds + baths )
h3.fit <- lm(h3.formula, data=hp)
summary(h3.fit)
modelCompare(h2.fit, h3.fit)





##Do AIC Stuff


RSS1 <- deviance(fit2)   ## UNSCALED sum of squares
RSS2 <- sum((hp$list.price-fitted(fit2))^2)  ## ditto, from first principles
AIC3 <- nrow(hp)*log(RSS1/nrow(hp))+2*2 ## formula used within extractAIC

(AIC3 + 2 - 2*(-nrow(hp)/2*(log(2*pi)+1)))
AIC(fit2)  

fit0 <- lm(list.price ~ 1, data=hp)
AIC(fit0)

fitAll <- lm(list.price ~ beds + baths + sqft + lot.size + year.built + parking.spots + last.sale.price, data=hp )
fitStep <- stepAIC(fitAll)
summary(fitStep)
fitFW <- stepAIC(fitAll, direction="forward")
summary(fitFW)

fitBW <- stepAIC(fitAll, direction="backward")
summary(fitBW)


#generate random data for stepwise regression
random.data <- as.data.frame(matrix(runif(501000), nrow=1000))
names(random.data) <- c("y", paste0("x", 1:500))

random.fit <- lm(y ~ . , data=random.data)
#This takes a while to run - be careful
step(random.fit)
