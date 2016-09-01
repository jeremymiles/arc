lm()
library(psych)
setwd("C:/Users/Jeremy/Dropbox/Applying Reg and Corr/arcdata/arc")

#Chapter 5a (or whatever it is): categorical data

hp <- read.csv("http://raw.github.com/jeremymiles/arc/master/housePrices.csv")
hp <- hp[-51, ]

hp$log.listprice <- log(hp$list.price)

means <- tapply( hp$log.listprice, hp$parking.type, mean)
tapply( hp$log.listprice, hp$parking.type, sd)
tapply( hp$list.price, hp$parking.type, sd)


hp$garage <- hp$parking.type == "Garage"
plot(tapply( hp$log.listprice, hp$garage, mean), type="l",xaxt="n", ylab="log(list price)" , xlab="Garage",
     ylim=c(5, 6))
axis(1, at=c(1, 2), labels=c(0, 1))

lm(list.price ~ garage, data=hp)
lm(log.listprice ~ garage, data=hp)

summary(lm(log.listprice ~ garage, data=hp))
png(width=800, height=480)
plot(tapply( hp$log.listprice, hp$garage, mean), type="l",xaxt="n", ylab="log(list price)" , xlab="Garage",
     ylim=c(5, 6))
axis(1, at=c(1, 2), labels=c(0, 1))
lines(x=c(0, 1.2), y=c(means[2], means[2]))
text(x=1.1, y=means[2],labels= paste0("Intercept is value of y \n when x=0: ", round(means[2], 2),"."), pos=1)

lines(x=c(1.8, 2.2), y=c(means[1], means[1]))

text(x=1.8, y=means[1] + 0.2,labels= paste0("Slope is increase value of y \n when x increases from 0 to 1:\n" , 
                                            round(means[1], 2), "-", round(means[2], 2)," = ", 
                                            round(means[2]-means[1], 2)), pos=1)
dev.off()




#Read deprivation data

imd <- read.csv("http://raw.github.com/jeremymiles/arc/master/england-imd.csv")
table(imd$region)
by(imd$imd.score, imd$region, mean)

summary(lm(imd.score ~ region, data=imd))
table(imd$region)

imd$d1london  <- imd$region == "London"
imd$d2se	    <- imd$region == "South East"   
imd$d3sw	    <- imd$region == "South West"
imd$d4wm	    <- imd$region == "West Midlands"
imd$d5nw	    <- imd$region == "North West"
imd$d6ne	    <- imd$region == "North East"
imd$d7yath	  <- imd$region == "Yorkshire and The Humber"
imd$d8em	    <- imd$region == "East Midlands"  
imd$d9ee      <- imd$region == "East of England"

fit.dummy1 <- lm(imd.score ~ d1london + d2se + d3sw + d4wm + d5nw + d6ne + d7yath + d8em + d9ee, data=imd)
summary(fit.dummy1 )
fit.dummy2 <- lm(imd.score ~  d2se + d3sw + d4wm + d5nw + d6ne + d7yath + d8em + d9ee, data=imd)
summary(fit.dummy2 )

fit.dummy3 <- lm(imd.score ~  region , data=imd)
summary(fit.dummy3 )

imd$region2 <- relevel(imd$region, "London")
fit.dummy3 <- lm(imd.score ~  region2 , data=imd)
summary(fit.dummy3 )
fit.dummy3a <- lm(imd.score ~  relevel(region, "London") , data=imd)
summary(fit.dummy3a)

imd$region2 <- relevel(imd$region, "London")
levels(imd$region2)

data.frame(describeBy(imd$imd.score, imd$region, mat=TRUE))



##Dummy code with no intercept
fit.dummy4 <- lm(imd.score ~  region - 1 , data=imd)
summary(fit.dummy4)




###Effect coding 
imd$e1.1london  <- imd$region == "London"
imd$e1.2se      <- imd$region == "South East"   
imd$e1.3sw	    <- imd$region == "South West"
imd$e1.4wm	    <- imd$region == "West Midlands"
imd$e1.5nw	    <- imd$region == "North West"
imd$e1.6ne	    <- imd$region == "North East"
imd$e1.7yath	  <- imd$region == "Yorkshire and The Humber"
imd$e1.8em	    <- imd$region == "East Midlands"  
imd$e1.9ee      <- imd$region == "East of England"

imd$e1.1london  <- ifelse(imd$region == "London", -1, imd$e1.1london)
imd$e1.2se      <- ifelse(imd$region == "London", -1, imd$e1.2se)
imd$e1.3sw      <- ifelse(imd$region == "London", -1, imd$e1.3sw)
imd$e1.4wm      <- ifelse(imd$region == "London", -1, imd$e1.4wm)
imd$e1.5nw      <- ifelse(imd$region == "London", -1, imd$e1.5nw)
imd$e1.6ne      <- ifelse(imd$region == "London", -1, imd$e1.6ne)
imd$e1.7yath    <- ifelse(imd$region == "London", -1, imd$e1.7yath)
imd$e1.8em      <- ifelse(imd$region == "London", -1, imd$e1.8em)
imd$e1.9ee      <- ifelse(imd$region == "London", -1, imd$e1.9ee)

fit.effect1 <- lm(imd.score ~   e1.2se + e1.3sw + 
                e1.4wm + e1.5nw +e1.6ne + e1.7yath + e1.8em + e1.9ee , data=imd)
summary(fit.effect1)


###Effect coding 
imd$e9.1london  <- imd$region == "London"
imd$e9.2se      <- imd$region == "South East"   
imd$e9.3sw	    <- imd$region == "South West"
imd$e9.4wm	    <- imd$region == "West Midlands"
imd$e9.5nw	    <- imd$region == "North West"
imd$e9.6ne	    <- imd$region == "North East"
imd$e9.7yath	  <- imd$region == "Yorkshire and The Humber"
imd$e9.8em	    <- imd$region == "East Midlands"  
imd$e9.9ee      <- imd$region == "East of England"


imd$e9.1london <- ifelse(imd$region == "East of England", -1, imd$e9.1london)
imd$e9.2se      <- ifelse(imd$region == "East of England", -1, imd$e9.2se)
imd$e9.3sw      <- ifelse(imd$region == "East of England", -1, imd$e9.3sw)
imd$e9.4wm      <- ifelse(imd$region == "East of England", -1, imd$e9.4wm)
imd$e9.5nw      <- ifelse(imd$region == "East of England", -1, imd$e9.5nw)
imd$e9.6ne      <- ifelse(imd$region == "East of England", -1, imd$e9.6ne)
imd$e9.7yath    <- ifelse(imd$region == "East of England", -1, imd$e9.7yath)
imd$e9.8em      <- ifelse(imd$region == "East of England", -1, imd$e9.8em)
imd$e9.9ee      <- ifelse(imd$region == "East of England", -1, imd$e9.9ee)




fit.effect9 <- lm(imd.score ~   e9.1london + e9.2se + e9.3sw + 
                    e9.4wm + e9.5nw +e9.6ne + e9.7yath + e9.8em , data=imd)
summary(fit.effect9)








#Figure 3.14 - interaction graph

d <- read.csv("http://raw.github.com/jeremymiles/arc/master/implicit words.csv")


primedMeans <- tapply( d$primed, d$group, mean)
unprimedMeans <- tapply( d$unprimed, d$group, mean)
plot(primedMeans, type="l", ylim=c(70, 100), lty=1, lwd=2, xaxt="n", xlab="Group",ylab="Mean Score")
lines(unprimedMeans,  lty=2, lwd=2)
axis(1, at=1:2, labels=c("Different catext", "Same catext"))
legend("topleft", legend=c("Primed words", "Unprimed words"), lwd=2, lty=c(1, 2), border="white")

d$difference <- d$primed - d$unprimed
summary(lm(difference ~ group, data=d))



primed    <- d[, c("id", "group", "primed" )]
unprimed  <- d[, c("id", "group", "unprimed" )]
primed$type <- "primed"
unprimed$type <- "unprimed"
names(primed)[3]   <- "score"
names(unprimed)[3] <- "score"
dlong <- rbind(primed, unprimed)

dlong$id <- as.factor(dlong$id)

d.aov <- aov(score ~   type*group + Error(id), data = dlong)
summary(d.aov)

fit.reg <- lm(primed ~ group + unprimed, data=d)
summary(fit.reg)
  



hp <- read.csv("http://raw.github.com/jeremymiles/arc/master/housePrices.csv")
hp <- hp[-51, ]

hp$log.listprice <- log(hp$list.price)

summary(lm(log.listprice ~ beds, data=hp))
summary(lm(log.listprice ~ as.factor(beds), data=hp))
plot(tapply(hp$log.listprice, hp$beds, mean), type="l", 
     xlab="Number of bedrooms", ylab="Log(list price)")


beds.cat.fit <- lm(log.listprice ~ beds, data=hp)
beds.cat.fit <- lm(log.listprice ~ factor(beds), data=hp)
summary(beds.cat.fit)

summary(beds.cat.fit)

AIC(beds.cat.fit)
AIC(beds.cat.fit)

cat.preds <- predict(beds.cat.fit, newdata=data.frame(beds=1:6), interval="confidence")
cat.preds <- predict(beds.cat.fit, newdata=data.frame(beds=1:6), interval="confidence")

cat.preds <- as.data.frame(cat.preds)
cat.preds <- as.data.frame(cat.preds)

cat.preds
cat.preds
#Put two plots side by side
par(mfrow=c(1, 2))
#draw line of best fit for continuous
plot(x=1:6, y=cat.preds$fit, ylim=c(4.6, 6.6),
     type="l", lwd=2, ylab="Log(predicted price)", xlab="Number of bedrooms", main="catinuous Model")
#draw upper and lower confidence limits
lines(1:6,cat.preds$lwr)
lines(1:6, cat.preds$upr)

#draw line of best fit for categorical
plot(x=1:6, y=cat.preds$fit, ylim=c(4.6, 6.6),
     type="l", lwd=2, ylab="Log(predicted price)", xlab="Number of bedrooms", main="catinuous Model", yaxt=FALSE)
#draw upper and lower confidence limits
lines(1:6,cat.preds$lwr)
lines(1:6, cat.preds$upr)






#Put them on the same graph, in different colors
par(mfrow=c(1, 1))
plot(both.preds$fit.cat, ylim=c(4.6, 6.6), type="l", lwd=2, ylab="Log(predicted price)", xlab="Number of bedrooms", main="catinuous Model")
lines(1:6,both.preds$lwr.cat)
lines(1:6, both.preds$upr.cat)
lines(both.preds$fit.cat, lwd=2, col="red")
lines(rep(1:6), both.preds$lwr.cat, col="red")
lines(rep(1:6), both.preds$upr.cat, col="red")

