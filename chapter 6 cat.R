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




###Effect coding 
imd$e9.1london  <- imd$region == "London"
imd$e9.2se      <- imd$region == "South East"   
imd$e9..3sw	    <- imd$region == "South West"
imd$e9.4wm	    <- imd$region == "West Midlands"
imd$e9.5nw	    <- imd$region == "North West"
imd$e9.6ne	    <- imd$region == "North East"
imd$e9.7yath	  <- imd$region == "Yorkshire and The Humber"
imd$e9.8em	    <- imd$region == "East Midlands"  
imd$e9.9ee      <- imd$region == "East of England"

imd$e9.1london  <- ifelse(imd$region == "East of England", -1, imd$e9.11london)
imd$e9.2se      <- ifelse(imd$region == "East of England", -1, imd$e9.12se)
imd$e9.3sw      <- ifelse(imd$region == "East of England", -1, imd$e9.13sw)
imd$e9.4wm      <- ifelse(imd$region == "East of England", -1, imd$e9.14wm)
imd$e9.5nw      <- ifelse(imd$region == "East of England", -1, imd$e9.15nw)
imd$e9.6ne      <- ifelse(imd$region == "East of England", -1, imd$e9.16ne)
imd$e9.7yath    <- ifelse(imd$region == "East of England", -1, imd$e9.17yath)
imd$e9.8em      <- ifelse(imd$region == "East of England", -1, imd$e9.18em)
imd$e9.9ee      <- ifelse(imd$region == "East of England", -1, imd$e9.19ee)

fit.effect9 <- lm(imd.score ~  e9.1london + e9.2se + e9.3sw + 
                e9.4wm + e9.5nw +e6ne + e7yath + e8em , data=imd)
summary(fit.effect1)







###Effect coding 
imd$e9.1london  <- imd$region == "London"
imd$e9.2se      <- imd$region == "South East"   
imd$e9..3sw      <- imd$region == "South West"
imd$e9.4wm	    <- imd$region == "West Midlands"
imd$e9.5nw	    <- imd$region == "North West"
imd$e9.6ne	    <- imd$region == "North East"
imd$e9.7yath	  <- imd$region == "Yorkshire and The Humber"
imd$e9.8em	    <- imd$region == "East Midlands"  
imd$e9.9ee      <- imd$region == "East of England"

imd$e9.1london  <- ifelse(imd$region == "East of England", -1, imd$e9.11london)
imd$e9.2se  <- ifelse(imd$region == "East of England", -1, imd$e9.12se)
imd$e9.3sw  <- ifelse(imd$region == "East of England", -1, imd$e9.13sw)
imd$e9.4wm  <- ifelse(imd$region == "East of England", -1, imd$e9.14wm)
imd$e9.5nw  <- ifelse(imd$region == "East of England", -1, imd$e9.15nw)
imd$e9.6ne  <- ifelse(imd$region == "East of England", -1, imd$e9.16ne)
imd$e9.7yath  <- ifelse(imd$region == "East of England", -1, imd$e9.17yath)
imd$e9.8em  <- ifelse(imd$region == "East of England", -1, imd$e9.18em)
imd$e9.9ee  <- ifelse(imd$region == "East of England", -1, imd$e9.19ee)

fit.effect9 <- lm(imd.score ~  e9.1london + e9.2se + e9.3sw + 
                    e9.4wm + e9.5nw +e6ne + e7yath + e8em , data=imd)
summary(fit.effect1)




#Figure 3.14 - interaction graph

d <- read.csv("http://raw.github.com/jeremymiles/arc/master/implicit words.csv")
primedMeans <- tapply( d$primed, d$group, mean)
unprimedMeans <- tapply( d$unprimed, d$group, mean)
plot(primedMeans, type="l", ylim=c(70, 100), lty=1, lwd=2, xaxt="n", xlab="Group",ylab="Mean Score")
lines(unprimedMeans,  lty=2, lwd=2)
axis(1, at=1:2, labels=c("Different Context", "Same Context"))
legend("topleft", legend=c("Primed words", "Unprimed words"), lwd=2, lty=c(1, 2), border="white")