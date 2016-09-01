
library(psych)
setwd("C:/Users/Jeremy/Dropbox/Applying Reg and Corr/arcdata/arc")

#Chapter 5a (or whatever it is): categorical data

hp <- read.csv("http://raw.github.com/jeremymiles/arc/master/housePrices.csv")
hp <- hp[-51, ]

hp$log.listprice <- log(hp$list.price)

means <- tapply( hp$log.listprice, hp$parking.type, mean)
tapply( hp$log.listprice, hp$parking.type, sd)

hp$garage <- hp$parking.type == "Garage"
plot(tapply( hp$log.listprice, hp$garage, mean), type="l",xaxt="n", ylab="log(list price)" , xlab="Garage",
     ylim=c(5, 6))
axis(1, at=c(1, 2), labels=c(0, 1))

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

fit2 <- lm(imd.score ~ d2se + d3sw + d4wm + d5nw + d6ne + d7yath + d8em + d9ee, data=imd)
summary(fit2)


data.frame(describeBy(imd$imd.score, imd$region, mat=TRUE))

