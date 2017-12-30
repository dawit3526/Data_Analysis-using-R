#############################################################
# Data Analysis
# Computer Lab Session n° 7:
# Linear Regression (1/2)
#############################################################


##################################################
# Simple Linear Regression on Eucalyptus Dataset #
##################################################

euc <- read.table("~/R/data/eucalyptus.txt",header=T)
summary(euc)
plot(ht~circ,data=euc)
regeuc <- lm(ht~circ,data=euc)
summary(regeuc)
regeuc

abline(regeuc,col="red")


# Just for the test with the coefficients found by lm
# par(new=TRUE)
# x <- 26:77
# y <- x * 0.257 + 9.0374
# plot(x,y, col="blue", ylim=c(11.25,27.75), type="l")

attributes(regeuc)
sum_euc <- summary(regeuc)
sum_euc
plot(ht~circ,data=euc)

circ=seq(min(euc[,"circ"]),max(euc[,"circ"]),length=100)
grid<-data.frame(circ)
CIpred<-predict(regeuc,new=grid,interval="pred",level=0.95)
matlines(grid$circ,cbind(CIpred),lty=c(1,2,2),col=2)

# New Model
multreg<-lm(ht~circ+I(sqrt(circ)),data=euc)
summary(multreg)

plot(ht~circ,data=euc)
circ=seq(min(euc[,"circ"]),max(euc[,"circ"]),length=100)
grid2<-data.frame(circ)
CIpred2<-predict(multreg,new=grid2,interval="pred",level=0.95)
matlines(grid2$circ,cbind(CIpred2),lty=c(1,2,2),col=2)

win.graph(800,600,10)
plot(regeuc)
plot(multreg)


###########################################################
# Simple and Multiple Linear Regressions on Ozone Dataset #
###########################################################

ozone <- read.table("~/R/data/ozone.txt",header=T)
summary(ozone)

win.graph(800,600,10)
pairs(ozone[1:11])
cor(ozone[1:11])

plot(maxO3~T12,data=ozone)
regozone <- lm(maxO3~T12,data=ozone)
summary(regozone)
regozone

abline(regozone,col="red")

y <- as.vector(ozone$maxO3)
x <- as.matrix(ozone[,2:11])

regmultozone <- lm (y~x)

regmultozone
summary(regmultozone)

plot(regozone)
plot(regmultozone)

