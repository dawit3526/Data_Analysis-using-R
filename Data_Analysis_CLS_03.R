#############################################################
# Data Analysis
# Computer Lab Session n° 3:
# Probability, Random Variables and Probability Distributions
#############################################################

# Probabilities
# Relative Frequency Approach to Probability
# Heads or tails?

require(stats)

# 10 random values

for (i in 1:10)
{
  x <- round(runif(1))
  print(x)
}

# Relative frequency of heads
# as a function of the number of tosses
# (with 1000 tosses)

proba <- as.vector(1:1000)

sum <- 0
for (i in 1:1000)
{
  x <- round(runif(1))
  sum <- sum + x
  proba[i] <- sum / i
}

plot(1:1000,proba[1:1000],"l", xlim=c(1,1000), ylim=c(0,1))
segments(0,0.5,1000,0.5, col="red")

# With 10,000 tosses...

proba <- as.vector(1:10000)

sum <- 0
for (i in 1:10000)
{
  x <- round(runif(1))
  sum <- sum + x
  proba[i] <- sum / i
}

plot(1:10000,proba[1:10000],"l", xlim=c(1,10000), ylim=c(0,1))
segments(0,0.5,10000,0.5, col="red")

# Random Variables and Probability Distributions
# Probability Distribution for Discrete Random Variables

# With one die

max <- 6
nb_events <- max
proba <- as.vector(1:max)
for (i in 1:max)
{
  proba[i] <- 1/nb_events
}
require(ggplot2)

qplot(factor(1:max),proba[1:max], 
      geom="bar", stat="identity",
      xlab="Value obtained by one die",
      ylab="Probability")


# With two dice

max_dice <- 6
score_max <- max_dice * 2
nb_events <- max_dice ^ 2
proba <-  rep.int(0,score_max)
proba <- as.vector(proba)

for (i in 1:max_dice)
{
  for (j in 1:max_dice)
    {
    proba[i+j] <- proba[i+j] + 1/nb_events
    }
}

qplot(factor(1:score_max),proba[1:score_max], geom="bar", stat="identity",
      xlab="Value obtained with the sum of two dice",
      ylab="Probability")



# Expected value

expec <- 0

for (i in 1:score_max)
{
    expec <- expec + proba[i] * i
}

print(expec)

# Variance

variance <- 0

for (i in 1:score_max)
{
  variance <- variance + (expec - i)^2 * proba[i]
}

print(variance)
st_deviation <- sqrt(variance)
print(st_deviation)

# Second way to compute the variance (variance bis)

expec_square <- 0

for (i in 1:score_max)
{
  expec_square <- expec_square + proba[i] * i^2
}

variance_bis <- expec_square - (expec^2)
print(variance_bis)

# Unbiased estimator of the variance for i.i.d. observations

u_variance <- variance * (nb_events / (nb_events - 1))



# Another way

sum <-  rep.int(0,nb_events)
sum <- as.vector(sum)
sum
event <- 0
for (i in 1:max_dice)
{
  for (j in 1:max_dice)
  {
    event <- event + 1
    sum[event] <- i+j
  }
}
sum
hist(sum, breaks = c(1:12), col = "blue1")
mean(sum)
var(sum)
sd(sum)
var(sum)


#########################

revision_grade <- read.csv("~/R/data/revision_grade.csv", sep=";")

table(revision_grade)

cor(revision_grade)

#########################

data("iris")

pairs(iris[-5], bg=iris$Species, pch=21)

cor(iris[-5])


# install.packages("PerformanceAnalytics", lib="C:/Program Files/R/R-2.15.2/library")
library(PerformanceAnalytics)
chart.Correlation(iris[-5], bg=iris$Species, pch=21)

# This chart contains a lot of information: 
# On the diagonal are the univariate distributions, plotted as histograms and kernel density plots.
# On the right of the diagonal are the pair-wise correlations, with red stars signifying significance levels.
# As the correlations get bigger the font size of the coefficient gets bigger.
# On the left side of the diagonal is the scatter-plot matrix, with loess smoothers in red to help illustrate the underlying relationship.
# This plot combines a large amount of information into one command and one easy to follow plot.



#########################
# Binomial distribution #
#########################

colors<-c("black", "blue", "red", "green")

n_max<-20
n<-5
p <- 1/2
fd <- function(x) {dbinom(x,n,p)}
plot(cbind(0:n, sapply(0:n,fd)),
     xlim=c(0,n_max), ylim=c(0,.40),
     type="p", ylab="", xlab="",
     pch=15, cex=2, col=colors[1], cex.axis=2)

n<-10
fd <- function(x) {dbinom(x,n,p)}
points(cbind(0:n, sapply(0:n,fd)),
       xlim=c(0,n_max), ylim=c(0,.40),
       type="p", ylab="", xlab="",
       pch=16, cex=2, col=colors[2], cex.axis=2)

n<-15
fd <- function(x) {dbinom(x,n,p)}
points(cbind(0:n, sapply(0:n,fd)),
       xlim=c(0,n_max), ylim=c(0,.40),
       type="p", ylab="", xlab="",
       pch=17, cex=2, col=colors[3], cex.axis=2)


n<-20
fd <- function(x) {dbinom(x,n,p)}
points(cbind(0:n, sapply(0:n,fd)),
       xlim=c(0,n_max), ylim=c(0,.40),
       type="p", ylab="", xlab="",
       pch=18, cex=2, col=colors[4], cex.axis=2)

mtext(c(expression(paste(italic(n), "=5 "))), adj=0, at=1, col=colors[1])

mtext(c(expression(paste(italic(n), "=10 "))), adj=0, at=3, col=colors[2])

mtext(c(expression(paste(italic(n), "=15 "))), adj=0, at=5, col=colors[3])

mtext(c(expression(paste(italic(n), "=20 "))), adj=0, at=7, col=colors[4])

title(main=c(expression(paste("Binomial distributions for ", 
                              italic(p), " = 1/2 and ", 
                              italic(n), " = 5, 10, 15 and 20"))),
      sub="Probability mass function")


n_max <- 10
n <- 10
p <- 1/2
fd <- function(x) {dbinom(x,n,p)}
plot(cbind(0:n, sapply(0:n,fd)),
     xlim=c(0,n_max), ylim=c(0,.40),
     type="p", ylab="", xlab="",
     pch=15, cex=2, col=colors[1], cex.axis=2)

p <- 1/3
fd <- function(x) {dbinom(x,n,p)}
points(cbind(0:n, sapply(0:n,fd)),
       xlim=c(0,n_max), ylim=c(0,.40),
       type="p", ylab="", xlab="",
       pch=16, cex=2, col=colors[2], cex.axis=2)

p <- 1/4
fd <- function(x) {dbinom(x,n,p)}
points(cbind(0:n, sapply(0:n,fd)),
       xlim=c(0,n_max), ylim=c(0,.40),
       type="p", ylab="", xlab="",
       pch=17, cex=2, col=colors[3], cex.axis=2)


p <- 1/5
fd <- function(x) {dbinom(x,n,p)}
points(cbind(0:n, sapply(0:n,fd)),
       xlim=c(0,n_max), ylim=c(0,.40),
       type="p", ylab="", xlab="",
       pch=18, cex=2, col=colors[4], cex.axis=2)

mtext(c(expression(paste(italic(p), "=1/2 "))), adj=0, at=1, col=colors[1])

mtext(c(expression(paste(italic(p), "=1/3 "))), adj=0, at=3, col=colors[2])

mtext(c(expression(paste(italic(p), "=1/4 "))), adj=0, at=5, col=colors[3])

mtext(c(expression(paste(italic(p), "=1/5 "))), adj=0, at=7, col=colors[4])

title(main=c(expression(paste("Binomial distributions for ", 
                              italic(n), " = 10 and ", 
                              italic(p), " = 1/2, 1/3, 1/4 and 1/5"))),
      sub="Probability mass function")


######################
# Normal distibution #
######################
x=seq(-4,4,length=200)
y=1/sqrt(2*pi)*exp(-x^2/2)
plot(x,y,type="l",lwd=5,col="blue")

x=seq(-4,4,length=200)
par(new = TRUE)
y=dnorm(x,mean=0,sd=1)
plot(x,y,type="l",lwd=1,col="yellow")



x <- seq(-10,100,.1)
normdensity1 <- dnorm(x,mean=10,sd=5)
normdensity2 <- dnorm(x,mean=40,sd=2.5)
normdensity3 <- dnorm(x,mean=70,sd=10)
plot(x,normdensity1,type="l",col="red", ylim=range(c(normdensity1,normdensity2,normdensity3)))
par(new = TRUE)
plot(x,normdensity2,type="l",col="green",ylim=range(c(normdensity1,normdensity2,normdensity3)), axes = FALSE, xlab = "", ylab = "")
par(new = TRUE)
plot(x,normdensity3,type="l",col="blue", ylim=range(c(normdensity1,normdensity2,normdensity3)), axes = FALSE, xlab = "", ylab = "")



pnorm(1, mean=0, sd=1)

y=dnorm(x)
plot(x,y,type="l", lwd=2, col="blue")
x=seq(-4,1,length=200)
y=dnorm(x)
polygon(c(-4,x,1),c(0,y,0),col="gray")


# With one standard deviation
x=seq(-4,4,length=200)
y=dnorm(x)
plot(x,y,type="l", lwd=2, col="blue")
x=seq(-1,1,length=100)
y=dnorm(x)
polygon(c(-1,x,1),c(0,y,0),col="gray")

pnorm(1,mean=0,sd=1)-pnorm(-1,mean=0,sd=1)
# 68%

# With 2 sd

x=seq(-4,4,length=200)
y=dnorm(x)
plot(x,y,type="l", lwd=2, col="blue")
x=seq(-2,2,length=200)
y=dnorm(x)
polygon(c(-2,x,2),c(0,y,0),col="gray")
pnorm(2,mean=0,sd=1)-pnorm(-2,mean=0,sd=1)
# 95%

# A normal distribution with mean = 3500 grams
# and standard deviation = 600 grams
# is a reasonable model for the probability distribution
# of the continuous variable X : 
# birth weight of a randomly selected full-term baby.

# What proportion of birth weights are between 2900 and 4700 grams?
pnorm(4700,mean=3500,sd=600)-pnorm(2900,mean=3500,sd=600)
# 0.8185946

# What birth weight w is exceeded only in 2.5% of the cases?
qnorm(1 - 2.5/100, mean = 3500, sd = 600)
# 4675.978


z <- qnorm(2.5/100, mean = 3500, sd = 600, lower.tail = FALSE)



x=seq(-4,4,length=200)
y=dnorm(x)
plot(x,y,type="l", lwd=2, col="blue")
x=seq(-2,2,length=200)
y=dnorm(x)
polygon(c(-2,x,2),c(0,y,0),col="gray")


z <- qnorm(2.5/100, mean = 3500, sd = 600, lower.tail = FALSE)

x <- seq(0, 7000, length=200)
y <- dnorm(x, mean= 3500, sd = 600) # same as y <- dnorm((x - 3500) / 600)  
plot(x, y, type="l", lwd=2, col="gray")
z <- qnorm(2.5/100, mean = 3500, sd = 600, lower.tail = FALSE)
x <- seq(z, 7000, length=100)
y <- dnorm(x, mean= 3500, sd = 600)
polygon(c(z, x, 7000), c(0, y, 0), col="navy")
mtext("What birth weight is exceeded only in 2.5% of the cases?", side=1, adj=1, col="navy")

#####


scores <- read.table("~/R/data/scores.txt", quote="\"")

# sapply(scores,mean)

# sapply(scores,sd)

x <- seq(1:25)

plot(x,scores[x,1])

hist(scores[x,1])

mu <- mean(scores[,1])

sigma <- sd(scores[,1])

x2=seq(20,80,length=200)

par(new = TRUE)
y=dnorm(x2,mean=mu,sd=sigma)
plot(x2,y,type="l",lwd=1,col="red", xlab="", ylab="", yaxt="n")

# How many students do you expect to find with a score equal 
# or greater than 55 ?

# With the model (with the normal distribution)
round(25 * (pnorm(55,mean=mu,sd=sigma, lower.tail=FALSE)))
# For 55 => 8

# How many are they in reality?
nb_scores <- 0
for (i in 1:25)
{
  if (scores[i,1]>= 55) nb_scores <- nb_scores + 1
}

print(nb_scores)
# In reality => 8! It's the same!

# How many students do you expect to find with a score equal 
# or greater than 60 ?

# With the model (with the normal distribution)
round(25 * (pnorm(60,mean=mu,sd=sigma, lower.tail=FALSE)))
# For 60 => 4

# How many are they in reality?
nb_scores <- 0
for (i in 1:25)
{
  if (scores[i,1]>= 60) nb_scores <- nb_scores + 1
}

print(nb_scores)
# In reality => 4! It's the same again!

# How many students do you expect to find with a score equal 
# or greater than 65 ?

# With the model (with the normal distribution)
round(25 * (pnorm(65,mean=mu,sd=sigma, lower.tail=FALSE)))
# For 65 => 2

# How many are they in reality?
nb_scores <- 0
for (i in 1:25)
{
  if (scores[i,1]>= 65) nb_scores <- nb_scores + 1
}

print(nb_scores)
# In reality => 2! It's the same again!

# How many students do you expect to find with a score equal 
# or greater than 70 ?

# With the model (with the normal distribution)
round(25 * (pnorm(70,mean=mu,sd=sigma, lower.tail=FALSE)))
# For 70 => 1

# How many are they in reality?
nb_scores <- 0
for (i in 1:25)
{
  if (scores[i,1]>= 70) nb_scores <- nb_scores + 1
}

print(nb_scores)
# In reality => 1! It's the same again!
# Conclusion: the normal distribution is a good model of this data


#####################################
# Reading student data (Excel file) #
#####################################

library(XLConnect)
students <- readWorksheet(loadWorkbook("~/R/data/students_data.xlsx"),sheet=1)

rownames(students) <- students[,1]
students[,1] <- NULL
students

cor(students)
# Significant correlation only
# between the variables Age and NbDays (99.38%)

x <- seq(1:25)
# They are 25 students


# With the Size (Attribute N°1)

plot(x,students[x,1])

hist(students[x,1])

mu <- mean(students[,1])

sigma <- sd(students[,1])

x2=seq(155,190,length=200)

par(new = TRUE)
y=dnorm(x2,mean=mu,sd=sigma)
plot(x2,y,type="l",lwd=1,col="red", xlab="", ylab="", yaxt="n")

# How many students do you expect to find with a size equal 
# or greater than 180?

# With the model (with the normal distribution)
round(25 * (pnorm(180,mean=mu,sd=sigma, lower.tail=FALSE)))
# For 180 => 6

# How many are they in reality?
nb_scores <- 0
for (i in 1:25)
{
  if (students[i,1]>= 180) nb_scores <- nb_scores + 1
}

print(nb_scores)
# in reality: 7 (8 is not so bad)

# With the Age (Attribute N°5)

plot(x,students[x,5])

hist(students[x,5])

mu <- mean(students[,5])

sigma <- sd(students[,5])

x2=seq(20,32,length=200)

par(new = TRUE)
y=dnorm(x2,mean=mu,sd=sigma)
plot(x2,y,type="l",lwd=1,col="red", xlab="", ylab="", yaxt="n")

# How many students do you expect to find with a age less 
# than one standard deviation?

young <- mu - sigma

# With the model (with the normal distribution)
round(25 * (pnorm(young,mean=mu,sd=sigma, lower.tail=TRUE)))
# For 1 standard deviation => 4

# How many are they in reality?
nb_scores <- 0
for (i in 1:25)
{
  if (students[i,5]<= young) nb_scores <- nb_scores + 1
}

print(nb_scores)
# in reality: 0 (very different)

old <- mu + sigma

# With the model (with the normal distribution)
round(25 * (pnorm(old,mean=mu,sd=sigma, lower.tail=FALSE)))
# For 1 standard deviation => 4 (idem for young or old)

# How many are they in reality?
nb_scores <- 0
for (i in 1:25)
{
  if (students[i,5]>= old) nb_scores <- nb_scores + 1
}

print(nb_scores)
# in reality: 3 (not so bad)

older <- mu +  2 * sigma

# With the model (with the normal distribution)
round(25 * (pnorm(older,mean=mu,sd=sigma, lower.tail=FALSE)))
# For 2 standard deviations => 1 

# How many are they in reality?
nb_scores <- 0
for (i in 1:25)
{
  if (students[i,5]>= older) nb_scores <- nb_scores + 1
}

print(nb_scores)
# in reality: 2 (not so bad)

# Export data in LaTeX format
library(Hmisc)
latex(students, file="")            # If you want all the data
latex(describe(students), file="")

ma=matrix(c(1,1,2,2,1,1,1,0,1),nrow = 3) 
solve(ma)
