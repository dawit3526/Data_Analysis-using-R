#############################################################
# Data Analysis
# Computer Lab Session n° 6:
# Principal Component Analysis
#############################################################

###################
# 1. Introduction #
###################

###############################################
# How can we represent high-dimensional data? #
# Loading the planets dataset                 #
###############################################
library(XLConnect)
planets <- readWorksheet(loadWorkbook("~/R/data/planets.xlsx"),sheet=1)
planets
names <- planets[,1]

rownames(planets) <- planets[,1]
planets[,1] <- NULL
planets

###########################
# Pairwise Representation #
###########################
pairs(planets)

######################
# Log Transformation #
######################
planets.log <- log(planets)
colnames(planets.log) <- paste("log(",colnames(planets),")", sep="")
pairs(planets.log)

###################################
# 2D Representation and Plot Size #
###################################
win.graph(800, 600, 10)

# Create the plot without plotting the pointsc
plot(x = planets.log[,1], y= planets.log[,3], cex=(round(planets.log[,2])),
     xlab = colnames(planets.log[1]), ylab = colnames(planets.log[3]))

# Write text to the position of each point.
text(x=planets.log[,1], y=planets.log[,3], labels=names, cex = 0.8, col = "blue" )

######################
# 3D Representationt #
######################
library(rgl)
plot3d(x=planets.log[,1],
       y=planets.log[,2],
       z=planets.log[,3],
       xlab=colnames(planets.log[1]),
       ylab=colnames(planets.log[2]),
       zlab=colnames(planets.log[3]))

text3d(x=planets.log[,1],
       y=planets.log[,2],
       z=planets.log[,3],
       text=names,
       cex=0.8,
       col="blue")

################################################
# Representation with Dimensionality Reduction #
################################################
# planets.pca <- princomp(planets.log, cor = TRUE, scores = TRUE)
planets.pca <- princomp(planets.log)
planets.pca
win.graph(800, 600, 10)
biplot(planets.pca)

###################################################
# 2. PCA on a 2-Dimensional Dataset, Step by Step #
###################################################

x <- c(1, 3, 3, 5, 5, 6, 8, 9)
y <- c(2, 3, 5, 4, 6, 5, 7, 8)

mydataset <- cbind(x,y)
mydataset

plot(mydataset[,"x"],mydataset[,"y"])
plot(mydataset[,"x"],mydataset[,"y"], asp=1)

n <- nrow(mydataset)
d <- ncol(mydataset)

xbar = mean(x)
ybar = mean(y)

newX = x-xbar
newY = y-ybar

# Covariance without correction
covariance <- 0
for (i in 1:n)
  {
  covariance <- covariance + newX[i] * newY[i]
  }
covariance <- covariance / (n)  
covariance

# Covariance with correction
covariance <- 0
for (i in 1:n)
  {
  covariance <- covariance + newX[i] * newY[i]
  }
covariance <- covariance / (n - 1)  
covariance

# Simply by using the R function "cov"
mynewdataset <- data.frame(newX, newY)
covariance <- cov(mynewdataset)
covariance

# Eigenvectors and eigenvalues of the covariance matrix
E <- eigen(covariance)
E$vectors
E$values
Component1 <- (E$vectors[1]/E$vectors[2])*newX
Component1
Component2 <- (E$vectors[3]/E$vectors[4])*newY
Component2

# Graphs (on the same separated window)
win.graph(800,600,10)
plot(mynewdataset$newX, mynewdataset$newY, 
     col="black", xlim=c(-6,6),ylim=c(-6,6), asp=1)
par(new=TRUE)
plot(mynewdataset$newX, Component1, xlab="", ylab="",
     col="blue", type="l", xlim=c(-6,6),ylim=c(-6,6), asp=1)
par(new=TRUE)
plot(mynewdataset$newY, Component2, xlab="", ylab="",
     col="red", type= "l", xlim=c(-6,6),ylim=c(-6,6), asp=1)

# Biplot of the PCA
mynewdataset.pca <- princomp(mynewdataset)
win.graph(800,600,10)
biplot(mynewdataset.pca)

#########################################
# 3. PCA on an High-Dimensional Dataset #
#########################################
# 3.1 Dataset
rm(list=ls()) # for clearing all the objects from the workspace
library(XLConnect)
students <- readWorksheet(loadWorkbook("~/R/data/students.xlsx"),sheet=1)
students
names <- students[,1]
rownames(students) <- students[,1]
students[,1] <- NULL
students

# 3.2 Descriptive Statistics
# Summary
summary(students)
# Number of Observations
n <- nrow(students)
print(n)
# Object Properties
attributes(students)

# 3.3 Pairwise Representation #
pairs(students)
cor(students)

win.graph(800,600,10)
plot(students$NbDays, students$Age, 
     xlim=c(7700,12000), ylim=c(20,32))
par(new=TRUE)
x <- seq(7700, 12000, length=2000)
y <- floor(x/365.25)
plot(x, y, col="red", type="l", xlab="", ylab="", 
     xlim=c(7700,12000), ylim=c(20,32))

# 3.4 PCA with the princomp function
pca.students <- princomp(students, cor = TRUE, scores = TRUE)
pca.students
summary(pca.students)
attributes(pca.students)

# 3.5 Eigenvalues 
# Variances associated with the axes (i.e., eigenvalues)
eigenvalues <- pca.students$sdev^2
eigenvalues

# Scree Plot
plot(1:5,eigenvalues,type="b",ylab="Eigenvalues",xlab="Components",main="Scree Plot")

# Confidence interval of the eigenvalues at 95%
# Hint: pnorm(1.96, mean=0, sd=1) - pnorm(-1.96, mean=0, sd=1) = 0.95
val.low  <- eigenvalues * exp(-1.96 * sqrt(2.0/(n-1)))
val.high <- eigenvalues * exp(+1.96 * sqrt(2.0/(n-1)))

# Table
table <- cbind(val.low,eigenvalues,val.high)
colnames(table) <- c("Lower Bound","Eigenvalues","Higher Bound")
print(table,digits=3)

# 3.6 Graphs
# Coordinates of the Variables on the Factorial Axes
# Variables-Axes Correlation
c1 <- pca.students$loadings[,1]*pca.students$sdev[1]
c2 <- pca.students$loadings[,2]*pca.students$sdev[2]

# Correlation
correlation <- cbind(c1,c2)
print(correlation,digits=2)

# Square value of the correlation
print(correlation^2,digits=2)

# Cumulative square of the correlation
print(t(apply(correlation^2,1,cumsum)),digits=2)

# Correlation Circle
win.graph(800, 600, 10)
plot(c1,c2,xlim=c(-1,+1),ylim=c(-1,+1),type="n")
abline(h=0,v=0)
text(c1,c2,labels=colnames(students),cex=1, col="red")
symbols(0,0,circles=1,inches=FALSE,add=TRUE)

# Representation of the students in the first factorial plane
plot(pca.students$scores[,1],
     pca.students$scores[,2],
     type="n",xlab="Comp.1",ylab="Comp.2")
abline(h=0,v=0)
text(pca.students$scores[,1],
     pca.students$scores[,2],
     labels=rownames(students),cex=0.75)

# Simultaneous Representation of the Variables and Observations
biplot(pca.students,cex=0.75)

# Representation of the students in the first factorial cube
library(rgl)
plot3d(x=pca.students$scores[,1],
       y=pca.students$scores[,2],
       z=pca.students$scores[,3],
       xlab="Comp.1",
       ylab="Comp.2",
       zlab="Comp.3")
text3d(x=pca.students$scores[,1],
       y=pca.students$scores[,2],
       z=pca.students$scores[,3],
       xlab="",
       ylab="",
       zlab="",
       text=names,
       cex=0.8,
       col="blue")

