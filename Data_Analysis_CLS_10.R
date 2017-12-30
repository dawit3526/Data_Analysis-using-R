#############################################################
# Data Analysis
# Computer Lab Session n° 10:
# Clustering 2
#############################################################

###########################
# Clustering Iris Dataset #
###########################

data("iris") 
mydata <- iris[1:4] 
class <- as.matrix(iris[5])

kmeans.result <- kmeans(mydata,3)
kmeans.result
summary(kmeans.result)
w <- (kmeans.result$tot.withinss/kmeans.result$totss)*100
b <- (kmeans.result$betweenss/kmeans.result$totss)*100
print(paste("WSS=",round(w,2),"%"))
print(paste("BSS=",round(b,2),"%"))
print(table(class, kmeans.result$cluster))

for (i in 1:10)
  {
  print(paste("Loop n°",i))
  kmeans.result <- kmeans(mydata,3)
  kmeans.result
  w <- (kmeans.result$tot.withinss/kmeans.result$totss)*100
  b <- (kmeans.result$betweenss/kmeans.result$totss)*100
  print(paste("WSS=",round(w,2),"%"))
  print(paste("BSS=",round(b,2),"%"))
  print(table(class, kmeans.result$cluster))
  print("********")
  }

summary(kmeans.result)


hc <- hclust(dist(mydata), "ave")

hc

summary(hc)

win.graph(800, 600, 10)
plot(hc, hang = -1, labels=class)

idx <- sample(1:dim(iris)[1], 40)
irisSample <- iris[idx,]
irisSample$Species <- NULL
hc <- hclust(dist(irisSample), method="ave")
win.graph(800, 600, 10)
plot(hc, hang = -1, labels=iris$Species[idx])


# cut tree into 3 clusters
rect.hclust(hc, k=3)
groups <- cutree(hc, k=3)

##############################
# Clustering Ruspini Dataset #
##############################

rm(list=ls())
mydata <- read.table("~/R/data/ruspini.txt", quote="\"")
n = nrow(mydata)

trace_BH <- 0 
trace_CH <- 0
x <- 0

for (k in 2:10)
  {
  x <- c(x,k)
  print(paste("Test with k=",k))
  kmeans.result <- kmeans(mydata,k)
  kmeans.result
  w <- (kmeans.result$tot.withinss/kmeans.result$totss)*100
  b <- (kmeans.result$betweenss/kmeans.result$totss)*100
  print(paste("WSS=",round(w,2),"%"))
  print(paste("BSS=",round(b,2),"%"))
  BH_index <- w / k
  print(paste("BH index=",round(BH_index,2)))
  trace_BH <- c(trace_BH,BH_index)
  CH_index <- (b / (k - 1)) / (w / (n - k))
  print(paste("CH index=",round(CH_index,2)))
  trace_CH <- c(trace_CH,CH_index)
  print("********")
  }
x <- x[-1]
trace_CH <- trace_CH[-1]
trace_BH <- trace_BH[-1]
plot(x,trace_BH,type="l",xlab="number of clusters",
     ylab="Ball and Hall clustering quality index")
plot(x,trace_CH,type="l",
     xlab="number of clusters",
     ylab="Calinski and Harabasz clustering quality index")
require(ggplot2)
qplot(factor(x),trace_CH, 
      geom="bar", stat="identity",
      xlab="number of clusters",
      ylab="Calinski and Harabasz clustering quality index")

