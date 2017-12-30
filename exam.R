#summary of the fast food
#Part 1
mydata = fast_food
summary(mydata)
hist(mydata$Time)
mu=mean(mydata$Time)
s=sd(mydata$Time)
x=seq(-4,4,length=200)
par(new = TRUE)
y=dnorm(x,mean=mu,sd=s)
plot(x,y,type="l",lwd=1,col="yellow")
pnorm(13.5,mean=mu,sd=s,lower.tail = TRUE)-pnorm(18,mean=mu,sd=s,lower.tail = TRUE)
#count 
nb_scores <- 0
for (i in 1:350)
{
  if (mydata[i,1]>=13.5 && mydata[i,1]<=18) nb_scores <- nb_scores + 1
}
for(i in 1:350)
{
  if(mydata[i,1]>15)
    newdata1[i,1] = mydata[i,1]
  else
    newdat2[i,1] = mydata[i,1]
}
mydata[,1]


#Part 2

data = light_van_car
summary(data)
pairs(data)
#----yes outilers
data.log <- log(data)
colnames(data.log) <- paste("log(",colnames(data),")", sep="")
pairs(data.log)
#-------
#yes I see new outlier

#plotint the cost as function of kilometers

plot(data.log$`log(Nb.km)`,data.log$`log(Cost)`)
#linear regration
fit <- lm(Nb.km ~ Cost, data)
fit
abline(fit)
plot(fit)
#multipl linear regration
fit <- lm(Nb.km ~ Cost + data.log$`log(Nb.km)` + Years + data.log$`log(Years)`, data=data)

summary(fit) # show results
fit
##Estimate price of the car

##Part 3

#number one
data3 = bacteria
row.names(bacteria) = bacteria[,1]
bacteria[,1] = NULL
pairs(data3)
# 6 or 7 clusters 
#number 2
plot(data3)
#Here it i think I can have 5 or 6 almost similar clusters
#number 3
cor(data3)
#number 4
summary(data3)
#number 5

for (k in 2:10)
{
  for(i in 1:5){
  kmeans.result <- kmeans (data3, k)
  print(kmeans.result$tot.withinss)
 temp1= kmeans.result$cluster/k-1
 temp2 = kmeans.result$betweenss/10-k
 C_H_index = temp1/temp2
 print(C_H_index)
  i=i+1
}
}

max_val = max(C_H_index)
max_val
kmeans(data3,max_val)
