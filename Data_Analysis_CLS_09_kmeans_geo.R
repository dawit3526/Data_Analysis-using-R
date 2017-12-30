#############################################################
# Data Analysis
# Computer Lab Session n° 9:
# Clustering -- k-means (with geographical distance)
#############################################################

# install.packages("Imap")
# install.packages("googleVis")
# install.packages("xlsx")
# install.packages("rJava")
library(Imap)
library(googleVis)
library(xlsx)
library(XLConnect)

mydata <- readWorksheet(loadWorkbook("~/R/data/students_geo_2015.xlsx"),
                         sheet=1)
rownames(mydata) <- mydata[,1]
student_name <- mydata[,1]
mydata[,1] <- NULL


##############
# Attributes #
##############
# n_c : number of columns (the attributes)
# n_r or dim: number of rows (the individuals)
n_c <- ncol(mydata)
n_r <- nrow(mydata)
dim <- n_r

summary(mydata)
####################################
# Here: no Z-score transformation! #
####################################

####################################
# Choice of the number of clusters #
####################################
# Reading the number "k" (of clusters) chosen by the user
k <- readline("Number of clusters? ")
print(paste("Clustering in",k,"clusters"))
k <- as.integer(k)   
# We are reading a string but it is better to convert k into an integer

####################################
# Random choice of the first means #
####################################
# For this choice, we are using the sample function (without remplacement)
random_points <- sample(1:dim, k, replace=F)
# Assignment of values to the k means
means <- matrix(nrow=k, ncol=n_c)
for (i in 1:k)
  {
  for (j in 1:n_c)
    {
    means[i,j] <- mydata[random_points[i],j]
    }
  }

# Declaration and definition of variables:
# - clusters: cluster number attributed to a point
# - prev_clusters: previous clusters (= "clusters" at the previous step)
# - A: first point (with its "n_c" coordinates)
# - B: second point (with its "n_c" coordinates)
clusters <- matrix(nrow=n_r, ncol=1, 0)
prev_clusters <- matrix(nrow=n_r, ncol=1, 0)
A  <- matrix(nrow=n_c, ncol=1, 0)
B  <- matrix(nrow=n_c, ncol=1, 0)

###############################################
# Distance function (here: geodesic distance) #
###############################################
d <- function(X, Y) 
{
  distance <- gdist(X[2,1],X[1,1],Y[2,1],Y[1,1])
  return(distance)
}



nb_loops <- 0
convergence_in_progress <- TRUE
mydata$latlong <- paste(mydata[,1], mydata[,2], sep=":")

######################################################
# Repetition until convergence has not yet been made #
######################################################

while (convergence_in_progress)
 {
 for(i in 1:dim) # For each individual,
  {
  distances <- matrix(nrow=k, ncol=1, Inf)
  for(v in 1:n_c) # for each attribute,
    {
    A[v,1] <- mydata[i,v] # build the point A...
    }
  for (j in 1:k) # For each mean,
    {
    for (v in 1:n_c) # for each attribute,
      {
      B[v,1] <- means[j,v] # build the point B...
      }
    distances[j,1] <- d(A,B) # Compute the distance between A and B
    }
  dist_min <- Inf # Initialization of the minimum distance to infinity
  cluster <- 0    # and the cluster number to zero.
  for (j in 1:k)  # For each mean point,
    {
    if (distances[j,1] < dist_min) # If the current distance is smaller than the minimum distance
      {
      cluster <- j    # faire le changement de numéro de cluster...
      dist_min <- distances[j,1]       # ...et de distance minimale
      }
    }
    clusters[i,1] <- cluster # Add the number of cluster to "clusters"
  }
 nb_loops <- nb_loops + 1
 # While the convergence is still in progress,
 # we test whether the current clusters and those obtained before are the same
 the_same <- TRUE
 for (i in 1:dim) # For each individual,
   {      # as they do not differ from one element, they are considered as identical 
   the_same <- the_same && (clusters[i,1] == prev_clusters[i,1])
   }
 # convergence must still be done if the identity is not found
 convergence_in_progress <- !(the_same) 
 prev_clusters <- clusters
 #########
 # Graph #
 #########
 x <- 1:dim
 # Graph with the different colors for each cluster
 y <- 1:k        # presentation of the "k" means (in blue squares)
 print(paste("Results obtained in ", nb_loops, " loops."))
 mydata$cluster <- clusters
 # Graph with the different colors for each cluster
 RepGeo <- gvisGeoChart(mydata, locationvar = "latlong", colorvar="cluster",
                        options=list(hovervar="cluster",
                                     sizeAxis="{minValue: 7,  maxSize: 7}",
                                     colorAxis="{colors:['purple', 'red', 'orange', 'grey', 'green', 'blue', 'navy']}"))
 plot(RepGeo)
 readline("Press [Enter] key to continue...")
 # Computation of the new means
 if (convergence_in_progress)
   {
   means <- matrix(nrow=k, ncol=n_c, 0)
   nb_indiv <- matrix(nrow=k, ncol=1, 0)
   for(i in 1:dim) # for each individual,
     {             # we add its number to the clusters
     nb_indiv[clusters[i,1],1] <- nb_indiv[clusters[i,1],1] + 1
     for (j in 1:n_c) # For each variable,
       {              # we add the coordinate of the points to the mean...
       means[clusters[i,1],j] <- means[clusters[i,1],j] + mydata[i,j]
       }
     }
   for(i in 1:k)  # for each mean,
     {
     for (j in 1:n_c) # for each variable,
       {              # we compute the mean
       means[i,j] <- means[i,j] / nb_indiv[i,1]
       }
     }
   }
 }

data_geo <- list("latlong" = paste(mydata[,1], mydata[,2], sep=":"),
                 "cluster" = paste(clusters, student_name))
RepGeo <- gvisMap(data_geo, locationvar = "latlong", tipvar = "cluster",
                  options=list(
                    showTip = TRUE,
                    lineColor = "blue"))
plot(RepGeo)

mydata$cluster <- clusters
for (i in 1:k)
  {
  print(paste("*** Cluster number ",i,":"))
  for (j in 1:dim)
    {
    if (mydata$cluster[j] == i)
      {
      print(paste0("[", i,"] ",rownames(mydata)[j]))
      }
    }
  }

