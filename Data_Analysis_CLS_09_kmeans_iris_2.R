#############################################################
# Data Analysis
# Computer Lab Session n° 9:
# Clustering -- k-means (iris with 2 attributes)
#############################################################
data("iris") # We are working with a dataframe
mydata <- iris[3:4] # with only the 3rd and the 4th attributes (the petals)
class <- as.matrix(iris[5])

##############
# Attributes #
##############
# n_c : number of columns (the attributes)
# n_r or dim: number of rows (the individuals)
n_c <- ncol(mydata)
n_r <- nrow(mydata)
dim <- n_r

summary(iris)
##########################
# Z-score Transformation #
##########################
mydata_zs <- mydata

# Before normalization
summary(mydata_zs)

# With a loop (not recommended)
# for (i in 1:n_c)  
#  {
#  mydata_zs[i] <- ((mydata_zs[i] - sapply(mydata_zs[i],mean))
#                    / sapply(mydata_zs[i],sd))
#  }

# Or simply with a normalize function   
normalize <- function(row) { (row - mean(row)) / sd(row) }
mydata_zs <- apply(mydata, 2, normalize)

# After normalization  
summary(mydata_zs)

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
    means[i,j] <- mydata_zs[random_points[i],j]
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

################################################
# Distance function (here: Euclidian distance) #
################################################
d <- function(X, Y) 
  {
  distance <- 0
  for (z in 1:n_c)
    {
    distance <- distance + (X[z,1] - Y[z,1])^2
    }
  return(sqrt(distance))
  }




nb_loops <- 0
convergence_in_progress <- TRUE
######################################################
# Repetition until convergence has not yet been made #
######################################################

win.graph(800, 600, 10)

while (convergence_in_progress)
 {
 for(i in 1:dim) # For each individual,
  {
  distances <- matrix(nrow=k, ncol=1, Inf)
  for(v in 1:n_c) # for each attribute,
    {
    A[v,1] <- mydata_zs[i,v] # build the point A...
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
 plot(mydata_zs[x,1], mydata_zs[x,2], col=clusters[x,1])
 par(new = TRUE) # On the same graph,
 y <- 1:k        # presentation of the "k" means (in blue squares)
 plot(means[y,1], means[y,2], col="blue", 
      xlab="", ylab="", 
      xlim=range(mydata_zs[x,1]), 
      ylim=range(mydata_zs[x,2]), pch=15)
 print(paste("Results obtained in ", nb_loops, " loops."))
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
       means[clusters[i,1],j] <- means[clusters[i,1],j] + mydata_zs[i,j]
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
