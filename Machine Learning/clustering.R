# This mini-project is based on the K-Means exercise from 'R in Action'
# Go here for the original blog post and solutions
# http://www.r-bloggers.com/k-means-clustering-from-r-in-action/

# Exercise 0: Install these packages if you don't have them already

# install.packages(c("cluster", "rattle","NbClust"))
library(cluster)
library(rattle)
library(NbClust)

# Now load the data and look at the first few rows
data(wine, package="rattle")
head(wine)

# Exercise 1: Remove the first column from the data and scale
# it using the scale() function

# Removing the first column from the data
Type <- wine$Type
wine$Type = NULL

# Scaling the data
wine <- data.frame(scale(x = wine))

# Now we'd like to cluster the data using K-Means. 
# How do we decide how many clusters to use if you don't know that already?
# We'll try two methods.

# Method 1: A plot of the total within-groups sums of squares against the 
# number of clusters in a K-means solution can be helpful. A bend in the 
# graph can suggest the appropriate number of clusters. 

wssplot <- function(data, nc=15, seed=1234){
	              wss <- (nrow(data)-1)*sum(apply(data,2,var))
               	      for (i in 2:nc){
		        set.seed(seed)
	                wss[i] <- sum(kmeans(data, centers=i)$withinss)}
	                
		      plot(1:nc, wss, type="b", xlab="Number of Clusters",
	                        ylab="Within groups sum of squares")
	   }

wssplot(wine)

# Exercise 2:
#   * How many clusters does this method suggest?
# Ans: Looking at the bend in the plot, 3 clusters seem to be a good idea.
#   * Why does this method work? What's the intuition behind it?
# Ans: The intuition is to reduce the sum of squares within the groups while keeping the the number of clusters reasonable.   
#   * Look at the code for wssplot() and figure out how it works
# Ans: It plots the number of clusters and the sum of squares within each group, to compare a good balance by observing the bend.
#      The bend suggests that the sum of squares has reduced significantly and is not going to reduce a lot further while the number of 
#      clusters is not too high.


# Method 2: Use the NbClust library, which runs many experiments
# and gives a distribution of potential number of clusters.

set.seed(1234)
nc <- NbClust(wine, min.nc=2, max.nc=15, method="kmeans")
barplot(table(nc$Best.n[1,]),
	          xlab="Numer of Clusters", ylab="Number of Criteria",
		            main="Number of Clusters Chosen by 26 Criteria")

# Exercise 3: How many clusters does this method suggest?
# Ans: The method suggest 3 clusters.

# Exercise 4: Once you've picked the number of clusters, run k-means 
# using this number of clusters. Output the result of calling kmeans()
# into a variable fit.km

fit.km <- kmeans(x = wine, centers = 3, iter.max = 1000)

# Now we want to evaluate how well this clustering does.

# Exercise 5: using the table() function, show how the clusters in fit.km$clusters
# compares to the actual wine types in wine$Type. Would you consider this a good
# clustering?
table(fit.km$cluster, Type)
# * The clustering seems to match the actual wine types very closely.

# Exercise 6:
# * Visualize these clusters using  function clusplot() from the cluster library
# * Would you consider this a good clustering?

library(cluster)
clusplot(x = wine, clus = fit.km$cluster)
# * Looking at the cluster plot, the clustering seems to be appropriate. 
#   The data points seem to be well distributed among the clusters, and the clusters don't seem to overlap.