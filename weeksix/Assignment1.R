# Problem 1 - Hierarchical Clustering
# Read the data set into R
dailyKos <- read.csv("dailykos.csv")
# Compute the distances (using method="euclidean"), and use hclust to build the model 
distance <- dist(dailyKos, method="euclidean")
clusterIntensity <- hclust(distance, method="ward.D")
# Running the dist function will probably take you a while. Why?
# Because we have lots of observations and variables
# Plot the dendrogram of your hierarchical clustering model. Just looking at the dendrogram, 
# which of the following seem like good choices for the number of clusters?
# Pick the one that give you the longest distance between the children's node and the parent's node
# In this problem, we are trying to cluster news articles or blog posts into groups. What are
# good choices for the number of clusters? 
# We do not want the category to be so general, so we pick the number of cluster based on that metric
# Let's pick 7 clusters. Use the cutree function to split your data into 7 clusters, create 
# 7 new datasets, each containing the observations from one of the clusters. 
clusters <- cutree(clusterIntensity, k=7)
cluster1 <- subset(dailyKos, clusters==1)
cluster2 <- subset(dailyKos, clusters==2)
cluster3 <- subset(dailyKos, clusters==3)
cluster4 <- subset(dailyKos, clusters==4)
cluster5 <- subset(dailyKos, clusters==5)
cluster6 <- subset(dailyKos, clusters==6)
cluster7 <- subset(dailyKos, clusters==7)
table(clusters)
# How many observations are in cluster 3?
374
# Which cluster has the most observations?
1
# Which cluster has the fewest observations?
4
subset
tail(sort(colMeans(cluster1)))
# What is the most frequent word in this cluster, in terms of average value?
# Answer based on the above function
# Now repeat the command given in the previous problem for each of the other clusters
tail(sort(colMeans(cluster2)))
tail(sort(colMeans(cluster3)))
tail(sort(colMeans(cluster4)))
tail(sort(colMeans(cluster5)))
tail(sort(colMeans(cluster6)))
tail(sort(colMeans(cluster7)))
# Which words best describe cluster 2?
# Choose the 4 most frequent words
# Which cluster could best be described as the cluster related to the Iraq war?
# Pick the one with words related to war
# Which cluster best corresponds to the democratic party?
# Pick the one with words that are related to the democratic party