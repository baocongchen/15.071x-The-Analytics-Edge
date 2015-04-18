# Using the table function in R, please answer the following questions about the dataset "movies".
# How many movies are classified as comedies?
sum(movies$Comedy)
# How many movies are classified as westerns?
sum(movies$Western)
# How many movies are classified as romance AND drama?
table(movies$Drama, movies$Romance)
# Run the cutree function again to create the cluster groups, but this time pick k = 2 clusters. It turns out that 
# the algorithm groups all of the movies that only belong to one specific genre in one cluster (cluster 2), and 
# puts all of the other movies in the other cluster (cluster 1). What is the genre that all of the movies in 
# cluster 2 belong to?
for (i in 2:genresNum) {
    print(genres[i])
    print(tapply(movies[,genres[i]], clusterGroups, sum))
}
