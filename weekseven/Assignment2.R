# Problem 1 - Summarizing the Data
edges <- read.csv("edges.csv")
users <- read.csv("users.csv")
# How many Facebook users are there in our dataset?
str(users)
# In our dataset, what is the average number of friends per user? 
# We have 146*2 connections (friends) and 59 nodes (users) and this is the clue
# Out of all the students who listed a school, what was the most common locale?
table(users$locale)
# Is it possible that either school A or B is an all-girls or all-boys school?
table(users$school,users$gender)
##################################

# Problem 2 - Creating a Network
install.packages("igraph")
library(igraph)
# Based on ?graph.data.frame, which of the following commands will create a graph g 
# describing our social network, with the attributes of each user correctly loaded?
?graph.data.frame
# Use the correct command from Problem 2.1 to load the graph g.
g <- graph.data.frame(edges, directed=F, users)
plot(g, vertex.size=5, vertex.label=NA)
# How many connected components with at least 2 nodes are there in the graph?
# Answer based on the graph
# How many users are there with no friends in the network?
# Answer based on the graph
# How many users are friends with 10 or more other Facebook users in this network?
allEdges <- data.frame(c(edges$V1,edges$V2))
table(allEdges)
# or you can use the function table(degree(g)) or table(degree(g) >= 10)
# To visually draw attention to these nodes, we will change the size of the vertices so 
# the vertices with high degrees are larger
V(g)$size = degree(g)/2+2
plot(g, vertex.label=NA)
# What is the largest size we assigned to any node in our graph?
max(V(g)$size)
# What is the smallest size we assigned to any node in our graph?
min(V(g)$size)
####################################

# Problem 3 - Coloring Vertices
# Update the colors by setting the color to black for all vertices, than setting it to red for the 
# vertices with gender A and setting it to gray for the vertices with gender B:
V(g)$color = "black"
V(g)$color[V(g)$gender == "A"] = "red"
V(g)$color[V(g)$gender == "B"] = "gray"
# Plot the resulting graph. What is the gender of the users with the highest degree in the graph?
plot(g, vertex.label=NA)
# Now, color the vertices based on the school that each user in our network attended.
V(g)$color[V(g)$school == ""] = "white"
V(g)$color[V(g)$school == "A"] = "red"
V(g)$color[V(g)$school == "AB"] = "green"
# Are the two users who attended both schools A and B Facebook friends with each other?
# What best describes the users with highest degree?
plot(g, vertex.label=NA)
# Now, color the vertices based on the locale of the user.
V(g)$color[V(g)$locale == ""] = "white"
V(g)$color[V(g)$locale == "A"] = "red"
V(g)$color[V(g)$locale == "B"] = "green"
# The large connected component is most associated with which locale?
plot(g, vertex.label=NA)
# The 4-user connected component is most associated with which locale?
#############################################

# Problem 4 - Other Plotting Options
# Which igraph plotting function would enable us to plot our graph in 3-D?
?igraph.plotting
# What parameter to the plot() function would we use to change the edge width when plotting g?
?igraph.plotting