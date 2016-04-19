# get to know the data

# load data, plot some statistics


rm(list = ls())

#######################

library(ggplot2)
library(igraph)

#######################

data.dir <- "/home/username/Dropbox/Active_Projects/Mobility_Article/Data/umn_foursquare_datasets/"
#data.dir <- "umn_foursquare_datasets/"
figures.dir <- "figures/"
data.names <- c("checkins","ratings","socialgraph","users","venues")

#######################

index <- 3
#filename <- paste(data.dir,data.names[index],".RData", sep="")
#filename
#load(filename)
my.data <- read.csv(paste(data.dir,data.names[index],".head.dat", sep=""), sep="|")
my.data <- my.data[-1,]

#######################

# create graph from edges

# since undirected, remove even edges
#odd.indices <- seq(1,dim(my.data)[1], by = 2)
#edge.data <- subset(my.data, my.data[1] == odd.indices)

# convert to numeric
# edge.list <- c(my.data$first_user_id, my.data$second.user.id)

edge.list <- c(as.numeric(as.character(my.data$first_user_id)), as.numeric(as.character(my.data$second_user_id)))

social.network <- graph(edge.list, directed=FALSE)


#######################  BASIC MEASURES

degree.dist <- degree.distribution(social.network)
cdf.degree.dist <- degree.distribution(social.network, cumulative = TRUE)
plot(degree.dist, log = "xy")
plot(cdf.degree.dist, log = "xy")



between.ness <- betweenness(social.network)
plot(between.ness, log = "xy")



clustering <- cluster.distribution(social.network)
plot(clustering)



assort <- assortativity.degree(social.network)
assort


pagerank <- page.rank(social.network)$vector
plot(pagerank, log = "xy")

## ## First three eigenvalues of the adjacency matrix of a graph
## ## We need the ’Matrix’ package for this
## if (require(Matrix)) {
## g <- erdos.renyi.game(1000, 5/1000)
## M <- get.adjacency(g, sparse=TRUE)
## f2 <- function(x, extra=NULL) { cat("."); as.vector(M %*% x) }
## baev <- arpack(f2, options=list(n=vcount(g), nev=3, ncv=8, sym=TRUE,
## which="LM", maxiter=200))
## }

adj.matrix <- get.adjacency(social.network, sparse = TRUE)
plot(adj.matrix)

#eigen.decomp <- eigen(adj.matrix)
#plot(eigen.decomp)


plot(social.network)

#qplot(x = first_user_id, y = second_user_id, data = my.data)
#ggsave(file=paste(figures.dir, "network_", data.names[index], ".pdf", sep=""))

#######################


rm(list = ls())
