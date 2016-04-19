
# find principle components using all categories


##############

# load data

# convert categorical variables to numeric

# run PCA on all columns of data

# plot biplot (how to do 3D biplot?)

##############


rm(list = ls())

#plotting
library("ggplot2")
library(cluster)

# plfit code
#source("plfit.r")


##############


data.dir = "/home/username/Dropbox/Active_Projects/Mobility_Article/Data/"
figures.dir = "./Figures/"

load(paste(data.dir, "trips.Rdata", sep=""))

##############



my.trips <- data.frame(as.numeric(trips$sgtypd), as.numeric(trips$bland), as.numeric(trips$polgk), as.numeric(trips$rtypd7), as.numeric(trips$hwzweck), as.numeric(trips$hvm), as.numeric(trips$stichtag), as.numeric(trips$st.stdg), trips$wegkm.k, trips$wegmin.k)

pca <- prcomp(my.trips, scale = T)
biplot(pca)
