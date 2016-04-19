# load data

# cluster by trip lengths


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


trip.lengths <- data.frame(trips$wegkm.k)

clusters <- agnes(trip.lengths)
