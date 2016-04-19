# get to know the data

# load data, plot some statistics


rm(list = ls())

#######################

library(ggplot2)

#######################

data.dir <- "/home/username/Dropbox/Active_Projects/Mobility_Article/Data/umn_foursquare_datasets/"
#data.dir <- "umn_foursquare_datasets/"
figures.dir <- "figures/"
data.names <- c("checkins","ratings","socialgraph","users","venues")

#######################
index <- 1
#filename <- paste(data.dir,data.names[index],".RData", sep="")
#filename
#load(filename)

my.data <- read.csv(paste(data.dir,data.names[index],".head.dat", sep=""), sep="|")
my.data <- my.data[-1,]


qplot(id, data = my.data, geom = "bar")
ggsave(file=paste(figures.dir, "id_", data.names[index], ".pdf", sep=""))

qplot(user_id, data = my.data, geom = "bar")
ggsave(file=paste(figures.dir, "user_id_", data.names[index], ".pdf", sep=""))

qplot(venue_id, data = my.data, geom = "bar")
ggsave(file=paste(figures.dir, "venue_id_", data.names[index], ".pdf", sep=""))

qplot(x = longitude, y = latitude, data = my.data)
ggsave(file=paste(figures.dir, "lat_lon_", data.names[index], ".pdf", sep=""))

qplot(created_at, data = my.data, geom = "bar")
ggsave(file=paste(figures.dir, "created_at_", data.names[index], ".pdf", sep=""))

#######################

index <- 2
#filename <- paste(data.dir,data.names[index],".RData", sep="")
#filename
#load(filename)
my.data <- read.csv(paste(data.dir,data.names[index],".head.dat", sep=""), sep="|")
my.data <- my.data[-1,]

qplot(user_id, data = my.data, geom = "bar")
ggsave(file=paste(figures.dir, "user_id_", data.names[index], ".pdf", sep=""))

qplot(venue_id, data = my.data, geom = "bar")
ggsave(file=paste(figures.dir, "venue_id_", data.names[index], ".pdf", sep=""))

qplot(rating, data = my.data, geom = "bar")
ggsave(file=paste(figures.dir, "rating_", data.names[index], ".pdf", sep=""))

#######################

index <- 3
#filename <- paste(data.dir,data.names[index],".RData", sep="")
#filename
#load(filename)
my.data <- read.csv(paste(data.dir,data.names[index],".head.dat", sep=""), sep="|")
my.data <- my.data[-1,]

qplot(first_user_id, data = my.data, geom = "bar")
ggsave(file=paste(figures.dir, "first_user_id_", data.names[index], ".pdf", sep=""))

qplot(second_user_id, data = my.data, geom = "bar")
ggsave(file=paste(figures.dir, "second_user_id_", data.names[index], ".pdf", sep=""))

qplot(x = first_user_id, y = second_user_id, data = my.data)
ggsave(file=paste(figures.dir, "location_", data.names[index], ".pdf", sep=""))

#######################

index <- 4
#filename <- paste(data.dir,data.names[index],".RData", sep="")
#filename
#load(filename)
my.data <- read.csv(paste(data.dir,data.names[index],".head.dat", sep=""), sep="|")
my.data <- my.data[-1,]


qplot(id, data = my.data, geom = "bar")
ggsave(file=paste(figures.dir, "id_", data.names[index], ".pdf", sep=""))

qplot(x = longitude, y= latitude, data = my.data)
ggsave(file=paste(figures.dir, "location_", data.names[index], ".pdf", sep=""))

#######################

index <- 5
#filename <- paste(data.dir,data.names[index],".RData", sep="")
#filename
#load(filename)
my.data <- read.csv(paste(data.dir,data.names[index],".head.dat", sep=""), sep="|")
my.data <- my.data[-1,]

qplot(id, data = my.data, geom = "bar")
ggsave(file=paste(figures.dir, "id_", data.names[index], ".pdf", sep=""))

qplot(x = longitude, y= latitude, data = my.data)
ggsave(file=paste(figures.dir, "location_", data.names[index], ".pdf", sep=""))

#######################


rm(list = ls())
