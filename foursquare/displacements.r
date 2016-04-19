# get to know the data

# for each user, find their displacements


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

#######################

# remove NAs
clean.data <- subset(my.data, latitude >= -180 & longitude >= -180 & latitude <= 180 & longitude <= 180)

unique.users <- unique(clean.data$user_id)

num.checkins.per.user <- data.frame()

user.displacement.checkins <- data.frame()

# find the number of trips per user,
# and all trips for users having more than one
for (user.id in unique.users) {

  # get this user's trips
  user.checkins <- subset(clean.data, user_id == user.id)
  
  # skip iteration if more than 1 trip
  num.checkins <- dim(user.checkins)[1]
#  num.checkins.per.user <- rbind(num.checkins.per.user, data.frame(user.id, num.checkins))
  
  if (num.checkins == 1)
    next

  
  user.displacement.checkins <- rbind(user.displacement.checkins, user.checkins)
}


# get displacements for users having more than one checkin (a movement)
displaced.users <- unique(user.displacement.checkins$user_id)

all.displacements <- data.frame()

# find the number of trips per user, and 
for (user.id in displaced.users) {

  user.checkins <- subset(user.displacement.checkins, user_id == user.id)
  
  # sort by time (if needed)

  # find the displacement distances using vector operations
  lat.diffs <- diff(user.checkins$latitude)
  lon.diffs <- diff(user.checkins$longitude)

#  from <- user.checkins$venue
#  to <- user.checkins$venue
  
  # time stamps
  
  # time differences
  delta.t <- diff(as.POSIXlt(user.checkins$created_at))
  
  delta.r <- sqrt(lat.diffs * lat.diffs + lon.diffs * lon.diffs)

  displacements <- data.frame(user.id, delta.r, delta.t, user.checkins)
  
  all.displacements <- rbind(all.displacements, data.frame(user_id, displacements))
}

# mean squared displacement



rm(list = ls())
