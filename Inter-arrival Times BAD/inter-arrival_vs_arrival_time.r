# load data

# sort by arrival times

# calculate inter-arrival times

# plot histogram

#####################################################
rm(list = ls())

load("./tripdata.Rdata")

trips.ordered <- trips[order(as.POSIXlt(trips$timestamp.arrival)),]

# INTER-ARRIVAL TIMES
d1 <- diff(as.POSIXlt(trips.ordered$timestamp.arrival))

color.index <- 1

# plot inter-arrival times vs. arrival times
inter.arrival.time <- c(0, d1)

arrival.time <- as.POSIXlt(trips.ordered$timestamp.arrival)
plot(arrival.time, inter.arrival.time,  main = "All Modes : Inter-Arrival Times vs Arrival Times", col=color.index)

################### FOR EACH MODE ################                                       

trip.modes <- unique(trips$mode)

# remove last
trip.modes <- trip.modes[-length(trip.modes)]

color.index <- 2
for (trip.mode in trip.modes) {

  print(trip.mode)
  
  # get subset
  mode.trips <- subset(trips.ordered, mode == trip.mode)
  print(dim(mode.trips))

  # INTER-ARRIVAL TIMES
  d1 <- diff(as.POSIXlt(trips.ordered$timestamp.arrival))

  # plot inter-arrival times vs. arrival times
  inter.arrival.time <- c(0, d1)

  arrival.time <- as.POSIXlt(trips.ordered$timestamp.arrival)
  plot(arrival.time, inter.arrival.time,  main = paste(trip.mode, ": Inter-Arrival Times vs Arrival Times"), col=color.index)

  color.index <- color.index + 1
}

legend("topright", legend = trip.modes, pch = 1, col=1:length(trip.modes) + 1)
