# load data


# calculate arrivals per day

##########################################################################

rm(list = ls())

load("./tripdata.Rdata")

trip.modes <- as.character(unique(trips$mode))

#remove last element (no response for 'mode')
trip.modes <- trip.modes[-length(trip.modes)]


################# ALL MODES: plot arrivals at daily intervals ########################

dev.new(width=14, height=7)

breaks.interval <- "days"

#trip.mode <- trip.modes[1]

 #subset data by mode
 # mode.trips <- subset(trips, mode == trip.mode)

mode.trips <- trips

  # change timestamp to date.time
mode.trips$timestamp.arrival <- as.POSIXlt(mode.trips$timestamp.arrival)

  # sort by timestamp
  #mode.trips.ordered <- mode.trips[order(as.POSIXlt(mode.trips$timestamp.arrival)),]
df.time.unclassed <- data.frame(mode.trips, unclass(as.POSIXlt(mode.trips$timestamp.arrival)))

# use only weekdays
df.time.unclassed <- subset(df.time.unclassed, wday > 0 & wday < 6)

  # bin by day
  my.table <- table(cut(df.time.unclassed$timestamp.arrival, breaks = breaks.interval))

  # bin by day
#  my.table <- table(cut(mode.trips$timestamp.arrival, breaks = breaks.interval))

  print(length(my.table))

#  plot(my.table, type = "s", col = "black", xlab="Date", ylab="Number of Arrivals", main=paste("Arrivals (all modes) ", breaks.interval))

acf(my.table, main = "all modes")

################# EACH MODE: plot arrivals at daily intervals ########################

index = 1
colors <- c(2:(length(trip.modes)+1))

dev.new(width=14, height=7)

breaks.interval <- "days"

trip.mode <- trip.modes[1]

 #subset data by mode
  mode.trips <- subset(trips, mode == trip.mode)
 
  # change timestamp to date.time
mode.trips$timestamp.arrival <- as.POSIXlt(mode.trips$timestamp.arrival)

  # sort by timestamp
  #mode.trips.ordered <- mode.trips[order(as.POSIXlt(mode.trips$timestamp.arrival)),]
df.time.unclassed <- data.frame(mode.trips, unclass(as.POSIXlt(mode.trips$timestamp.arrival)))

# use only weekdays
df.time.unclassed <- subset(df.time.unclassed, wday > 0 & wday < 6)

  # bin by day
  my.table <- table(cut(df.time.unclassed$timestamp.arrival, breaks = breaks.interval))

  # bin by day
#  my.table <- table(cut(mode.trips$timestamp.arrival, breaks = breaks.interval))

  print(length(my.table))

#  plot(my.table, type = "s", col = "black", xlab="Date", ylab="Number of Arrivals", main=paste("Arrivals (all modes) ", breaks.interval))

#acf(my.table, main = "all modes")

  acf(my.table, main = trip.mode)

  index <- index + 1

#################################################################################3




trip.modes.abbrev <- trip.modes[-1]

for (trip.mode in trip.modes.abbrev) {

#trip.mode <- trip.modes[5]

#subset data by mode
  mode.trips <- subset(trips, mode == trip.mode)
 
  # change timestamp to date.time
mode.trips$timestamp.arrival <- as.POSIXlt(mode.trips$timestamp.arrival)

  # sort by timestamp
  #mode.trips.ordered <- mode.trips[order(as.POSIXlt(mode.trips$timestamp.arrival)),]
df.time.unclassed <- data.frame(mode.trips, unclass(as.POSIXlt(mode.trips$timestamp.arrival)))

# use only weekdays
df.time.unclassed <- subset(df.time.unclassed, wday > 0 & wday < 6)

  # bin by day
  my.table <- table(cut(df.time.unclassed$timestamp.arrival, breaks = breaks.interval))

  # bin by day
#  my.table <- table(cut(mode.trips$timestamp.arrival, breaks = breaks.interval))

  print(length(my.table))

#  plot(my.table, type = "s", col = "black", xlab="Date", ylab="Number of Arrivals", main=paste("Arrivals (all modes) ", breaks.interval))

#acf(my.table, main = "all modes")

  acf(my.table, main = trip.mode)

  index <- index + 1

}





rm(list = ls())
