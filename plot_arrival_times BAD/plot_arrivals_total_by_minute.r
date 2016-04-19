# load data


# calculate arrivals per day

##########################################################################

rm(list = ls())

load("./tripdata.Rdata")

trip.modes <- as.character(unique(trips$mode))

#remove last element (no response for 'mode')
trip.modes <- trip.modes[-length(trip.modes)]

####################################  

breaks.interval <- "minutes"

#subset data by date
start.date <- as.POSIXlt("2008-04-01 00:00:00") 
end.date <- as.POSIXlt("2008-12-31 00:00:00")
trips <- subset(trips, timestamp.arrival >= start.date & timestamp.arrival <= end.date)

################ FOR ALL MODES ################################


dev.new(width=14, height=7)

# change timestamp to date.time
trips$timestamp.arrival <- as.POSIXlt(trips$timestamp.arrival)

df.time.unclassed <- data.frame(unclass(as.POSIXlt(trips$timestamp.arrival)))

h1 <- hist(df.time.unclassed$min, breaks = 59, plot=F)
plot(h1, border = "black", xlab="Minute", ylab="Number of Arrivals", main=paste("Arrivals:", breaks.interval, "averaged over entire time period"))


############################ FOR EACH MODE ###################################

index = 1
colors <- c(2:(length(trip.modes)+1))

dev.new(width=14, height=7)


trip.mode <- trip.modes[index]

 #subset data by mode

mode.trips <- subset(trips, mode == trip.mode)
  
  # change timestamp to date.time
  mode.trips$timestamp.arrival <- as.POSIXlt(mode.trips$timestamp.arrival)

 df.time.unclassed <- data.frame(unclass(as.POSIXlt(mode.trips$timestamp.arrival)))


h1 <- hist(df.time.unclassed$min, breaks = 59, plot=F)
plot(h1, border = colors[index], xlab="Minute", ylab="Number of Arrivals", main=paste("Arrivals:", breaks.interval, "averaged over entire time period"))



for (index in 2:length(trip.modes)) {

  #subset data by mode and day
  
  trip.mode <- trip.modes[index]

  mode.trips <- subset(trips, mode == trip.mode)

#  mode.trips <- subset(trips, mode == trip.mode & timestamp.arrival <= end.time.interval & timestamp.arrival >= start.time.interval)
  
  # change timestamp to date.time
   mode.trips$timestamp.arrival <- as.POSIXlt(mode.trips$timestamp.arrival)
 
 df.time.unclassed <- data.frame(unclass(as.POSIXlt(mode.trips$timestamp.arrival)))


h1 <- hist(df.time.unclassed$min, breaks = 59, plot=F)
lines(h1$count, type = "s", col = colors[index], xlab="Minute", ylab="Number of Arrivals",  main=paste("Arrivals:", breaks.interval))

}


legend("topright",  legend = trip.modes, pch = 1, col = colors)




rm(list = ls())
