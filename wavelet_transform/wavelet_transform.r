# load data


# calculate arrivals per day

##########################################################################

rm(list = ls())
library(wavelets)
#library(xts)

load("./decomposed_trips_minutes.Rdata")

trip.modes <- as.character(unique(trips.detailed$mode))

#remove last element (no response for 'mode')
trip.modes <- trip.modes[-length(trip.modes)]



################ FOR ALL MODES ################################

dev.new(width=10, height=10)
#plot(my.table, type = "s")

# take discrete transform of trips by time
time.series <- as.data.frame(my.table)

myts <- as.ts(time.series$Freq)
dwt1 <- dwt(myts)
plot(dwt1, main = "Discrete Wavelet Transform, All Modes")

############################ FOR EACH MODE ###################################

breaks.interval = "mins"

for (index in 1:length(trip.modes)) {


  #subset data by mode and day
  
  trip.mode <- trip.modes[index]

	print(trip.mode)

  mode.trips <- subset(trips.detailed, mode == trip.mode)

  	print(length(mode.trips))

  my.table <- table(cut(mode.trips$timestamp.arrival, breaks = breaks.interval))
  
 time.series <- as.data.frame(my.table)

  myts <- as.numeric(as.ts(time.series$Freq))
  dwt1 <- dwt(myts)
  plot(dwt1, main = paste("Discrete Wavelet Transform, ", trip.mode))
}


rm(list = ls())
