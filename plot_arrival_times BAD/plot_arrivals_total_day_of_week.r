# load data


# calculate arrivals per day

##########################################################################

rm(list = ls())
library(ggplot2)

load("./tripdata.Rdata")

trip.modes <- as.character(unique(trips$mode))

#remove last element (no response for 'mode')
trip.modes <- trip.modes[-length(trip.modes)]

####################################  

#subset data by date
start.date <- as.POSIXlt("2008-04-01 00:00:00") 
end.date <- as.POSIXlt("2008-12-31 00:00:00")
trips <- subset(trips, timestamp.arrival >= start.date & timestamp.arrival <= end.date)


breaks.interval <- "day of week"


dev.new(width=14, height=7)


# change timestamp to date.time
trips$timestamp.arrival <- as.POSIXlt(trips$timestamp.arrival)

df.time.unclassed <- data.frame(unclass(as.POSIXlt(trips$timestamp.arrival)))

#%h1 <- hist(df.time.unclassed$wday, breaks = 6)

#%plot(h1,   border = "black", xlab="Day", ylab="Number of Arrivals",  main=paste("Arrivals:", breaks.interval))


c <- ggplot(df.time.unclassed, aes(factor(wday)))
c + geom_bar()  + opts(title=paste("Arrivals: (all modes)", breaks.interval), axis.text.x=theme_text(angle=45, hjust = 1))


#c <- ggplot(mergedtrips, aes(factor(stichtag)))
#c + geom_bar()  + opts(title = " trip: day of week", axis.text.x=theme_text(angle=45, hjust = 1))


#######################################################

#breaks.interval <- "day of week"




colors <- c(2:(length(trip.modes)+1))

index = 1
dev.new(width=14, height=7)


trip.mode <- trip.modes[index]

#subset data by mode

mode.trips <- subset(trips, mode == trip.mode)

# change timestamp to date.time
mode.trips$timestamp.arrival <- as.POSIXlt(mode.trips$timestamp.arrival)

df.time.unclassed <- data.frame(unclass(as.POSIXlt(mode.trips$timestamp.arrival)))

## h1 <- hist(df.time.unclassed$wday, breaks = 7, plot = F)
## plot(h1,   border = colors[index], xlab="Day", ylab="Number of Arrivals",  main=paste("Arrivals:", breaks.interval))

c <- ggplot(df.time.unclassed, aes(factor(wday)))
c + geom_bar()  + opts(title=paste("Arrivals:", trip.mode, breaks.interval), axis.text.x=theme_text(angle=45, hjust = 1))




#################


index <- 2

  dev.new(width=14, height=7)

  trip.mode <- trip.modes[index]

                                        #subset data by mode

  mode.trips <- subset(trips, mode == trip.mode)

                                        # change timestamp to date.time
  mode.trips$timestamp.arrival <- as.POSIXlt(mode.trips$timestamp.arrival)

  df.time.unclassed <- data.frame(unclass(as.POSIXlt(mode.trips$timestamp.arrival)))

  ## h1 <- hist(df.time.unclassed$wday, breaks = 7, plot = F)
  ## plot(h1,   border = colors[index], xlab="Day", ylab="Number of Arrivals",  main=paste("Arrivals:", breaks.interval))

  c <- ggplot(df.time.unclassed, aes(factor(wday)))
  c + geom_bar()  + opts(title=paste("Arrivals:", trip.mode, breaks.interval), axis.text.x=theme_text(angle=45, hjust = 1))




index <- 3

  dev.new(width=14, height=7)

  trip.mode <- trip.modes[index]

                                        #subset data by mode

  mode.trips <- subset(trips, mode == trip.mode)

                                        # change timestamp to date.time
  mode.trips$timestamp.arrival <- as.POSIXlt(mode.trips$timestamp.arrival)

  df.time.unclassed <- data.frame(unclass(as.POSIXlt(mode.trips$timestamp.arrival)))

  ## h1 <- hist(df.time.unclassed$wday, breaks = 7, plot = F)
  ## plot(h1,   border = colors[index], xlab="Day", ylab="Number of Arrivals",  main=paste("Arrivals:", breaks.interval))

  c <- ggplot(df.time.unclassed, aes(factor(wday)))
  c + geom_bar()  + opts(title=paste("Arrivals:", trip.mode, breaks.interval), axis.text.x=theme_text(angle=45, hjust = 1))



index <- 4

  dev.new(width=14, height=7)

  trip.mode <- trip.modes[index]

                                        #subset data by mode

  mode.trips <- subset(trips, mode == trip.mode)

                                        # change timestamp to date.time
  mode.trips$timestamp.arrival <- as.POSIXlt(mode.trips$timestamp.arrival)

  df.time.unclassed <- data.frame(unclass(as.POSIXlt(mode.trips$timestamp.arrival)))

  ## h1 <- hist(df.time.unclassed$wday, breaks = 7, plot = F)
  ## plot(h1,   border = colors[index], xlab="Day", ylab="Number of Arrivals",  main=paste("Arrivals:", breaks.interval))

  c <- ggplot(df.time.unclassed, aes(factor(wday)))
  c + geom_bar()  + opts(title=paste("Arrivals:", trip.mode, breaks.interval), axis.text.x=theme_text(angle=45, hjust = 1))


index <- 5

  dev.new(width=14, height=7)

  trip.mode <- trip.modes[index]

                                        #subset data by mode

  mode.trips <- subset(trips, mode == trip.mode)

                                        # change timestamp to date.time
  mode.trips$timestamp.arrival <- as.POSIXlt(mode.trips$timestamp.arrival)

  df.time.unclassed <- data.frame(unclass(as.POSIXlt(mode.trips$timestamp.arrival)))

  ## h1 <- hist(df.time.unclassed$wday, breaks = 7, plot = F)
  ## plot(h1,   border = colors[index], xlab="Day", ylab="Number of Arrivals",  main=paste("Arrivals:", breaks.interval))

  c <- ggplot(df.time.unclassed, aes(factor(wday)))
  c + geom_bar()  + opts(title=paste("Arrivals:", trip.mode, breaks.interval), axis.text.x=theme_text(angle=45, hjust = 1))


## for (index in 2:length(trip.modes)) {


##   dev.new(width=14, height=7)

##   trip.mode <- trip.modes[index]

##                                         #subset data by mode

##   mode.trips <- subset(trips, mode == trip.mode)

##                                         # change timestamp to date.time
##   mode.trips$timestamp.arrival <- as.POSIXlt(mode.trips$timestamp.arrival)

##   df.time.unclassed <- data.frame(unclass(as.POSIXlt(mode.trips$timestamp.arrival)))

##   ## h1 <- hist(df.time.unclassed$wday, breaks = 7, plot = F)
##   ## plot(h1,   border = colors[index], xlab="Day", ylab="Number of Arrivals",  main=paste("Arrivals:", breaks.interval))

##   c <- ggplot(df.time.unclassed, aes(factor(wday)))
##   c + geom_bar()  + opts(title=paste("Arrivals:", trip.mode, breaks.interval), axis.text.x=theme_text(angle=45, hjust = 1))

## }


#legend("topright",  legend = trip.modes, pch = 1, col = colors)




rm(list = ls())
