# load data

# sort by arrival times

# calculate inter-arrival times

# plot histogram

#####################################################
rm(list = ls())

load("./tripdata.Rdata")

trips.ordered <- trips[order(as.POSIXlt(trips$timestamp.arrival)),]

# INTER-ARRIVAL TIMES
inter.arrival.times <- as.numeric(diff(as.POSIXlt(trips.ordered$timestamp.arrival)))

################## LIN LIN HISTOGRAM #################



h <- hist(inter.arrival.times, breaks = 1000, plot = F)

################## LOG LOG CCDF ######################

inter.arrival.time.intervals <- h$breaks[-1]
cdf <- ecdf(inter.arrival.times)
CDF <- cdf(inter.arrival.time.intervals)
CCDF <- 1 - CDF

#log10.cdf <- log10(CDF)
log10.ccdf <- log10(CCDF)
log10.inter.arrival.times <- log10(inter.arrival.time.intervals)

color.index <- 1
plot(log10.inter.arrival.times, log10.ccdf, type = "s", main = "log-log CCDF Inter-Arrival Times", col = color.index)




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
inter.arrival.times <- as.numeric(diff(as.POSIXlt(mode.trips$timestamp.arrival)))

################## LIN LIN HISTOGRAM #################



h <- hist(inter.arrival.times, breaks = 1000, plot = F)

################## LOG LOG CCDF ######################

inter.arrival.time.intervals <- h$breaks[-1]
cdf <- ecdf(inter.arrival.times)
CDF <- cdf(inter.arrival.time.intervals)
CCDF <- 1 - CDF

log10.ccdf <- log10(CCDF)
log10.inter.arrival.times <- log10(inter.arrival.time.intervals)

lines(log10.inter.arrival.times, log10.ccdf, type = "s", main = "log-log CCDF Inter-Arrival Times", col = color.index)

color.index <- color.index + 1
 
}


legend("bottomleft", legend = c("all modes", as.character(trip.modes)), pch = 1, col=c(1,2,3,4,5,6))


#################### FIT DATA ##########################################3


trips.ordered <- trips[order(as.POSIXlt(trips$timestamp.arrival)),]

# INTER-ARRIVAL TIMES
inter.arrival.times <- as.numeric(diff(as.POSIXlt(trips.ordered$timestamp.arrival)))

################## LIN LIN HISTOGRAM #################



h <- hist(inter.arrival.times, breaks = 1000, plot = F)

################## LOG LOG CCDF ######################

x <- h$breaks[-1]

y <- h$counts

temp <- data.frame(y, x)


# plot data
plot(temp$x, temp$y, log="xy", xlab = "Arrival Time", ylab = "Frequency", main="LOG-LOG Hist Arrival Times")

# fit non-linear model
mod <- nls(y ~ lambda * exp(lambda * x), data = temp, start = list(lambda = 0))
summary(mod)

# add fitted curve
y.predict <- predict(mod, list(x = temp$x))
lines(temp$x, predict(mod, y.predict), log="xy")

## #########  PLOT LOG-LOG ###########

## log.x <- log10(x)

## log.y <- log10(y)

## log.y.predict <- log10(y.predict)

## plot(log.x, log.y)

## lines(log.x, log.y.predict)
