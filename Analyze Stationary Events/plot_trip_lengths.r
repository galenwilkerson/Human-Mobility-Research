
# load data

# subset by stationary time window
# subset by categorical variable(s)

# remove error/special values

# calculate inter-event times, durations, trip lengths
# plot CCDFs



########################################################

rm(list = ls())
library(ggplot2)
library(scales)


########################################################


# subset events by time period, return data frame within time period (inclusive)
# times are POSIXlt
subset.events.time.period <- function(events, time.column.name, start.time, end.time) {

  
  subset.events <- subset(events, events[,time.column.name] >= start.time & events[,time.column.name] <= end.time)
   
  
  return(subset.events)
}


# subset events by (inclusive) weekday(s) (0-6 starting on Sunday)
# return data frame on this day, also containing unclassed time columns
# times are POSIXlt
subset.events.day.of.week <- function(events, time.column.name, my.start.wday, my.end.wday) {
  
  new.df <- get.unclassed.data.frame.with.timescale(events, time.column.name)
  
  subset.events <- subset(new.df, wday >= my.start.wday, wday <= my.end.wday)
  
  return(subset.events)
}


# subset events by category, return list of subset data frames
subset.events.category <- function(events, subset.category, categories) {

  subsets <- list()

  i <- 1

  for (cat in categories) {
    
    # get rows by category
    category.events <- subset(events, events[,subset.category] == cat)
    subsets[[i]] <- category.events
    i <- i + 1
  }

   return(subsets)
}


# get frequency of events by time scale
# return data frame with timestamp and frequency columns
get.frequency.by.timescale <- function(events, time.stamp.column.name, timescale) {

  event.table <- table(cut(events[,time.stamp.column.name], breaks = timescale))
  event.frequency <- data.frame(event.table)
  colnames(event.frequency) <- c("timestamp", "frequency")
  
  return(event.frequency)
}



# return data frame with original events and 'unclassed' timestamp information
get.unclassed.data.frame.with.timescale <- function(events, time.stamp.column.name) {

  # break apart timestamp into components
  df.unclassed <- data.frame(events, unclass(as.POSIXlt(events[,time.stamp.column.name])))
 # h <- hist(df.unclassed[,timescale.name], plot = F)
      
  return(df.unclassed)
}

  
############################################################################



data.dir = "/home/username/Dropbox/Active_Projects/Mobility_Article/Data/"
figures.dir = "./Figures/"

load(paste(data.dir, "trips.Rdata", sep=""))

# get events within time range ########################################

## - from June 1 to Oct. 1, 2008
## - on a weekday
## - between 9am and 2pm
start.time <- as.POSIXlt("2008-06-01 00:00:00 CEST")
end.time <- as.POSIXlt("2008-10-01 00:00:00 CEST")
events.within.period <- subset.events.time.period(trips, time.column.name = "timestamp.event", start.time = start.time, end.time = end.time)

events.within.period$timestamp.event <- as.POSIXlt(events.within.period$timestamp.event)
temp.events <- unclass(events.within.period$timestamp.event)
temp.df <- data.frame(events.within.period, temp.events)
events.on.day <- subset(temp.df, wday >= 1 & wday <= 5)

events.in.range <- subset(events.on.day, hour >= 9 & hour <= 14)

events <- events.in.range

######################### FIND log-log CCDF #############################

# make sure sorted in order of event
events <- events[order(events$timestamp.event),]


# use hist for finding breaks
h.inter <- hist(events$wegkm.k, breaks = 100000, plot = F)

# find xvalues, log x values, log ccdf
my.breaks <-  h.inter$breaks
trip_length <- my.breaks[-1]
log10.trip_length <- log10(trip_length)

# find CDF, use for CCDF, log CCDF
cdf <- ecdf(events$wegkm.k)
CDF <- cdf(trip_length)
CCDF1 <- 1 - CDF
log10.ccdf <- log10(CCDF1)

category <- "all_modes"

# keep track of results in data frame by category for plotting later
df <- data.frame(log10.trip_length, log10.ccdf, category)
 
############################ PLOT BY CATEGORY ############################

## # get categories, remove 'bad' categories #############################
all.categories <- as.character(unique(trips$hvm))
bad.category <- all.categories[length(all.categories)]
good.categories <- all.categories[-length(all.categories)]
events.good.categories <- subset(events, hvm != bad.category)


# subset events by category
events.in.cat <- list()
index <- 1
for (category in good.categories) {
  events.in.cat[[index]] <- subset(events.good.categories, hvm == category)
  index <- index + 1
}

# for each subset, get the events, find the trip_length
# add results to a data frame by category

index <- 1
for (events in events.in.cat) {
  category <- good.categories[index]

  cdf <- ecdf(events$wegkm.k)
  CDF <- cdf(trip_length)
  CCDF <- 1 - CDF
  log10.ccdf <- log10(CCDF)

  df <- rbind(df, data.frame(log10.trip_length, log10.ccdf, category))
    
  index <- index + 1
}


p <- ggplot(data=df, aes(x = log10.trip_length, y = log10.ccdf, color=category)) +
  geom_line() +
  xlim(-1,3) +
  ggtitle("CCDF Trip_Length (km)")

 ggsave(paste("CCDF_trip_length.pdf", sep=""))
