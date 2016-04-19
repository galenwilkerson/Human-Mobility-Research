
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


# CCDF:
# function that takes in data frame, interval, max value
# returns data frame of x.values and ccdf
CCDF <- function(in.data.frame, x.interval = 1, max.value) {

  df.names <- names(in.data.frame)
  
  # get unique groups
  #groups <- as.vector(unique(in.data.frame[,2]))
  
  # create data subsets based on groups
  #data.subset1 <- subset(in.data.frame, eval(df.names[2]) == groups[1])
  
  # grab data from first column
  data.values <- in.data.frame[,1]

  # group names
#  group.names <- in.data.frame[,2]
  
  # set up breaks, x-values, log xvalues
  breaks.vector <- seq(0, max.value, by=x.interval)
  x.values <- breaks.vector[-1]
  log10.xvals <- log10(x.values)

  # the CDF function
  cdf <- ecdf(data.values)

  # the CDF values
  cdf <- cdf(x.values)

  # the CCDF values
  ccdf <- 1 - cdf
#  log10.ccdf <- log10(ccdf)
  
  #qplot(log10.xvals, log10.ccdf, main = main.title, xlab = xlab, ylab = ylab, legend.position = "auto")
  
  #legend("bottomleft", paste(seq(0,23,3), ":00 \t", num.trips.at.time),  col=seq(0,23,3)+1, pch=19, title="time of day \t num. trips")
  
  #legend(location, groups,  colors, marker.type, legend.title)

  return(data.frame(x.values, ccdf))
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

# make sure sorted in order of event
events <- events[with(events, order(timestamp.event)), ]

## # get categories, remove 'bad' categories ##################################
## all.categories <- as.character(unique(trips$hvm))
## bad.category <- all.categories[length(all.categories)]
## good.categories <- all.categories[-length(all.categories)]

## events.good.categories <- subset(events.within.hour.range, hvm != bad.category)

pdf(file="output.pdf")

# plot inter-event times, trip durations, trip lengths

inter.event.times <- as.numeric(diff(events$timestamp.event))
inter.event.times.mins <- inter.event.times/60

h.inter <- hist(inter.event.times.mins, breaks = 100000, main="histogram durations")

x.values <- h.inter$breaks[-1]
log10.x.values <- log10(x.values)
log10.count <- log10(h.inter$counts)

plot(log10.x.values, log10.count, main="log-log durations")

cdf <- ecdf(inter.event.times)
CDF <- cdf(x.values)
CCDF1 <- 1 - CDF
log10.ccdf <- log10(CCDF1)

plot(log10.x.values, log10.ccdf, main="CCDF inter-event times")


########################## trip durations

durations <- events$wegmin.k

dur.hist <- hist(durations, breaks = 1000, main="histogram trip durations")

x.values <- dur.hist$breaks[-1]
log10.x.values <- log10(x.values)
log10.count <- log10(dur.hist$counts)
plot(log10.x.values, log10.count, main="log-log PDF trip durations")

cdf <- ecdf(durations)
CDF <- cdf(x.values)
CCDF1 <- 1 - CDF
log10.ccdf <- log10(CCDF1)
plot(log10.x.values, log10.ccdf, main="CCDF trip durations")


########################## trip lengths 

lengths <- events$wegkm.k


length.hist <- hist(lengths, breaks = 1000, main="histogram trip lengths")

x.values <- length.hist$breaks[-1]
log10.x.values <- log10(x.values)
log10.count <- log10(length.hist$counts)
plot(log10.x.values, log10.count, main="log-log PDF trip lengths")

cdf <- ecdf(lengths)
CDF <- cdf(x.values)
CCDF1 <- 1 - CDF
log10.ccdf <- log10(CCDF1)
plot(log10.x.values, log10.ccdf, main="CCDF trip lengths")


dev.off()



## plot automatically, use ggplot2 ##########################################

inter.event.df <- data.frame(inter.event.times)
inter.event.ccdf <- CCDF(inter.event.df, x.interval = 1, max(inter.event.times))
log10.inter.event.ccdf <- log10(inter.event.ccdf)
qplot(log10.inter.event.ccdf$x.values, log10.inter.event.ccdf$ccdf) +
  ggtitle("CCDF inter-event times")

ggsave(file = "inter-event.pdf")


durations.df <- data.frame(durations)
durations.ccdf <- CCDF(durations.df, x.interval = 1, max(events$wegmin.k))
log10.durations.ccdf <- log10(durations.ccdf)
qplot(log10.durations.ccdf$x.values, log10.durations.ccdf$ccdf) +
  ggtitle("CCDF durations")
ggsave(file = "durations.pdf")


lengths.df <- data.frame(lengths)
lengths.ccdf <- CCDF(lengths.df, x.interval = 1, max(events$wegkm.k))
log10.lengths.ccdf <- log10(lengths.ccdf)
qplot(log10.lengths.ccdf$x.values, log10.lengths.ccdf$ccdf) +
  ggtitle("CCDF lengths")
ggsave(file = "lengths.pdf")
