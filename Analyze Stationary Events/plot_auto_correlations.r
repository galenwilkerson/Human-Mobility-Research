
# load data

# subset by stationary time window
# subset by categorical variable(s)

# remove error/special values

# find auto-correlation by month, week, days, hour

########################################################

rm(list = ls())
library(ggplot2)
library(scales)
library(wavelets)
library(xts)

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


# divide data into groups by date
# takes in df having a timestamp column
# returns the same df, with a new column "date.group", only to day resolution
#   starting at 0, going until num.groups - 1
# assumes timestamp column is called "timestamp.event"
group.data.by.date <- function(events, start.date, end.date, num.groups) {

    grouped.events <- data.frame()
    
    # get numeric start and stop date
    # divide by number of groups
    # find beginning, end of each group
    # iterate through groups, labeling data frame accordingly,
    # building new data frame

    start.date <- as.POSIXlt(start.date)
    end.date  <- as.POSIXlt(end.date)
    date.diff <- end.date - start.date

    group.size <- date.diff/num.groups

    for (group in 0:(num.groups - 1)) {
      ith.start.date <- trunc(start.date + group * group.size, units = "days")
      ith.end.date <- trunc(start.date + ((group + 1) * group.size) - 1, units = "days")
 
      print(ith.start.date)
      print(ith.end.date)

      events.this.group <- subset(events, timestamp.event >= ith.start.date & timestamp.event <= ith.end.date)

      # add group
      events.this.group$date.group <- group

      # create new data frame
      grouped.events <- rbind(grouped.events, data.frame(events.this.group))
    }

    return(grouped.events)
  }





############################################################################



data.dir = "/home/username/Dropbox/Active_Projects/Mobility_Article/Data/"
figures.dir = "./Figures/"

load(paste(data.dir, "trips.Rdata", sep=""))

# get events within time range ########################################

## - from June 1 to Oct. 1, 2008

start.time <- as.POSIXlt("2008-06-01 00:00:00 CEST")
end.time <- as.POSIXlt("2008-10-01 00:00:00 CEST")
events.within.period <- subset.events.time.period(trips, time.column.name = "timestamp.event", start.time = start.time, end.time = end.time)


events <- events.within.period


## ## - on a weekday
## ## - between 9am and 2pm
## events.within.period$timestamp.event <- as.POSIXlt(events.within.period$timestamp.event)
## temp.events <- unclass(events.within.period$timestamp.event)
## temp.df <- data.frame(events.within.period, temp.events)
## events.on.day <- subset(temp.df, wday >= 1 & wday <= 5)

## events.in.range <- subset(events.on.day, hour >= 9 & hour <= 14)

## events <- events.in.range

############################## AUTOCORRELATION #############################

# make sure sorted in order of event
events <- events[order(events$timestamp.event),]

# bin by month
breaks.interval <- "month"

my.table <- table(cut(events$timestamp.event, breaks = breaks.interval))

print(length(my.table))

#  plot(my.table, type = "s", col = "black", xlab="Date", ylab="Number of Arrivals", main=paste("Arrivals (all modes) ", breaks.interval))

pdf(paste(figures.dir, "acf_", breaks.interval, "all_modes.pdf", sep=""))
acf(my.table, main=breaks.interval)
dev.off()

# bin by week
breaks.interval <- "week"

my.table <- table(cut(events$timestamp.event, breaks = breaks.interval))

print(length(my.table))

#  plot(my.table, type = "s", col = "black", xlab="Date", ylab="Number of Arrivals", main=paste("Arrivals (all modes) ", breaks.interval))

pdf(paste(figures.dir, "acf_", breaks.interval, "all_modes.pdf", sep=""))
acf(my.table, main=breaks.interval)
dev.off()


# bin by day
breaks.interval <- "day"

my.table <- table(cut(events$timestamp.event, breaks = breaks.interval))

print(length(my.table))

#  plot(my.table, type = "s", col = "black", xlab="Date", ylab="Number of Arrivals", main=paste("Arrivals (all modes) ", breaks.interval))

pdf(paste(figures.dir, "acf_", breaks.interval, "all_modes.pdf", sep=""))
acf(my.table, main=breaks.interval)
dev.off()


# bin by hour
breaks.interval <- "hour"

my.table <- table(cut(events$timestamp.event, breaks = breaks.interval))

print(length(my.table))

#  plot(my.table, type = "s", col = "black", xlab="Date", ylab="Number of Arrivals", main=paste("Arrivals (all modes) ", breaks.interval))

pdf(paste(figures.dir, "acf_", breaks.interval, "all_modes.pdf", sep=""))
acf(my.table, main=breaks.interval)
dev.off()


# bin by min
breaks.interval <- "min"

my.table <- table(cut(events$timestamp.event, breaks = breaks.interval))

print(length(my.table))

#  plot(my.table, type = "s", col = "black", xlab="Date", ylab="Number of Arrivals", main=paste("Arrivals (all modes) ", breaks.interval))

pdf(paste(figures.dir, "acf_", breaks.interval, "all_modes.pdf", sep=""))
acf(my.table, main=breaks.interval)
dev.off()



############################ WAVELET TRANSFORM ##########################


## breaks.interval = "mins"

## my.table <- table(cut(events$timestamp.event, breaks = breaks.interval))
## time.series <- as.data.frame(my.table)
## myts <- as.ts(time.series$Freq)
## dwt1 <- dwt(myts)

## pdf(paste(figures.dir, "wavelet_transform_", breaks.interval, "all_modes.pdf", sep=""))
## plot(dwt1, main = paste("Discrete Wavelet Transform, All Modes"))
## dev.off()

## rm(list = ls())


####################### DIVIDE INTO DATE GROUPS, AUTOCORRELATION ###################

# divide events by date
start.date <- start.time
end.date <- end.time
num.groups <- 5

events.grouped <- group.data.by.date(events, start.date, end.date, num.groups)


# run autocorrelation on each group

for (group in 0:num.groups) {
#group <- 4

print(group)

  # subset events
  events <- subset(events.grouped, date.group == group)
  

                                        # make sure sorted in order of event
  events <- events[order(events$timestamp.event),]


  
  ## # bin by month
  ## breaks.interval <- "month"

  ## my.table <- table(cut(events$timestamp.event, breaks = breaks.interval))

  ## print(length(my.table))

  ##                                       #  plot(my.table, type = "s", col = "black", xlab="Date", ylab="Number of Arrivals", main=paste("Arrivals (all modes) ", breaks.interval))

  ## pdf(paste(figures.dir, "acf_group_", group, breaks.interval, "all_modes.pdf", sep="_"))
  ## acf(my.table, main=breaks.interval)
##  acf(my.table, main=paste("date group", group, breaks.interval))

  ## dev.off()

                                        # bin by week
  ## breaks.interval <- "week"

  ## my.table <- table(cut(events$timestamp.event, breaks = breaks.interval))

  ## print(length(my.table))

  ##                                       #  plot(my.table, type = "s", col = "black", xlab="Date", ylab="Number of Arrivals", main=paste("Arrivals (all modes) ", breaks.interval))

  ## pdf(paste(figures.dir, "acf_group_", group, breaks.interval, "all_modes.pdf", sep="_"))
  ## acf(my.table, main=breaks.interval)
##  acf(my.table, main=paste("date group", group, breaks.interval))

  ## dev.off()


                                        # bin by day
  breaks.interval <- "day"

  my.table <- table(cut(events$timestamp.event, breaks = breaks.interval))

  print(length(my.table))

                                        #  plot(my.table, type = "s", col = "black", xlab="Date", ylab="Number of Arrivals", main=paste("Arrivals (all modes) ", breaks.interval))

  pdf(paste(figures.dir, "acf_group_", group, breaks.interval, "all_modes.pdf", sep="_"))
  acf(my.table, main=paste("date group", group, breaks.interval))
  dev.off()


                                        # bin by hour
  breaks.interval <- "hour"

  my.table <- table(cut(events$timestamp.event, breaks = breaks.interval))

  print(length(my.table))

                                        #  plot(my.table, type = "s", col = "black", xlab="Date", ylab="Number of Arrivals", main=paste("Arrivals (all modes) ", breaks.interval))

  pdf(paste(figures.dir, "acf_group_", group, breaks.interval, "all_modes.pdf", sep="_"))
  acf(my.table, main=paste("date group", group, breaks.interval))
  dev.off()


                                        # bin by min
  breaks.interval <- "min"

  my.table <- table(cut(events$timestamp.event, breaks = breaks.interval))

  print(length(my.table))

                                        #  plot(my.table, type = "s", col = "black", xlab="Date", ylab="Number of Arrivals", main=paste("Arrivals (all modes) ", breaks.interval))

  pdf(paste(figures.dir, "acf_group_", group, breaks.interval, "all_modes.pdf", sep="_"))
  acf(my.table, main=paste("date group", group, breaks.interval))
  dev.off()

}



rm(list = ls())
