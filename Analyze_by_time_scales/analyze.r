
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
library(lubridate)

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
# returns the same df, with a new column "date.group", only to time.unit resolution
# units are: "second","minute","hour","day", "week", "month", or "year."
#   starting at 0, going until num.groups - 1
# assumes timestamp column is called "timestamp.event"
group.data.by.date <- function(events, start.date, end.date, num.groups, time.unit) {

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
#      ith.start.date <- trunc(start.date + group * group.size, units = time.unit)
#      ith.end.date <- trunc(start.date + ((group + 1) * group.size) - 1, units = time.unit)

      # use lubridate
      ith.start.date <- floor_date(start.date + group * group.size, time.unit)
      ith.end.date <- floor_date(start.date + ((group + 1) * group.size) - 1, time.unit)
 
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





## Day:

## We are interested in day-of-week, so we want to divide the data into several sections this way.

## - pdf of trips/day for each day of week
## - auto-correlation of events: lags at day scale to reveal cycles
## - inter-event time PDF, CDF for each day of week
## - trip length log-log CCDF for each day of week
## - trip duration log-log CCDF for each day of week
## - fit all above curves


############################################################################

data.dir = "/home/username/Dropbox/Active_Projects/Mobility_Article/Data/"
figures.dir = "./Figures/"

load(paste(data.dir, "trips.Rdata", sep=""))

############################################################################



# by weekly lags, do auto-correlation


# bin by week
breaks.interval <- "week"
week.table <- table(cut(trips$timestamp.event, breaks = breaks.interval))
print(length(week.table))

# just plot number of trips by week
# plot(my.table, type = "s", col = "black", xlab="Date", ylab="Number of Arrivals", main=paste("Arrivals (all modes) ", breaks.interval))

week.df <- data.frame(week.table)
qplot(Var1, Freq, data=week.df) + opts(title = "weekly number of trips", axis.text.x=theme_text(angle=90, hjust = 1))


# auto-correlation
acf(week.table, main=breaks.interval)

###

# divide weeks into 5 sections:

start.date <- min(trips$timestamp.event)
end.date <- max(trips$timestamp.event)
num.groups <- 5

trips.grouped <- group.data.by.date(trips, start.date, end.date, num.groups, "week")

trips.grouped$one <- 1


# for each section, plot:
#   pdf of trips/week

unique.weeks <- unique(floor_date(trips.grouped$timestamp.event, "week"))

#qplot(floor_date(timestamp.event, "week"), data = trips.grouped)#, color = date.group)

timescale = "day"

#for (i in 0:(num.groups - 1)) {
  i <- 0

  # get trips in group
  these.trips <- subset(trips.grouped, date.group == i)

  # find trips/day for this subset
  event.table <- table(cut(these.trips$timestamp.event, breaks = timescale))

event.df <- data.frame(event.table)
qplot(Var1, Freq, data = event.df) +
  ggtitle(paste("# trips/day, date group ", i))


#}


# for each section
#   plot lin CDF inter-event time


# for each section
#   plot log-log CCDF trip length


# for each section
#   plot log-log CCDF trip duration











rm(list = ls())
