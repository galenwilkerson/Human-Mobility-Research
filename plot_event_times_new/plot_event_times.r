
# load data

# subset by categorical variable(s)

# remove error/special values

# plot event density at different time scales over different time ranges

# examples:
# trips/day within subset of time period (time series)
# trips/day summed over all weeks within time period (histogram)

# trips/month within subset time range
# trips/month summed over all years within time period



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

# PLOT LONGER TIME SCALES


data.dir = "/home/username/Dropbox/Active_Projects/Mobility_Article/Data/"
figures.dir = "./Figures/"

load(paste(data.dir, "trips.Rdata", sep=""))


# get events within time range
start.time <- as.POSIXlt("2008-04-01 00:00:00 CEST")
end.time <- as.POSIXlt("2008-12-31 23:59:59 CEST")
events.within.period <- subset.events.time.period(trips, time.column.name = "timestamp.event", start.time = start.time, end.time = end.time)


# get categories, remove 'bad' categories
all.categories <- as.character(unique(trips$hvm))
bad.category <- all.categories[length(all.categories)]
good.categories <- all.categories[-length(all.categories)]

events.good.categories <- subset(events.within.period, hvm != bad.category)

timescales <- c("months", "weeks", "days")
timescale.intervals <- c(1, 2, 14)



for (timescale.index in 1:3) {

  timescale <- timescales[timescale.index]
  
  #filen <- paste(figures.dir, "All days", timescale, ".pdf", sep="_")

  events.good.categories$timestamp.event <- as.POSIXct(events.good.categories$timestamp.event)
  table1 <- table(cut(events.good.categories$timestamp.event, breaks = timescale))
  
  plot(table1, type = "s", col = "black", xlab="Date", ylab="Number of Events",  main=paste("Events: (All modes)", timescale))
}




##############################################################################

# NOW PLOT HISTOGRAMS OF SHORTER TIMESCALES

print("Plotting histograms")

# FOR EACH TIMESCALE (DAY OF WEEK, HOUR OF DAY, MINUTE OF DAY)
# PLOT HISTOGRAM OF EVENTS PER TIMESCALE

timescales <- c("wday", "hour", "min")


events.timeperiod <- c("all", "weekdays", "saturdays", "sundays")

events.all <- events.good.categories
events.weekdays <- subset(events.good.categories, )
events.saturday <- subset(events.good.categories, )
events.sunday <- subset(events.good.categories, )

events.list <- list(events.all, events.weekdays, events.saturday, events.sunday)

events.index <- 1
for (events in events.list) {

  index <- 1

  
  timescale <- timescales[index]
  filen <- paste(figures.dir, timescale, events.timeperiod[events.index], ".pdf", sep="_")

  unclassed.data.frame <- get.unclassed.data.frame.with.timescale(events, "timestamp.event")

  p <- qplot(data=unclassed.data.frame, x=wday, geom = "bar",  color=hvm, fill=hvm, position="dodge") + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  ggtitle(paste(timescale, events.timeperiod[events.index]))


#p

  ggsave(filen, height=8, width=14)



################

  index <- index + 1

  timescale <- timescales[index]
  filen <- paste(figures.dir, timescale, events.timeperiod[events.index], ".pdf", sep="_")


  p <- qplot(data=unclassed.data.frame, x=hour, geom = "bar",  color=hvm, fill=hvm, position="dodge", binwidth = 1) + 
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      ggtitle(paste(timescale, events.timeperiod[events.index]))


  ggsave(filen, height=8, width=14)


################


  index <- index + 1


  timescale <- timescales[index]
  filen <- paste(figures.dir, timescale, events.timeperiod[events.index], ".pdf", sep="_")


  p <- qplot(data=unclassed.data.frame, x=min, geom = "bar",  color=hvm, fill=hvm, position="dodge", binwidth = 1) + 
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      ggtitle(paste(timescale, events.timeperiod[events.index]))

  ggsave(filen, height=8, width=14)

  ################


  index <- index + 1


  timescale <- timescales[index]
  filen <- paste(figures.dir, timescale, events.timeperiod[events.index], ".pdf", sep="_")


  p <- qplot(data=unclassed.data.frame, x=min, geom = "bar",  color=hvm, fill=hvm, position="dodge", binwidth = 1) + 
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      ggtitle(paste(timescale, events.timeperiod[events.index]))

  ggsave(filen, height=8, width=14)


  events.index <- events.index + 1
}
    
rm(list = ls())
