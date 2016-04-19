
# load data

# subset by categorical variable(s)

# remove error/special values

# plot event autocorrelation by category


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
get.unclassed.data.frame.by.timescale <- function(events, time.stamp.column.name, timescale.name) {

  # break apart timestamp into components
  df.unclassed <- data.frame(events, unclass(as.POSIXlt(events[,time.stamp.column.name])))
 # h <- hist(df.unclassed[,timescale.name], plot = F)
      
  return(df.unclassed)
}

############################################################################


data.dir = "/home/username/Dropbox/Active_Projects/Mobility_Article/Data/"
figures.dir = "./Figures/"

load(paste(data.dir, "trips.Rdata", sep=""))


# get events within time range
start.time <- as.POSIXlt("2008-04-01 00:00:00 CEST")
end.time <- as.POSIXlt("2008-12-31 23:59:59 CEST")
events.within.period <- subset.events.time.period(trips, time.column.name = "timestamp.event", start.time = start.time, end.time = end.time)


# get categories, remove 'bad' categories
categories <- as.character(unique(trips$hvm))
categories <- categories[-length(categories)]

# get list of events in each category
category.column.name <- "hvm"
events.by.category <- subset.events.category(events.within.period, category.column.name, categories)

# we want to see autocorrelations at different scales
timescales <- c("month", "week", "day", "hour", "min")

   # for each timescale
timescale.index <- 1
for (timescale in timescales) {

   #timescale <- timescales[1] #TEST

  print(timescale)
  
   #for each category, plot frequency of events over entire time period
  category.index <- 1
  for(events.this.category in events.by.category) {
   # events.this.category <- events.by.category[[1]] # TEST

    category <- categories[category.index]
    print(categories[category.index])
    
   # get frequency of events.this.category by time-scale (time series)
    event.frequency.df <- get.frequency.by.timescale(events.this.category, "timestamp.event", timescale)

    event.frequency.df$timestamp <- as.POSIXct(event.frequency.df$timestamp)


   # save in file
    filen <- paste(figures.dir, "autocorrelation:", timescale, gsub(" ", "_", categories[category.index]), ".pdf", sep="_")

    pdf(filen)

   # plot autocorrelation
    acf(event.frequency.df$timestamp, main=paste("autocorrelations:", timescale, ",", category))

    dev.off()

    category.index <- category.index + 1
  }
}


    
rm(list = ls())
