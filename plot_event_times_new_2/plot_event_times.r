
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
library(plyr)

########################################################


# subset events by time period, return data frame within time period (inclusive)
# times are POSIXlt
subset.events.time.period <- function(events, time.column.name, start.time, end.time) {

  print(time.column.name)
  
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
#events.within.period <- subset.events.time.period(trips, time.column.name = "timestamp.event", start.time = start.time, end.time = end.time)


# get categories, remove 'bad' categories
all.categories <- as.character(unique(trips$hvm))
bad.category <- all.categories[length(all.categories)]
good.categories <- all.categories[-length(all.categories)]

# remove passengers for plot
good.categories <- good.categories[-3] 
passenger.mode <- all.categories[3]

#events.good.categories <- subset(events.within.period, hvm != bad.category)
events.good.categories <- subset(trips, hvm != bad.category & hvm != passenger.mode)

timescales <- c("months", "weeks", "days")
timescale.intervals <- c(1, 2, 14)

# colors for modes
#colors.set <- c("black", "orange", "red", "green", "blue", "purple")
#colors.set <- c("#993300", "blue", "red", "#006633")
#colors.set <- c("blue", "red", #993300", "#006633")

my.brown <- "#993300"
dark.green <- "#006633"
colors.set <- c("red", "blue", my.brown, dark.green)

colors.set <- c("blue", "red", my.brown, dark.green)




#legend.labels <- c("Walk", "Bike", "Drive", "Public Trans.")
#legend.labels <- c("Bike", "Drive", "Public Trans.", "Walk")
legend.labels <- c("Walk", "Drive", "Bike", "Public Trans.")

for (timescale.index in 1:3) {

  timescale <- timescales[timescale.index]
  
  #filen <- paste(figures.dir, "All days", timescale, ".pdf", sep="_")

  events.good.categories$timestamp.event <- as.POSIXct(events.good.categories$timestamp.event)
  table1 <- table(cut(events.good.categories$timestamp.event, breaks = timescale))
  
#  plot(table1, type = "s", col = "black", xlab="Date", ylab="Number of Events",  main=paste("Events: (All modes)", timescale))
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


days.of.week <- c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday")
days.of.week.short <- c("Sun", "Mon", "Tues", "Wed", "Thu", "Fri", "Sat")

#legend.labels <- c("bicycle", "auto. driver", "auto. passenger", "public trans.", "walk")

for (events in events.list) {

  index <- 1

  
  timescale <- timescales[index]
  filen <- paste(figures.dir, timescale, events.timeperiod[events.index], ".pdf", sep="_")

  print(timescale)

  unclassed.data.frame <- get.unclassed.data.frame.with.timescale(events, "timestamp.event")

  #plot1 <-ggplot(NULL) +
  # plot the data
  # rescale the line based on data frame entry "line.size"
  # rescale alpha value based on "line.alpha"
 # geom_bar(data = df, aes(x=log10.trip.lengths, y=log10.ccdf, colour=trip.mode, group=trip.mode, size = line.size, alpha = line.alpha)) +

   # geom_bar(data = unclassed.data.frame, aes(x=wday, colour=hvm, position="dodge"))


  
#  legend.labels <- c("I. All Modes", "A. Walking",  "C. Automobile Driver",  "B. Bicycling", "D. Public Transport")


  # try making a table first
  my.table <- table(unclassed.data.frame$hvm, unclassed.data.frame$wday)
  my.table.freqs <- my.table
  
  # freq for each day
  day.indices <- seq(1,length(days.of.week.short))
  for(day.index in day.indices) {
    my.table.freqs[,day.index] <- my.table[,day.index]/sum(my.table[,day.index])
  }
  

  my.df <- data.frame(my.table.freqs)
  my.df <- rename(my.df, c("Var1" = "mode", "Var2" = "day"))

  day.vals <- seq(0,6)

  my.df$day <- mapvalues(my.df$day, from = day.vals, to = days.of.week)

#  wday <- seq(0,6)
#  unclassed.data.frame$wday <- mapvalues(unclassed.data.frame$wday, from = wday, to = days.of.week)
  
#  plot1 <-
#     ggplot(data=my.df, aes(x=factor(day), y = Freq, fill=factor(mode))) +
#       geom_bar(stat="identity", position="dodge", guide = F)) +

   plot1 <-
     ## ggplot(data=unclassed.data.frame, aes(x=wday)) +
     ##   geom_bar(aes(y = (..count..)/sum(..count..), color=hvm, width = 2, fill=hvm, position="dodge", guide = F)) +
   
    qplot(data=unclassed.data.frame, x=wday, color=hvm, geom="bar", width = 2, fill=hvm, position="dodge", guide = F) + 
#    qplot(data=unclassed.data.frame, x=wday, y = (..count..)/sum(..count..), color=hvm, geom="bar", width = 2, fill=hvm, position="dodge", guide = F) + 
     theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
       theme_bw() +
         theme(legend.position="bottom") +
           
          # scale axis text
           theme(axis.text.x  = element_text(angle=45, size=30, hjust = 1, vjust = 1)) + 
             theme(axis.text.y  = element_text(size=30)) +
               guides(color=FALSE) +


                 scale_x_discrete(label = days.of.week.short, limits=days.of.week.short)+


             # color and labels of legend
               scale_fill_manual(values=colors.set,
                                   name="Mode:",
                                   breaks=good.categories,
                                   labels=legend.labels) +

                                     # color and labels of legend
               scale_color_manual(values=colors.set,
                                   name="Mode:",
                                   breaks=good.categories,
                                   labels=legend.labels) +
                                     
                                        # size of legend text and titles
                                        #  theme(legend.key.size=unit(5,"cm")) + 
                                     theme(legend.text=element_text(size=30)) +
# theme(legend.text=element_text(size=18, angle = -45)) +
                                       theme(legend.title=element_text(size=30)) +
#+ theme(legend.text = element_text(size = 20, colour = "red", angle = 45))
                                        # axis text size
                                         theme(axis.title.x=element_text(size=30)) +
                                           theme(axis.title.y=element_text(size=30)) +

                                        # axis labels
                                             xlab("Day of Week") +
                                                ylab("Count") #+
#                                                 position_dodge(width = .5)


  plot1

#  ggsave(filen, height=8, width=14)

ggsave(filen, width = 240, height = 210, units = "mm")


################

  index <- index + 1

  timescale <- timescales[index]
  filen <- paste(figures.dir, timescale, events.timeperiod[events.index], ".pdf", sep="_")

  print(timescale)


  p <- qplot(data=unclassed.data.frame, x=hour, geom = "bar",  color=hvm, fill=hvm, position="dodge", binwidth = 1) + 
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      theme_bw()  +
        theme(legend.position="bottom") +
  # scale axis text
           theme(axis.text.x  = element_text(size=30)) + 
             theme(axis.text.y  = element_text(size=30)) +
              guides(color=FALSE) +

                # color and labels of legend
               scale_fill_manual(values=colors.set,
                                   name="Mode:",
                                   breaks=good.categories,
                                   labels=legend.labels) +

                                     # color and labels of legend
               scale_color_manual(values=colors.set,
                                   name="Mode:",
                                   breaks=good.categories,
                                   labels=legend.labels) +
             # color and labels of legend
               ## scale_colour_manual(values=colors.set,
               ##                     name="Mode",
               ##                     breaks=good.categories,
               ##                     labels=legend.labels) +
                                     
                                        # size of legend text and titles
                                        #  theme(legend.key.size=unit(5,"cm")) + 
                                     theme(legend.text=element_text(size=30)) +
                                       theme(legend.title=element_text(size=30)) +

                                        # axis text size
                                         theme(axis.title.x=element_text(size=30)) +
                                           theme(axis.title.y=element_text(size=30)) +

                                        # axis labels
                                             xlab("Time of Day (hour)") +
                                               ylab("Count")# +
p
  
#  ggsave(filen, height=8, width=14)
ggsave(filen, width = 240, height = 210, units = "mm")


################


  index <- index + 1


  timescale <- timescales[index]
  filen <- paste(figures.dir, timescale, events.timeperiod[events.index], ".pdf", sep="_")

  print(timescale)


  p <- qplot(data=unclassed.data.frame, x=min, geom = "bar",  color=hvm, fill=hvm, position="dodge", binwidth = 1) + 
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      theme_bw() +
        theme(legend.position="bottom") +
  # scale axis text
           theme(axis.text.x  = element_text(size=30)) + 
             theme(axis.text.y  = element_text(size=30)) +
                             guides(color=FALSE) +

                                        # color and labels of legend
               scale_fill_manual(values=colors.set,
                                 name="Mode:",
                                 breaks=good.categories,
                                 labels=legend.labels) +

                                   scale_color_manual(values=colors.set,
                                   name="Mode:",
                                   breaks=good.categories,
                                   labels=legend.labels) +
                                   
             # color and labels of legend
               ## scale_colour_manual(values=colors.set,
               ##                     name="Mode",
               ##                     breaks=good.categories,
               ##                     labels=legend.labels) +
                                     
                                        # size of legend text and titles
                                        #  theme(legend.key.size=unit(5,"cm")) + 
                                     theme(legend.text=element_text(size=30)) +
                                       theme(legend.title=element_text(size=30)) +

                                        # axis text size
                                         theme(axis.title.x=element_text(size=30)) +
                                           theme(axis.title.y=element_text(size=30)) +

                                        # axis labels
                                             xlab("minute") +
                                               ylab("Count")# +

p
  
                                        #  ggsave(filen, height=8, width=14)
ggsave(filen, width = 240, height = 210, units = "mm")

  ################


  ## index <- index + 1


  ## timescale <- timescales[index]
  ## filen <- paste(figures.dir, timescale, events.timeperiod[events.index], ".pdf", sep="_")

  ## print(timescale)
  
  ## p <- qplot(data=unclassed.data.frame, x=min, geom = "bar",  color=hvm, fill=hvm, position="dodge", binwidth = 1) + 
  ##   theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  ##     theme_bw() +
  ##       theme(legend.position="bottom") +
  ## # scale axis text
  ##          theme(axis.text.x  = element_text(size=30)) + 
  ##            theme(axis.text.y  = element_text(size=30)) +
#              guides(color=FALSE) +

  ##            # color and labels of legend
  ##              ## scale_colour_manual(values=colors.set,
  ##              ##                     name="Mode:",
  ##              ##                     breaks=good.categories,
  ##              ##                     labels=legend.labels) +
                                     
  ##                                       # size of legend text and titles
  ##                                       #  theme(legend.key.size=unit(5,"cm")) + 
  ##                                    theme(legend.text=element_text(size=30, angle = 45)) +
  ##                                      theme(legend.title=element_text(size=30, angle = 45)) +

  ##                                       # axis text size
  ##                                        theme(axis.title.x=element_text(size=30)) +
  ##                                          theme(axis.title.y=element_text(size=30)) +

  ##                                       # axis labels
  ##                                            xlab("weekday") +
  ##                                              ylab("Count")# +
  ## ggsave(filen, height=8, width=14)


  events.index <- events.index + 1
}
    
rm(list = ls())
