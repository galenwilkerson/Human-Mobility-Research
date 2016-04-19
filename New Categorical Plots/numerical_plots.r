# load trips

# for each numerical value

# plot trip length, trip duration, inter-event time


##############


rm(list = ls())
library(ggplot2)

##############

# subset events by category, return list of subset data frames
subset.events.category <- function(events, subset.category, cat.values) {

  subsets <- list()

  i <- 1

  for (cat.val in cat.values) {
    
    # get rows by category
    category.events <- subset(events, events[,subset.category] == cat.val)
    subsets[[i]] <- category.events
    i <- i + 1
  }

   return(subsets)

}


##############


data.dir = "/home/username/Dropbox/Active_Projects/Mobility_Article/Data/"
figures.dir = "./Figures/"

load(paste(data.dir, "trips.Rdata", sep=""))


##############



# get list of numerical values of interest

# for each numerical value

# split data by range of category

# plot distribution



###############

# TODO: PLOT THESE AS LOG LOG CCDFS

# removed numerical, not categorical variables (must be plotted differently)
#  "stich.j", "stich.wo",
num.cats <- c("w07", "anzpers", "h02", "h04.3", "hhgr06", "hhgr14", "hhgr18", "anzerw", "hp.alter",  "psu.nr")

###############



# for each numerical value

# plot density

# see change in time

# plot trip length, trip duration, and inter-event time by sections of numerical data

for (this.cat in num.cats) {

  print(this.cat)

  qplot(trips[,this.cat], data = trips, geom = "density", log="xy")
  
  qplot(this.cat, data = trips, geom = "density") +
    ggtitle(paste(this.cat, "density"))


  ggsave(paste(figures.dir, this.cat, "density.pdf" , sep = "_"), width = 297, height = 210, units = "mm")

}
