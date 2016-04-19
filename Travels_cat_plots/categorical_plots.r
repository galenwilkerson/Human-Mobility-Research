# load travels (Reisen)

# for each category

# plot histogram of categories

# plot trip frequency, trip length, trip duration


##############


rm(list = ls())
library(ggplot2)
library(foreign)

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


load(paste(data.dir, "travels.Rdata", sep=""))


##############



# get list of categories of interest

# for each category

# split data by value of category

# plot distribution


###############
# TODO: PLOT THESE AS LOG LOG CCDFS

# removed numerical, not categorical variables (must be plotted differently)
###############
#"r_gew","p10","p1014","p1015","p1016", "psu_nr", "hp_alter"

cats <- c("p1016")

#cats <- c("p101","p1012","p1013_1","p1013_2","p1013_3","p1013_4","p1013_5","p1013_6","p1013_7","p1013_8","p1013_9","hvm_r","hheink","oek_stat","h02","h04_3","hhgr06","hhgr14","hhgr18","anzerw","hhtyp","hp_sex","hp_altg1","hp_altg2","hp_altg3","hp_besch","hp_pkwfs","pergrup","pergrup1","lebensph","gesein","bland","westost","polgk","rtyp","rtypd7","ktyp","ktyp_zsg","sgtyp","sgtypd")


# plot trip length, trip duration subset by trip subsets
# "wegkm.k", "wegmin.k"
  
# make trip length histogram for each category, plot log-log


travels.subset <- subset(travels, p1016 <= 20000)


for (this.cat in cats) {

  print(this.cat)
  
  cat.vals <- as.vector(unique(travels.subset[,this.cat]))

  #print(cat.vals)
  
############### frequency

  
  ## # the category name, used for legend and plotting
  ## Legend <- travels[,this.cat]

  ## # plot density
  ## qplot(timestamp.event, data = travels, geom = "density", adjust = 1/10, color = Legend) +
  ##   ggtitle(paste(this.cat, "time frequency")) 

  ## ggsave(paste(figures.dir, this.cat, "time_frequency.pdf" , sep = "_"), width = 297, height = 210, units = "mm")

  
   
# REMOVE BAD/SPECIAL VALUES


  Legend <- travels.subset[,this.cat]


############################### PLOT HISTOGRAM

  qplot(Legend) +
    scale_x_discrete(limits=cat.vals) +
      theme(axis.text.x = element_text(angle=90, vjust=-1)) +
        ggtitle(paste(this.cat, "histogram"))


  ggsave(paste(figures.dir, this.cat, "histogram.pdf" , sep = "_"), width = 297, height = 210, units = "mm")


## ######################### FIND log-log CCDF #############################

# for each subset of cat, find the ccdf

  df <- data.frame()
  
  for(cat.val in cat.vals) {

    travels.subset.cat <- subset(travels.subset, travels.subset[,this.cat] == cat.val)
  

## # use hist for finding breaks
    h.inter <- hist(travels.subset.cat$p1016, breaks = 100000, plot = F)
    
## # find xvalues, log x values, log ccdf
    my.breaks <-  h.inter$breaks
    travel.lengths <- my.breaks[-1]
    log10.travel.length <- log10(travel.lengths)

## # find CDF, use for CCDF, log CCDF
    cdf <- ecdf(travels.subset.cat$p1016)
    CDF <- cdf(travel.lengths)
    CCDF1 <- 1 - CDF
    log10.ccdf <- log10(CCDF1)

## # keep track of results in data frame by category for plotting later
    df <- rbind(df, data.frame(log10.travel.length, log10.ccdf, cat.val))

}

  
  # plot travel length
  qplot(log10.travel.length, log10.ccdf, data = df, geom="line", color = cat.val) +
    ggtitle(paste(this.cat, "log log travel length (km)"))
 

  ggsave(paste(figures.dir, this.cat, "travel_length.pdf" , sep = "_"), width = 297, height = 210, units = "mm")
 
## ############## duration
  
##  ##  trips.subset <- subset(trips, wegmin.k <= 480)
## ##   Legend <- trips.subset[,this.cat]




##   df <- data.frame()
  
##   for(cat.val in cat.vals) {

##     trips.subset.cat <- subset(trips.subset, trips.subset[,this.cat] == cat.val)
  

## ## # use hist for finding breaks
##     h.inter <- hist(trips.subset.cat$wegmin.k, breaks = 100000, plot = F)
    
## ## # find xvalues, log x values, log ccdf
##     my.breaks <-  h.inter$breaks
##     trip.durations <- my.breaks[-1]
##     log10.trip.duration <- log10(trip.durations)

## ## # find CDF, use for CCDF, log CCDF
##     cdf <- ecdf(trips.subset.cat$wegmin.k)
##     CDF <- cdf(trip.durations)
##     CCDF1 <- 1 - CDF
##     log10.ccdf <- log10(CCDF1)

## ## # keep track of results in data frame by category for plotting later
##     df <- rbind(df, data.frame(log10.trip.duration, log10.ccdf, cat.val))

## }

## ##   # plot trip duration

##   qplot(log10.trip.duration, log10.ccdf, data = df, geom="line", color = cat.val) +
##     ggtitle(paste(this.cat, "log log trip duration (min)"))


  
  


##   ggsave(paste(figures.dir, this.cat, "trip_duration.pdf" , sep = "_"), width = 297, height = 210, units = "mm")


  
#  trips.subset <- subset(trips, tempo <= 639.96)
#  Legend <- trips.subset[,this.cat]


  ## # plot trip speed
  ## qplot(tempo, data = trips.subset, geom="density", color = Legend, log="xy") +
  ##   ggtitle(paste(this.cat, "log log trip speed (km/h)"))

  ## ggsave(paste(figures.dir, this.cat, "trip_speed.pdf" , sep = "_"), width = 297, height = 210, units = "mm")



  
  ## trips.subset <- subset(trips, co2weg <= 193449)
  ## Legend <- trips.subset[,this.cat]
  
  ## # plot trip co2
  ## qplot(co2weg, data = trips.subset, geom="density", color = Legend, log="xy") +
  ##   ggtitle(paste(this.cat, "log log trip CO2 (kg)"))

  ## ggsave(paste(figures.dir, this.cat, "trip_CO2.pdf" , sep = "_"), width = 297, height = 210, units = "mm")


  
  ## trips.subset <- subset(trips, kraftweg <= 62730)
  ## Legend <- trips.subset[,this.cat]

  
  ## # plot trip fuel
  ## qplot(kraftweg, data = trips.subset, geom="density", color = Legend, log="xy") +
  ##   ggtitle(paste(this.cat, "log log trip fuel (Liter)"))

  ## ggsave(paste(figures.dir, this.cat, "trip_fuel.pdf" , sep = "_"), width = 297, height = 210, units = "mm")
  
  # get inter-event times

  ## trips.subsets <- list()

  ## i <- 1

  ## for (cat.val in cat.vals) {
    
  ##   # get rows by category
  ##   category.events <- subset(trips, trips[,this.cat] == cat.val)
  ##   trips.subsets[[i]] <- category.events
  ##   i <- i + 1
  ## }
  
  #trips.subsets <- subset.events.category(trips, this.cat, cat.vals)


  ## new.trips <- data.frame()
  
  ## for (trips.subset in trips.subsets) {
    
  ##   # make sure sorted in order of event
  ##   trips.subset <- trips.subset[order(trips.subset$timestamp.event),]

  ##   inter.event.times <- as.numeric(diff(trips.subset$timestamp.event))
  ##   inter.event.times.mins <- inter.event.times/60

  ##   trips.subset$inter.event.times.mins <- inter.event.times.mins

  ##   new.trips <- rbind(new.trips, trips.subset)

  ## }


  ## qplot(inter.event.times.mins, data = new.trips.subset, geom = "density", color = trips[,this.cat])
}



## # create factors with value labels
## mtcars$gear <- factor(mtcars$gear,levels=c(3,4,5),
##    labels=c("3gears","4gears","5gears"))
## mtcars$am <- factor(mtcars$am,levels=c(0,1),
##    labels=c("Automatic","Manual"))
## mtcars$cyl <- factor(mtcars$cyl,levels=c(4,6,8),
##    labels=c("4cyl","6cyl","8cyl"))

## # Kernel density plots for mpg
## # grouped by number of gears (indicated by color)
## qplot(mpg, data=mtcars, geom="density", fill=gear, alpha=I(.5),
##    main="Distribution of Gas Milage", xlab="Miles Per Gallon",
##    ylab="Density")
