# load trips

# for each category

# plot trip length, trip duration, inter-event time


##############


rm(list = ls())

#plotting
library("ggplot2")
library(scales)
require(grid)

# plfit code
source("plfit.r")

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



# get list of categories of interest

# for each category

# split data by value of category

# plot distribution


###############
# TODO: PLOT THESE AS LOG LOG CCDFS

# removed numerical, not categorical variables (must be plotted differently)
# stich.j, stich.wo, w07", "anzpers", "h02", "h04.3", "hhgr06", "hhgr14", "hhgr18", "anzerw", "hp.alter",  "psu.nr"
###############



# plot inter-event time, trip length, trip duration subset by trip subsets
 # "wegkm.k", "wegmin.k", "tempo", "co2weg", "kraftweg",
  
 # make trip length histogram for each category, plot log-log






this.cat <- "sgtypd"



  print("")
  print(this.cat)
  
  cat.vals <- as.vector(unique(trips[,this.cat]))

# remove last entry
#cat.vals <- cat.vals[-length(cat.vals)]

  # the category name, used for legend and plotting
  Legend <- trips[,this.cat]

  ## # plot density
  ## qplot(timestamp.event, data = trips, geom = "density", adjust = 1/10, color = Legend) +
  ##   ggtitle(paste(this.cat, "time frequency")) 

  ## ggsave(paste(figures.dir, this.cat, "time_frequency.pdf" , sep = "_"), width = 297, height = 210, units = "mm")

  
   
# REMOVE BAD/SPECIAL VALUES

  trips.subset <- subset(trips, wegkm.k <= 950)
  Legend <- trips.subset[,this.cat]





## ######################### FIND log-log CCDF #############################

# for each subset of cat, find the ccdf

df <- data.frame()
best.fit.df <- data.frame()

legend.labels <- c("large core-city/metropolis (pop. > 500,000)",
                   "larger mid-size city (pop. >= 50,000)",
                   "small rural community (pop. < 7,500)",
                   "smaller mid-size city (pop. < 50,000)",
                   "large rural community (pop. >= 7,500)",
                   "small city",
                   "smaller core-city/metropolis (pop. < 500,000)")

legend.order <- factor(c(1,2,3,4,5,6))
legend.order.index <- 1


  for(cat.val in cat.vals) {

    print(cat.val)
    
    trips.subset.cat <- subset(trips.subset, trips.subset[,this.cat] == cat.val)


puf.trip.lengths <- trips.subset.cat$wegkm.k


# remove bad values
puf.trip.lengths <- na.omit(puf.trip.lengths)




## find xmin, alpha
print("fitting")
params <- plfit(puf.trip.lengths,"limit",3.00)
#params <- plfit(puf.trip.lengths)

num.trips <- length(puf.trip.lengths)
x.min <- as.numeric(params[1])
alpha <- as.numeric(params[2])

print(num.trips)
print(x.min)
print(alpha)




    

## # use hist for finding breaks
    h.inter <- hist(trips.subset.cat$wegkm.k, breaks = 100000, plot = F)
    
## # find xvalues, log x values, log ccdf
    my.breaks <-  h.inter$breaks
    trip.lengths <- my.breaks[-1]
    log10.trip.length <- log10(trip.lengths)

## # find CDF, use for CCDF, log CCDF
    cdf <- ecdf(trips.subset.cat$wegkm.k)
    CDF <- cdf(trip.lengths)
    CCDF1 <- 1 - CDF
    log10.ccdf <- log10(CCDF1)

## # keep track of results in data frame by category for plotting later
#    df <- rbind(df, data.frame(log10.trip.length, log10.ccdf, cat.val))

order <- legend.order[legend.order.index]
    
    df <- rbind(df, data.frame(log10.trip.length, log10.ccdf, cat.val, order))


############### PLOT CCDF OF BEST FIT FUNCTION ###############

# using vector of trip.lengths, compute P(X > x) as vector

# P(x) = ((x/x_min)^(1-alpha))
P.X <- ((trip.lengths/x.min)^(1-alpha))

cdf.at.xmin <- cdf(x.min)
ccdf.at.xmin <- 1 - cdf.at.xmin
#ccdf.at.xmin

log.ccdf.at.xmin <- log10(ccdf.at.xmin)
#log.ccdf.at.xmin

# plot CCDF
log10.P.X <- log10(P.X) + log.ccdf.at.xmin

best.fit.df <- rbind(best.fit.df, data.frame(log10.trip.length, log10.P.X, cat.val, order))






    
    print(cat.val)
    print("mean:")
    print(mean(trips.subset.cat$wegkm.k))
    print("sd:")
    print(sd(trips.subset.cat$wegkm.k))

    legend.order.index <- legend.order.index + 1

  }

  
  ## # plot trip length
  ## qplot(log10.trip.length, log10.ccdf, data = df, geom="line", color = cat.val) +
  ##   ggtitle(paste(this.cat, "log log trip length (km)"))
 

  ## ggsave(paste(figures.dir, this.cat, "trip_length.pdf" , sep = "_"), width = 297, height = 210, units = "mm")



# convert axes labels to 10^x
 scientific_10 <- function(x) {
   parse(text = gsub("1e", "10^", scientific_format()(10^x)))
 }

line.size <- 2

line.alpha <- .2

# clean up small CCDF values
df <- subset(df, log10.ccdf > -Inf)

plot1 <-ggplot(NULL) +
  # plot the data
  # rescale the line based on data frame entry "line.size"
  # rescale alpha value based on "line.alpha"
  geom_line(data = df, aes(x=log10.trip.length, y=log10.ccdf, colour=cat.val, group=cat.val, size = line.size, alpha = line.alpha)) +

 # the transparency values
  scale_alpha(range = c(0.7,1), guide=FALSE) +
  # the size of the lines
  scale_size(range=c(2, 3), guide=FALSE) +

            
  # make grid light grey
  theme_bw() +

  # legend location
  theme(legend.position=c(0.4, 0.25)) +

  # scale axis text
  theme(axis.text.x  = element_text(size=24)) + 
  theme(axis.text.y  = element_text(size=24)) +

  #color and labels of legend
  scale_colour_brewer(palette="Set1",
                      name="Municipality Type",
                      breaks=cat.vals,
                      labels=legend.labels) +

  # size of legend text and titles
#  theme(legend.key.size=unit(5,"cm")) + 
  theme(legend.text=element_text(size=18)) +
  theme(legend.title=element_text(size=18)) +

  # line thickness
  guides(colour = guide_legend(override.aes = list(size=line.size))) +
  
  # remove grey boxes
  theme(legend.key = element_blank()) +
  
  # vertical spacing of legend
  theme(legend.key.height=unit(1.5,"line")) +

  # axis text size
  theme(axis.title.x=element_text(size=24)) +
  theme(axis.title.y=element_text(size=24)) +

  # axis labels
  xlab("Trip Length (km)") +
  ylab("CCDF") +

  # axes as 10^x
  scale_x_continuous(label=scientific_10, limits=c(-1, 3)) +
  scale_y_continuous(label=scientific_10, limits=c(-4, 0)) #+

  ## # draw fit line
#geom_line(data = best.fit.df, aes(x = log10.trip.length, y = log10.P.X, colour = cat.val, group=cat.val), linetype = 2, size = 1.5, alpha = line.alpha) #+


  ## annotate("text", x = 1.15, y = -1.1, label = "I", size=8) +
  ## annotate("text", x = 1.4, y = -3.8, label = "A", size=8) +
  ## annotate("text", x = 2.1, y = -3.5, label = "B", size=8) +
  ## annotate("text", x = 2.55, y = -2.9, label = "C", size=8) +
  ## annotate("text", x = 2, y = -1.3, label = "D", size=8) +
  ## annotate("text", x = 2.95, y = -3.2, label = "E", size=8)

plot1 

outfile <- paste(figures.dir, this.cat, "trip_length.pdf" , sep = "_")
print(outfile)

ggsave(outfile, width = 240, height = 210, units = "mm")



rm(list = ls())














































