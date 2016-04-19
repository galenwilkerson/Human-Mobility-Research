# plot all modes and there best fit functions

# use trips.Rdata instead of mergedtrips.Rdata

# use ggplot2 and plot from created data frame of ccdfs

# use english labels
# white background
# log axis ticks as 10^1, 10^2, etc.

# also plot actual empirical data
# run as: NOT IN HOME DIRECTORY, INSTEAD USE /mnt/share/<directory>
#
# > nohup nice --adjustment=+19 R CMD BATCH <program.r> &
# then:
# > ionice -c3 -p<pid>
#
# clear all variables
rm(list = ls())

# plotting
library("ggplot2")
library(scales)
require(grid)

# plfit code
source("plfit.r")

# base directory
data.dir <- "Data/"
figures.dir <- "Figures/"

# load trip data
load(paste(data.dir,"trips.Rdata", sep=""))




# get rid of "MIV (Mitfahrer) (for plotting simplicity)
#trips <- subset(trips, hvm != "MIV (Mitfahrer)")


# get uniq list of modes
trip.modes <- as.vector(unique(trips$hvm))

# add "all modes" to modes
trip.modes <- c("all_modes", trip.modes)

# remove "no response"
trip.modes <- trip.modes[-length(trip.modes)]


# for now, remove passengers
trip.modes <- trip.modes[-4]



print(trip.modes)


city.types <- unique(trips$sgtyp)

print(city.types)


colors.index <- 1

df <- data.frame()
best.fit.df <- data.frame()

# the order that facets should appear in the legend
legend.order <- factor(c(1,2,4,5,3,6))
legend.order.index <- 1

for (trip.mode in trip.modes) {


print(trip.mode)

############# PREPARATION ############

puf.max.trip.length <- 950

ifelse(trip.mode == "all_modes",

       trips.subset <- subset(trips, wegkm.k <= puf.max.trip.length & wegkm.k > 0),
       trips.subset <- subset(trips, hvm == trip.mode & wegkm.k <= puf.max.trip.length & wegkm.k > 0))


ifelse(trip.mode == "all_modes",
       line.size <- 3,
       line.size <- 2)

ifelse(trip.mode == "all_modes",
       line.alpha <- 1,
       line.alpha <- .7)


# SUBSET TRIPS BY CITY SIZE

trips.subset.city <- subset(trips.subset, sgtyp == city.types[1])

#trips.subset.city <- subset(trips.subset, sgtyp == "Kern- u. Grostdte (kreisf. Stdte u. Oberz. >=100.000 E.)")
#trips.subset.country <- subset(trips.subset, sgtyp == "Lndliche Gemeinden (sonstige Gemeinden)")

puf.trip.lengths <- trips.subset.city$wegkm.k
#puf.trip.lengths <- trips.subset.country$wegkm.k

# remove bad values
puf.trip.lengths <- na.omit(puf.trip.lengths)


data.size <- length(puf.trip.lengths)

## x.interval <- .1

print("number of trips")
print(length(puf.trip.lengths))
print(summary(puf.trip.lengths))
print("sd:")
print(sd(puf.trip.lengths))
print("var:")
print(var(puf.trip.lengths, use = "everything"))


## find xmin, alpha
print("fitting")
#params <- plfit(puf.trip.lengths,"limit",6.00)
params <- plfit(puf.trip.lengths,"limit",3.00)
#params <- plfit(puf.trip.lengths)

num.trips <- length(puf.trip.lengths)
x.min <- as.numeric(params[1])
alpha <- as.numeric(params[2])

print(num.trips)
print(x.min)
print(alpha)




# use hist for finding breaks
    h.inter <- hist(puf.trip.lengths, breaks = 100000, plot = F)
    
# find xvalues, log x values, log ccdf
    my.breaks <-  h.inter$breaks
    trip.lengths <- my.breaks[-1]
    log10.trip.lengths <- log10(trip.lengths)

# find CDF, use for CCDF, log CCDF
    cdf <- ecdf(puf.trip.lengths)
    CDF <- cdf(trip.lengths)
    CCDF1 <- 1 - CDF
    log10.ccdf <- log10(CCDF1)


order <- legend.order[legend.order.index]


## # keep track of histogram results in data frame by category for plotting later
df <- rbind(df, data.frame(log10.trip.lengths, log10.ccdf, trip.mode, line.size, line.alpha, order))



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

best.fit.df <- rbind(best.fit.df, data.frame(log10.trip.lengths, log10.P.X, trip.mode, order))

legend.order.index <- legend.order.index + 1
} # END FOR


#################


this.cat <- "mode"

#colors.set <- c("black", "orange", "red", "blue", "purple")
colors.set <- c("black", "#006633", "red", "blue", "#993300")
#colors.set <- c("black", "green", "orange", "yellow", "red")
#colors.set <- c("black", "#33CC33", "#CC6633", "#FFCC00", "#CC0000")
#colors.set <- c("black", "#33CC33", "#CC6633", "#009933", "#CC0000")
#colors.set <- c("black", "orange", "red", "green", "blue", "purple", "black", "orange", "red", "green", "blue", "purple")

# remove passenger
legend.labels <- c("I. All Modes", "A. Walking",  "C. Automobile Driver",  "B. Bicycling", "D. Public Transport")
#legend.labels <- c("I. All Modes", "A. Walking",  "C. Automobile Driver", "D. Passenger",  "B. Bicycling", "E. Public Transport")
#legend.labels <- c("I. All Modes", "A. Walking",  "B. Bicycling", "C. Automobile Driver", "D. Passenger", "E. Public Transport")


# convert axes labels to 10^x
 scientific_10 <- function(x) {
   parse(text = gsub("1e", "10^", scientific_format()(10^x)))
 }

#tail(df)

# clean up small CCDF values
df <- subset(df, log10.ccdf > -Inf)

# cut of small x-values of best-fit line
#best.fit.df <- subset(best.fit.df, log10.trip.lengths > 0)


#tail(df)
#tail(best.fit.df)

# reorder the trip.mode factor according to the order column
#df$trip.mode.2 <- reorder(df$trip.mode, df$order)


#trip.modes.factor <- factor(trip.modes, levels=rev(levels(trip.modes)) )

# draw vertical line at urban boundary
city.line <- data.frame(x1 = c(1.17,1.17), y1 = c(-4,0))

df.all.modes <- subset(df, trip.mode == "all_modes")
df.only.modes <- subset(df, trip.mode != "all_modes")

plot1 <-ggplot(NULL) +
   # draw fit lines
  geom_line(data = best.fit.df, aes(x = log10.trip.lengths, y = log10.P.X, colour = trip.mode, group=trip.mode), linetype = 2, size = .5, alpha = .7) +
  
  # plot the data
  # rescale the line based on data frame entry "line.size"
  # rescale alpha value based on "line.alpha"
#  geom_line(data = df, aes(x=log10.trip.lengths, y=log10.ccdf, colour=trip.mode, group=trip.mode, size = line.size, alpha = line.alpha)) +
  geom_line(data = df.only.modes, aes(x=log10.trip.lengths, y=log10.ccdf, colour=trip.mode, group=trip.mode, size = line.size, alpha = line.alpha)) +
  geom_line(data = df.all.modes, aes(x=log10.trip.lengths, y=log10.ccdf, colour=trip.mode, group=trip.mode, size = line.size, alpha = line.alpha)) +
  # the transparency values
  scale_alpha(range = c(0.7,1), guide=FALSE) +
  # the size of the lines
  scale_size(range=c(2, 3), guide=FALSE) +

  # make grid light grey
  theme_bw() +

  # legend location
  theme(legend.position=c(0.25,0.25)) +

  # scale axis text
  theme(axis.text.x  = element_text(size=30)) + 
  theme(axis.text.y  = element_text(size=30)) +

  # color and labels of legend
  scale_colour_manual(values=colors.set,
                      name="Mode",
                      breaks=trip.modes,
#                      breaks = order,
                      labels=legend.labels) +

 # scale_color_hue(l=40) +
  
#  scale_colour_brewer(palette = "")
#scale_fill_gradient2(low="red", mid="yellow", high="green")
  
  # size of legend text and titles
  theme(legend.text=element_text(size=24)) +
  theme(legend.title=element_text(size=24)) +

  # remove grey boxes
  theme(legend.key = element_blank()) +
  
  # vertical spacing of legend
  theme(legend.key.height=unit(2,"line")) +

  # axis text size
  theme(axis.title.x=element_text(size=30)) +
  theme(axis.title.y=element_text(size=30)) +

  # axis labels
  xlab("Trip Length (km)") +
  ylab("CCDF") +

  # axes as 10^x
  scale_x_continuous(label=scientific_10, limits=c(-1, 3)) +
  scale_y_continuous(label=scientific_10, limits=c(-4, 0)) +

 


  annotate("text", x = 1.85, y = -1.9, label = "I", size=9) +
  annotate("text", x = 1.4, y = -3.8, label = "A", size=9) +
  annotate("text", x = 2.1, y = -3.5, label = "B", size=9) +
  annotate("text", x = 2.55, y = -2.9, label = "C", size=9) +
#  annotate("text", x = 2, y = -1.3, label = "D", size=9) +
  annotate("text", x = 2.95, y = -3.2, label = "D", size=9)# +

 ##  # draw a vertical line at x = 1.12 - the urban 'diameter'
##   geom_line(data = data.frame(x = c(1.12,1.12), y = c(-4,0)),
##              aes = aes(x = x, y = y), colour = "red")

#  qplot(x1, y1, data = city.line, geom = "line", color = "red")
# geom_line(data = city.line, aes(x = x1, y = y1, colour = "red"))
#  geom_line(data = city.line, aes = aes(x = x1, y = y1), colour = "red")

plot1 

outfile <- paste(figures.dir, this.cat, "trip_length.pdf" , sep = "_")

print(outfile)
ggsave(outfile, width = 240, height = 210, units = "mm")



##### PLOT HIST OF TRIPS BY MODE

trips.subset.city <- subset(trips, sgtyp == city.types[1])

modes <- as.character(unique(trips.subset.city$hvm))


trips.subset.1 <- subset(trips.subset.city, hvm == modes[1] | hvm == modes[2] | hvm == modes[4] | hvm == modes[5])


#new.df <- subset(df, trip.mode != "all_modes")

bar.plot <- qplot(trips.subset.1$hvm) +
  theme_bw()

bar.plot

outfile <- paste(figures.dir, "city_mode_freq.pdf" , sep = "_")

print(outfile)
ggsave(outfile, width = 240, height = 210, units = "mm")


## # histogram of trips by mode for city and country
## puf.max.trip.length <- 950

## trips.subset <- subset(trips, wegkm.k <= puf.max.trip.length & wegkm.k > 0)

## trips.subset.city <- subset(trips.subset, sgtyp == "Kern- u. Grostdte (kreisf. Stdte u. Oberz. >=100.000 E.)")

## qplot(data = trips.subset.city, colors = hvm) +
##   geom_hist()


## trips.subset.country <- subset(trips.subset, sgtyp == "Lndliche Gemeinden (sonstige Gemeinden)")


rm(list = ls())
