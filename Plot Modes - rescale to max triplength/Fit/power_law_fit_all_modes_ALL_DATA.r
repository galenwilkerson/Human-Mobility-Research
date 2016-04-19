# for driving mode

# get and plot several synthetic data sets (from plpva)

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

# plfit code
source("plfit.r")
source("plpva.r")

# base directory
base.data.dir <- "../Data/"
base.figures.dir <- "../Figures/"

# load trip data
load(paste(base.data.dir,"merged_trips.RData", sep=""))

# separate trips into hvm mode

# get uniq list of modes
trip.modes <- as.vector(unique(mergedtrips$hvm))

# add "all modes" to modes
#travel.modes <- c("all_modes", travel.modes)
trip.modes <- c("all_modes", trip.modes)

#print(travel.modes)
print(trip.modes)

color.plot <- c("black","red", "orange", "yellow", "green", "blue", "purple")
color.index <- 1


puf.max.trip.length <- 950


x.interval <- 1

breaks.vector <- seq(0,puf.max.trip.length, by=x.interval)

# remove first entry of breaks vector (for size matching with y values, and to avoid 'zero' problems)
x.values <- breaks.vector[-1]

breaks.vector.rescaled <- breaks.vector / puf.max.trip.length

x.vals.rescaled <- x.values / puf.max.trip.length

x.vals.log10.scale <- log10(x.vals.rescaled)

################# USE ALL MODES ###################

trip.mode <- trip.modes[1]

print(trip.mode)


############# PREPARATION ############


# use all modes
puf.trip.lengths <- subset(mergedtrips, wegkm.k <= puf.max.trip.length & wegkm.k > 0, select=c(wegkm.k))[,1]


puf.trip.lengths <- na.omit(puf.trip.lengths)

data.size <- length(puf.trip.lengths)


################ RESCALE BY MAX TRIP LENGTH FOR EACH MODE ####################

max.trip.length <- max(puf.trip.lengths)

print("max.trip.length")
print(max.trip.length)

puf.trip.lengths.rescaled <- puf.trip.lengths / max.trip.length



###############  EMPIRICAL CCDF ################

puf.hist <- hist(puf.trip.lengths.rescaled,
                 breaks = breaks.vector.rescaled,	
                 plot = F)

# get histogram
puf.hist <- puf.hist$count
#puf.cdf <- ecdf(puf.hist)

#cdf <- puf.pdf(x.vals.rescaled)
#ccdf <- 1 - cdf

puf.cdf <- ecdf(puf.trip.lengths.rescaled)
cdf <- puf.cdf(x.vals.rescaled)
ccdf <- 1 - puf.cdf(x.vals.rescaled)

ccdf.log10.scale <- log10(ccdf)

# new plot or use old plot
plot(x.vals.log10.scale,
#     puf.hist,
#     cdf,
     ccdf.log10.scale,
#     log="xy",
     type = "s",
     col=color.plot[color.index],
     main="trip length normalized by max trip length")



color.index <- color.index + 1




################## REST OF MODES x##################

# remove KEINE ANGABE
trip.modes <- trip.modes[-length(trip.modes)]

for (trip.mode in trip.modes[-1]) {

print(trip.mode)

############# PREPARATION ############


puf.trip.lengths <- subset(mergedtrips, hvm == trip.mode & wegkm.k <= puf.max.trip.length & wegkm.k > 0, select=c(wegkm.k))[,1]

puf.trip.lengths <- na.omit(puf.trip.lengths)

data.size <- length(puf.trip.lengths)


################ RESCALE BY MAX TRIP LENGTH FOR EACH MODE ####################

max.trip.length <- max(puf.trip.lengths)

print("max.trip.length")
print(max.trip.length)

puf.trip.lengths.rescaled <- puf.trip.lengths / max.trip.length


###############  EMPIRICAL CCDF ################

puf.hist <- hist(puf.trip.lengths.rescaled,
                 breaks = breaks.vector.rescaled,	
                 plot = F)

# get histogram
#puf.hist <- puf.hist$count
#puf.cdf <- ecdf(puf.hist)

#cdf <- puf.pdf(x.vals.rescaled)
#ccdf <- 1 - cdf

puf.cdf <- ecdf(puf.trip.lengths.rescaled)
cdf <- puf.cdf(x.vals.rescaled)
ccdf <- 1 - puf.cdf(x.vals.rescaled)

ccdf.log10.scale <- log10(ccdf)

# new plot or use old plot
lines(x.vals.log10.scale,
#     puf.hist,
#     cdf,
     ccdf.log10.scale,
#     log="xy",
      type="s",
     col=color.plot[color.index])
       #,
     #main=trip.mode)



color.index <- color.index + 1

} # end for loop

legend(-2.5,-3,trip.modes,lty=c(1), col=color.plot)


rm(list = ls())
