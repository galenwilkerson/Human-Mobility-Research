#  plot CDFs for:

# - trips only,
# - trips and travel together, for:
#   -  all modes
#   -  automobile
#   -  bicycle

##################

# run as: (NOT IN HOME DIRECTORY, INSTEAD USE /mnt/share/<directory>
#
# > nohup nice --adjustment=+19 R CMD BATCH <program.r> &
# then:
# > ionice -c3 -p<pid>
#
# clear all variables
rm(list = ls())

# to load STATA files
library(foreign)

# plotting
library("ggplot2")

# plfit code
source("plfit.r")
source("plpva.r")

# base directory
base.data.dir <- "../Data/"

# load travel data
travel.trips <- read.dta(paste(base.data.dir, "MiD2008_PUF_Reisen.dta", sep=""))

# load trip data
load(paste(base.data.dir,"merged_trips.RData", sep=""))


puf.max.travel.length <- 20000

puf.max.trip.length <- 950

x.interval <- 1

breaks.vector <- seq(0,puf.max.travel.length, by=x.interval)

# remove last entry of breaks vector (for size matching with y values)
x.values <- breaks.vector[-1]

x.vals.log10.scale <- log10(x.values)


color.plot <- c("black","red", "orange", "yellow", "green", "blue", "purple")
color.index <- 1


#############################  PREPARATION for ALL_MODES

# separate trips into hvm mode

# get uniq list of modes
travel.modes <- as.vector(unique(travel.trips$hvm_r))
trip.modes <- as.vector(unique(mergedtrips$hvm))

# add "all modes" to modes
travel.modes <- c("all_modes", travel.modes)
trip.modes <- c("all_modes", trip.modes)

print(travel.modes)
print(trip.modes)

current.mode <- "all_modes"


# subset reisen
puf.travel.lengths <- subset(travel.trips, p1016 <= puf.max.travel.length & p1016 > 0, select=c(p1016))[,1]
puf.travel.lengths <- na.omit(puf.travel.lengths)

# subset trips
puf.trip.lengths <- subset(mergedtrips, wegkm.k <= puf.max.trip.length & wegkm.k > 0, select=c(wegkm.k))[,1]
puf.trip.lengths <- na.omit(puf.trip.lengths)

trip.and.travel.trip.lengths <- c(puf.trip.lengths, puf.travel.lengths)

# fit travels and trips
## find xmin, alpha
print("fitting")
params <- plfit(trip.and.travel.trip.lengths)

num.trips <- length(trip.and.travel.trip.lengths)
x.min <- as.numeric(params[1])
alpha <- as.numeric(params[2])

print(num.trips)
print(x.min)
print(alpha)

summary(trip.and.travel.trip.lengths)



###############  EMPIRICAL CCDF ################

######### TRIPS ONLY #########

# get cdf

puf.cdf <- ecdf(puf.trip.lengths)
cdf <- puf.cdf(x.values)
ccdf <- 1 - puf.cdf(x.values)

ccdf.log10.scale <- log10(ccdf)

# new plot or use old plot
plot(x.vals.log10.scale,
     ccdf.log10.scale,
     type = "s",
     col=color.plot[color.index],
     main="CCDF of trip length \n for trips; trip & travel merged")


###### TRIPS AND TRAVEL #####

# get cdf

puf.cdf <- ecdf(trip.and.travel.trip.lengths)
cdf <- puf.cdf(x.values)
ccdf <- 1 - puf.cdf(x.values)

ccdf.log10.scale <- log10(ccdf)

# new plot or use old plot
lines(x.vals.log10.scale,
     ccdf.log10.scale,
     type = "s",
     lty = 2,
     col=color.plot[color.index])


color.index <- color.index + 1


## ######## #######################  PREPARATION for Auto driving


## travel.mode <- travel.modes[2]
## trip.mode <- trip.modes[2]

## travel.mode
## trip.mode

## # subset reisen
## puf.travel.lengths <- subset(travel.trips, hvm_r == travel.mode & p1016 <= puf.max.travel.length & p1016 > 0, select=c(p1016))[,1]

## puf.travel.lengths <- na.omit(puf.travel.lengths)

## # subset trips

## puf.trip.lengths <- subset(mergedtrips, hvm == trip.mode & wegkm.k <= puf.max.trip.length & wegkm.k > 0, select=c(wegkm.k))[,1]

## puf.trip.lengths <- na.omit(puf.trip.lengths)

## trip.and.travel.trip.lengths <- c(puf.trip.lengths, puf.travel.lengths)

## summary(puf.travel.lengths)
## summary(puf.trip.lengths)
## summary(trip.and.travel.trip.lengths)
## ###############  EMPIRICAL CCDF ################



## ######### TRIPS ONLY #########

## # get cdf

## puf.cdf <- ecdf(puf.trip.lengths)
## cdf <- puf.cdf(x.values)
## ccdf <- 1 - puf.cdf(x.values)

## ccdf.log10.scale <- log10(ccdf)

## # new plot or use old plot
## points(x.vals.log10.scale,
##        ccdf.log10.scale,
##        type = "s",
##        col=color.plot[color.index])


## ###### TRIPS AND TRAVEL #####

## # get cdf

## puf.cdf <- ecdf(trip.and.travel.trip.lengths)
## cdf <- puf.cdf(x.values)
## ccdf <- 1 - puf.cdf(x.values)

## ccdf.log10.scale <- log10(ccdf)

## # new plot or use old plot
## points(x.vals.log10.scale,
##        ccdf.log10.scale,
##        type = "s",
##        lty = 2,
##        col=color.plot[color.index])





## color.index <- color.index + 1



## ######## #######################  PREPARATION for bicycling


## travel.mode <- travel.modes[8]
## trip.mode <- trip.modes[6]

## travel.mode
## trip.mode

## # subset reisen
## puf.travel.lengths <- subset(travel.trips, hvm_r == travel.mode & p1016 <= puf.max.travel.length & p1016 > 0, select=c(p1016))[,1]

## puf.travel.lengths <- na.omit(puf.travel.lengths)

## # subset trips
## puf.trip.lengths <- subset(mergedtrips, hvm == trip.mode & wegkm.k <= puf.max.trip.length & wegkm.k > 0, select=c(wegkm.k))[,1]
## puf.trip.lengths <- na.omit(puf.trip.lengths)

## trip.and.travel.trip.lengths <- c(puf.trip.lengths, puf.travel.lengths)

## summary(puf.travel.lengths)
## summary(puf.trip.lengths)
## summary(trip.and.travel.trip.lengths)

## ###############  EMPIRICAL CCDF ################



## ######### TRIPS ONLY #########

## # get cdf

## puf.cdf <- ecdf(puf.trip.lengths)
## cdf <- puf.cdf(x.values)
## ccdf <- 1 - puf.cdf(x.values)

## ccdf.log10.scale <- log10(ccdf)

## # new plot or use old plot
## points(x.vals.log10.scale,
##        ccdf.log10.scale,
##        type = "s",
##        col=color.plot[color.index])


## ###### TRIPS AND TRAVEL #####

## # get cdf

## puf.cdf <- ecdf(trip.and.travel.trip.lengths)
## cdf <- puf.cdf(x.values)
## ccdf <- 1 - puf.cdf(x.values)

## ccdf.log10.scale <- log10(ccdf)

## # new plot or use old plot
## points(x.vals.log10.scale,
##        ccdf.log10.scale,
##        type = "s",
##        lty = 2,
##        col=color.plot[color.index])



## color.index <- color.index + 1


## legend(0,-3,c("all modes", "auto", "bicycle"),lty=c(1), col=color.plot)
## legend(0,-4,c("trip only", "trip & travel"),lty=c(1,2), col="black")



rm(list = ls())
