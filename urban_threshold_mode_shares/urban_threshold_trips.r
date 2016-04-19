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
data.dir <- "/home/username/Dropbox/Active_Projects/Mobility_Article/Data/"
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

puf.max.trip.length <- 950

thresh <- 10^(1.17)

intra.urban.trips <- subset(trips, wegkm.k <= thresh)
inter.urban.trips <- subset(trips, wegkm.k < puf.max.trip.length & wegkm.k >= thresh)



intra.mode.shares <- table(intra.urban.trips$hvm)
inter.mode.shares <- table(inter.urban.trips$hvm)

intra.mode.shares <- intra.mode.shares/sum(intra.mode.shares)
inter.mode.shares <- inter.mode.shares/sum(inter.mode.shares)


intra.mode.shares
inter.mode.shares

results <- rbind(intra.mode.shares, inter.mode.shares)

write.csv(results * 100, "urban_mode_shares.csv")

rm(list = ls())
