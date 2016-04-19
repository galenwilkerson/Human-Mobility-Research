# for one mode

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
source("synthdata.r")

# base directory
base.data.dir <- "../Data/"
base.figures.dir <- "../Figures/"

# load trip data
load(paste(base.data.dir,"merged_trips.RData", sep=""))

# separate trips into hvm mode

# get uniq list of modes
trip.modes <- as.vector(unique(mergedtrips$hvm))

# add "all modes" to modes
trip.modes <- c("all_modes", trip.modes)

print(trip.modes)

for (trip.mode in trip.modes) {
#trip.mode <- trip.modes[3]

print(trip.mode)

############# PREPARATION ############

puf.max.trip.length <- 950

ifelse(trip.mode == "all_modes",
       puf.trip.lengths <- subset(mergedtrips, wegkm.k <= puf.max.trip.length & wegkm.k > 0, select=c(wegkm.k))[,1],
       puf.trip.lengths <- subset(mergedtrips, hvm == trip.mode & wegkm.k <= puf.max.trip.length & wegkm.k > 0, select=c(wegkm.k))[,1])

puf.trip.lengths <- na.omit(puf.trip.lengths)

data.size <- length(puf.trip.lengths)

x.interval <- .1

breaks.vector <- seq(0,puf.max.trip.length, by=x.interval)

# remove first entry of breaks vector (for size matching with y values, and to avoid 'zero' problems)
x.values <- breaks.vector[-1]
log10.xvals <- log10(x.values)

## ## # find xmin, alpha
print("fitting")
params <- plfit(puf.trip.lengths)

num.trips <- length(puf.trip.lengths)
x.min <- as.numeric(params[1])
alpha <- as.numeric(params[2])

num.trips
x.min
alpha

#x.min <- 58.9
#alpha <- 2.419613


#################  Draw vertical line at xmin

outfile <- paste(base.figures.dir,"ccdf_",trip.mode,".pdf", sep="")
  
pdf(outfile)

plot(c(log10(x.min), log10(x.min)),
     c(-5, 0),
     type="l",
     lty=3,main=paste("CCDF ", trip.mode),
#     xlim=c(1.5,3),  # zoom in
     xlim=c(0,3), # zoom out
#     xlim=c(log10(x.min),3),  # start at xmin
#     ylim=c(-5,-1), # zoom in
     ylim=c(-5,0), # zoom out
     xlab= expression(log[10](r)),
     ylab= expression(log[10](p(r))))

# label xmin location
text(log10(x.min), -5, substitute(log(x[min]) == xminval, list(xminval =  round(log10(x.min), digits=2))))

############ PLOT CCDF OF SYNTHETIC DATA FROM R CODE ##########

# BELOW AND ABOVE XMIN (FROM SOURCE CODE)

synth.sets <- synthdata(puf.trip.lengths, x.min)

print(length(synth.sets))

colors.set <- c("red", "orange", "yellow", "green", "blue", "black")

colors.index <- 1

for (synth.set in synth.sets) {

  
  synth.trip.lengths <- unlist(synth.set)



# remove values greater than puf.max.trip.length
  synth.trip.lengths <- subset(synth.trip.lengths,
                               synth.trip.lengths <= puf.max.trip.length & synth.trip.lengths > 0)


  cdf.synth.hist <- ecdf(synth.trip.lengths)

  ccdf <- 1 - cdf.synth.hist(x.values)


  log10.ccdf <- log10(ccdf)



# just try plotting CCDF
  points(log10.xvals,
         log10.ccdf,
         type="l",
         col=colors.set[colors.index])

  colors.index <- colors.index + 1
}

############  EMPIRICAL CCDF ################

puf.hist <- hist(puf.trip.lengths,
                 breaks = breaks.vector,	
                 plot = F)

# get histogram
#cdf.puf.hist <- ecdf(puf.hist$counts)

cdf.puf.hist <- ecdf(puf.trip.lengths)


ccdf <- 1 - cdf.puf.hist(x.values)

log10.xvals <- log10(x.values)
log10.ccdf <- log10(ccdf)

log10.ecdf <- log10(cdf.puf.hist(x.values))

#plot(log10.xvals,
lines(log10.xvals,
      log10.ccdf,
      lwd=1.5)


############### PLOT CCDF OF BEST FIT FUNCTION ###############

# using vector of x.values, compute P(X > x) as vector

# P(x) = ((x/x_min)^(1-alpha))
P.X <- ((x.values/x.min)^(1-alpha))

cdf.at.xmin <- cdf.puf.hist(x.min)
ccdf.at.xmin <- 1 - cdf.at.xmin
ccdf.at.xmin

log.ccdf.at.xmin <- log10(ccdf.at.xmin)
log.ccdf.at.xmin

# plot CCDF
log10.x.vals <- log10(x.values) 
log10.P.X <- log10(P.X) + log.ccdf.at.xmin

lines(log10.x.vals,
      log10.P.X,
      type = "l",
      lty = 2,
      lwd = 1.5)

# label alpha on fit line
text(log10(x.min) - .3, log.ccdf.at.xmin, substitute(alpha == alphval, list(alphval = round(alpha, digits = 2))))

label1 <- paste("xmin = ", round(x.min,digits=2), " alpha = ", round(alpha,digits=2))
legend.labels <- c(label1,"synthetic cdf 1", "synthetic cdf 2","synthetic cdf 3","synthetic cdf 4","synthetic cdf 5","empirical cdf")

legend.colors <- c("black",colors.set)
legend("topright",legend.labels ,lty=c(2,1,1,1,1,1,1), lwd=c(1,1,1,1,1,1,1.5), col=legend.colors, cex = .75, inset = .01)

dev.off()

}

## ################## FITTING #################


## # FOR EACH SYNTHETIC DATA SET, FIND XMIN, ALPHA, P-VALUE

## index <- 1

## for (synth.set in synth.sets) {

##   synth.trip.lengths <- unlist(synth.set)
  
## ## # find xmin, alpha
##   print("fitting")
##   params <- plfit(synth.trip.lengths)

## ## warnings()
  
##   num.trips <- length(synth.trip.lengths)
##   x.min <- as.numeric(params[1])
##   alpha <- as.numeric(params[2])

##   print(paste("synthetic set #", index))
##   print(paste("num.trips", num.trips))
##   print(paste("x.min", x.min))
##   print(paste("alpha", alpha))

## ## ## ## # find p value
##   print("p-value")
##   p.params <- plpva(synth.trip.lengths, x.min)

## ## warnings()

  
##   p.val <- as.numeric(p.params[1])
##   D <- as.numeric(p.params[2])

##   print(p.val)
##   print(D)

## ## outframe <- data.frame("PUF", travel.mode, num.trips, x.min, alpha, p.val, D)

## ## # save as .csv
## ## write.csv(outframe, file = paste(base.data.dir,"puf_", travel.mode,"_hvm_trip&travel_length_fits_sample_",sample.size,".csv", sep=""))

##   index <- index + 1
## } # end for loop

rm(list = ls())
