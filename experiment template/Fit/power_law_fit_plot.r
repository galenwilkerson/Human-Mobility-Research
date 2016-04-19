# calculate the waiting times, check against calculated values


# get and plot several synthetic data sets (from plpva)
# also plot actual empirical data

# and best fit function

### # save sythetic data set (NOT NOW)


# also save X_Dmax and Dmax for each synthetic data set, as well as size of synthetic data

# plot Dmax vs. iteration (synthetic set)

# p(Dmax)
# p(X_Dmax)

# scatterplot (X_Dmax, Dmax)

###############################

#
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

# number of synthetic sets to use (very slow, so not much more than 100!!)
num.synth.sets <- 100

# load trip data
load(paste(base.data.dir,"merged_trips.RData", sep=""))

# separate trips into hvm mode

# get uniq list of modes
trip.modes <- as.vector(unique(mergedtrips$hvm))

# add "all modes" to modes
trip.modes <- c("all_modes", trip.modes)

print(trip.modes)

#for (trip.mode in trip.modes) {
trip.mode <- trip.modes[3]

  print(trip.mode)

############# PREPARATION ############

  puf.max.trip.length <- 950

  ifelse(trip.mode == "all_modes",
         puf.trip.lengths <- subset(mergedtrips, wegkm.k <= puf.max.trip.length & wegkm.k > 0, select=c(wegkm.k))[,1],
         puf.trip.lengths <- subset(mergedtrips, hvm == trip.mode & wegkm.k <= puf.max.trip.length & wegkm.k > 0, select=c(wegkm.k))[,1])

  puf.trip.lengths <- na.omit(puf.trip.lengths)

  data.size <- length(puf.trip.lengths)

  print("number of trips")
  print(data.size)

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

#################  Draw vertical line at xmin ###############

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


#################### plot the synthetic D values ####################

# BELOW AND ABOVE XMIN (FROM SOURCE CODE)

  synth.results <- synthdata(puf.trip.lengths, x.min, num.synth.sets = num.synth.sets)

  gof.synth <- unlist(synth.results[1])
  synth.sets <- unlist(synth.results[2])

  gof.synth <- gof.synth[1:num.synth.sets]
  synth.sets <- synth.sets[1:num.synth.sets]

  gof.hist <- hist(gof.synth, breaks = 100, main=trip.mode)
  
  plot(synth.results$count, log="xy")


############ PLOT CCDF OF SYNTHETIC DATA FROM R CODE ##########


  print(length(synth.sets))

  for (synth.set in synth.sets) {

    
    synth.trip.lengths <- unlist(synth.set)

    synth.size <- length(synth.trip.lengths)

# remove values greater than puf.max.trip.length
    synth.trip.lengths <- subset(synth.trip.lengths,
                                 synth.trip.lengths <= puf.max.trip.length & synth.trip.lengths > 0)


    cdf.synth <- ecdf(synth.trip.lengths)

    ccdf <- 1 - cdf.synth(x.values)


    log10.ccdf <- log10(ccdf)



# just try plotting CCDF
    points(log10.xvals,
           log10.ccdf,
           type="l",
           col="grey")

  }




############  EMPIRICAL CCDF ################

  cdf.puf <- ecdf(puf.trip.lengths)
  ccdf <- 1 - cdf.puf(x.values)

  log10.xvals <- log10(x.values)
  log10.ccdf <- log10(ccdf)

#plot(log10.xvals,
  lines(log10.xvals,
        log10.ccdf,
        lwd=1.5)


############### PLOT CCDF OF BEST FIT FUNCTION ###############

# using vector of x.values, compute P(X > x) as vector

# P(x) = ((x/x_min)^(1-alpha))
  P.X <- ((x.values/x.min)^(1-alpha))

  cdf.at.xmin <- cdf.puf(x.min)
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
        lwd = 1.5,
        col="red")

# label alpha on fit line
  text(log10(x.min) - .3, log.ccdf.at.xmin, substitute(alpha == alphval, list(alphval = round(alpha, digits = 2))))

  label1 <- paste("xmin = ", round(x.min,digits=2), " alpha = ", round(alpha,digits=2))
  legend.labels <- c(label1, paste(num.synth.sets, " synthetic cdfs", sep=""), "empirical cdf")

  legend.colors <- c("red", "grey", "black")
  legend("topright",legend.labels ,lty=c(2,1,1), lwd=2, col=legend.colors, cex = .75, inset = .01)

  dev.off()

#}


rm(list = ls())
