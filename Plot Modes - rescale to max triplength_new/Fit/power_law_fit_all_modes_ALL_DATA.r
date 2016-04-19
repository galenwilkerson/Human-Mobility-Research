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
require("grid")
require(scales)

# plfit code
source("plfit.r")
source("plpva.r")

# base directory
base.data.dir <- "../Data/"
figures.dir <- "../Figures/"

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



city.types <- unique(mergedtrips$sgtyp)

print(city.types)

# just use big city trips
city.trips <- subset(mergedtrips, sgtyp == city.types[3])


#colors.set <- c("black", "orange", "red", "green", "blue", "purple")
#colors.set <- c("black", "red", "orange", "green", "purple", "blue")


#color.plot <- c("black","red", "orange", "yellow", "green", "blue", "purple")
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
puf.trip.lengths <- subset(city.trips, wegkm.k <= puf.max.trip.length & wegkm.k > 0, select=c(wegkm.k))[,1]
#puf.trip.lengths <- subset(mergedtrips, wegkm.k <= puf.max.trip.length & wegkm.k > 0, select=c(wegkm.k))[,1]


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


# prep for plotting
df <- data.frame()
best.fit.df <- data.frame()

ifelse(trip.mode == "all_modes",
       line.size <- 3,
       line.size <- 2)

ifelse(trip.mode == "all_modes",
       line.alpha <- 1,
       line.alpha <- .6)


#df <- rbind(df, data.frame(x.vals.log10.scale, ccdf.log10.scale, trip.mode, line.size, line.alpha))

################### FIT AND KEEP FIT INFO

results <- data.frame()

print(trip.mode)
print("number of trips")
number.of.trips <- length(puf.trip.lengths.rescaled)
print(length(puf.trip.lengths.rescaled))
print(summary(puf.trip.lengths.rescaled))
av <- mean(puf.trip.lengths.rescaled)
print("sd:")
sd1 <- sd(puf.trip.lengths.rescaled)
print(sd(puf.trip.lengths.rescaled))
v1 <- (var(puf.trip.lengths.rescaled, use = "everything"))
print("var:")
print(var(puf.trip.lengths.rescaled, use = "everything"))


## find xmin, alpha
print("fitting")
#params <- plfit(puf.trip.lengths.rescaled,"limit",3.00)
params <- plfit(puf.trip.lengths.rescaled)

num.trips <- length(puf.trip.lengths.rescaled)
x.min <- as.numeric(params[1])
alpha <- as.numeric(params[2])

print(num.trips)
print(x.min)
print(alpha)

results <- rbind(results, data.frame(trip.mode, number.of.trips, alpha, x.min, av, v1))



############### PLOT CCDF OF BEST FIT FUNCTION ###############

# use hist for finding breaks
    h.inter <- hist(puf.trip.lengths.rescaled, breaks = 100000, plot = F)
    
# find xvalues, log x values, log ccdf
    my.breaks <-  h.inter$breaks
    trip.lengths <- my.breaks[-1]
    log10.trip.lengths <- log10(trip.lengths)

# find CDF, use for CCDF, log CCDF
    cdf <- ecdf(puf.trip.lengths.rescaled)
    CDF <- cdf(trip.lengths)
    CCDF1 <- 1 - CDF
    log10.ccdf <- log10(CCDF1)


df <- rbind(df, data.frame(log10.trip.lengths, log10.ccdf, trip.mode, line.size, line.alpha))

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

best.fit.df <- rbind(best.fit.df, data.frame(log10.trip.lengths, log10.P.X, trip.mode))

## # new plot or use old plot
## plot(x.vals.log10.scale,
## #     puf.hist,
## #     cdf,
##      ccdf.log10.scale,
## #     log="xy",
##      type = "s",
##      col=color.plot[color.index],
##      main="trip length normalized by max trip length")



color.index <- color.index + 1




################## REST OF MODES x##################

# remove KEINE ANGABE
trip.modes <- trip.modes[-length(trip.modes)]



# for now, remove passengers
trip.modes <- trip.modes[-4]


# includes "all modes", but without "keine angabe", and without passengers if removed
trip.modes.all.good <- trip.modes

# remove "all modes"
trip.modes <- trip.modes[-1]

for (trip.mode in trip.modes) {

print(trip.mode)

############# PREPARATION ############


puf.trip.lengths <- subset(city.trips, hvm == trip.mode & wegkm.k <= puf.max.trip.length & wegkm.k > 0, select=c(wegkm.k))[,1]

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

# add data for plotting
#df <- rbind(df, x.vals.log10.scale, ccdf.log10.scale, trip.mode)


ifelse(trip.mode == "all_modes",
       line.size <- 3,
       line.size <- 2)

ifelse(trip.mode == "all_modes",
       line.alpha <- 1,
       line.alpha <- .6)

#df <- rbind(df, data.frame(x.vals.log10.scale, ccdf.log10.scale, trip.mode, line.size, line.alpha))

################### FIT AND KEEP FIT INFO



print(trip.mode)
print("number of trips")
number.of.trips <- length(puf.trip.lengths.rescaled)
print(length(puf.trip.lengths.rescaled))
print(summary(puf.trip.lengths.rescaled))
av <- mean(puf.trip.lengths.rescaled)
print("sd:")
sd1 <- sd(puf.trip.lengths.rescaled)
print(sd(puf.trip.lengths.rescaled))
v1 <- (var(puf.trip.lengths.rescaled, use = "everything"))
print("var:")
print(var(puf.trip.lengths.rescaled, use = "everything"))


## find xmin, alpha
print("fitting")
#params <- plfit(puf.trip.lengths.rescaled,"limit",3.00)
params <- plfit(puf.trip.lengths.rescaled)

num.trips <- length(puf.trip.lengths.rescaled)
x.min <- as.numeric(params[1])
alpha <- as.numeric(params[2])

print(num.trips)
print(x.min)
print(alpha)

results <- rbind(results, data.frame(trip.mode, number.of.trips, alpha, x.min, av, v1))
                 
## print(trip.mode)
## print("number of trips")
## print(length(puf.trip.lengths.rescaled))
## print(summary(puf.trip.lengths.rescaled))
## print("sd:")
## print(sd(puf.trip.lengths.rescaled))
## print("var:")
## print(var(puf.trip.lengths.rescaled, use = "everything"))


## ## find xmin, alpha
## print("fitting")
## #params <- plfit(puf.trip.lengths.rescaled,"limit",3.00)
## params <- plfit(puf.trip.lengths.rescaled)

## num.trips <- length(puf.trip.lengths.rescaled)
## x.min <- as.numeric(params[1])
## alpha <- as.numeric(params[2])

## print(num.trips)
## print(x.min)
## print(alpha)


############### PLOT CCDF OF BEST FIT FUNCTION ###############

# use hist for finding breaks
    h.inter <- hist(puf.trip.lengths.rescaled, breaks = 100000, plot = F)
    
# find xvalues, log x values, log ccdf
    my.breaks <-  h.inter$breaks
    trip.lengths <- my.breaks[-1]
    log10.trip.lengths <- log10(trip.lengths)

# find CDF, use for CCDF, log CCDF
    cdf <- ecdf(puf.trip.lengths.rescaled)
    CDF <- cdf(trip.lengths)
    CCDF1 <- 1 - CDF
    log10.ccdf <- log10(CCDF1)



df <- rbind(df, data.frame(log10.trip.lengths, log10.ccdf, trip.mode, line.size, line.alpha))

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

best.fit.df <- rbind(best.fit.df, data.frame(log10.trip.lengths, log10.P.X, trip.mode))



color.index <- color.index + 1

} # end for loop



# save results

write.csv(results, "fit_results.csv")


############## PLOT

#legend(-2.5,-3,trip.modes,lty=c(1), col=color.plot)

#qplot(x.vals.log10.scale, ccdf.log10.scale, data = df, colour = trip.mode, geom="line")

# clean up small CCDF values
#df <- subset(df, ccdf.log10.scale > -Inf)

# combine dfs for plot ease
new.df <- data.frame(df, best.fit.df)

#df <- subset(df, ccdf.log10.scale > -Inf && ccdf.log10.scale < Inf && x.vals.log10.scale > -Inf && x.vals.log10.scale < Inf)
#new.df <- subset(new.df, log10.ccdf > -Inf && log10.trip.lengths > 0)
new.df <- subset(new.df, log10.ccdf > -Inf)

#legend.labels <- c("I. All Modes", "A. Walking",  "C. Automobile Driver", "D. Passenger",  "B. Bicycling", "E. Public Transport")
#legend.labels <- c("I. All Modes", "A. Automobile Driver", "D. Passenger",  "B. Bicycling", "E. Public Transport")
legend.labels <- c("I. All Modes", "C. Automobile Driver", "A. Walking", "D. Public Transport", "B. Bicycling")

colors.set <- c("black", "blue", "red", "#993300", "#006633")


df.all.modes <- subset(new.df, trip.mode == "all_modes")
df.only.modes <- subset(new.df, trip.mode != "all_modes")



# convert axes labels to 10^x
 scientific_10 <- function(x) {
   parse(text = gsub("1e", "10^", scientific_format()(10^x)))
 }

plot1 <-ggplot(NULL) +

   ## # draw fit line
#  geom_line(data = best.fit.df, aes(x = x.vals.log10.scale, y = log10.P.X, colour = trip.mode, group=trip.mode), linetype = 2, size = 1.5) +
  geom_line(data = df.only.modes, aes(x = log10.trip.lengths, y = log10.P.X, colour = trip.mode, group=trip.mode), linetype = 2, size = .5, alpha = .7) +
  geom_line(data = df.all.modes, aes(x = log10.trip.lengths, y = log10.P.X, colour = trip.mode, group=trip.mode), linetype = 2, size = .5, alpha = .7) +

  # plot the data
  # rescale the line based on data frame entry "line.size"
  # rescale alpha value based on "line.alpha"
#  geom_line(data = df, aes(x=x.vals.log10.scale, y=ccdf.log10.scale, colour=trip.mode, group=trip.mode, size = line.size, alpha = line.alpha)) +
  geom_line(data = df.only.modes, aes(x=log10.trip.lengths, y=log10.ccdf, colour=trip.mode, group=trip.mode, size = line.size, alpha = line.alpha)) +
  geom_line(data = df.all.modes, aes(x=log10.trip.lengths, y=log10.ccdf, colour=trip.mode, group=trip.mode, size = line.size, alpha = line.alpha)) +
  # the transparency values
  scale_alpha(range = c(0.7,1), guide=FALSE) +
  # the size of the lines
  scale_size(range=c(2, 3), guide=FALSE) +

  # make grid light grey
  theme_bw() +

  # legend location
  theme(legend.position=c(0.32,0.25)) +

  # scale axis text
  theme(axis.text.x  = element_text(size=30)) + 
  theme(axis.text.y  = element_text(size=30)) +

  #color and labels of legend
  scale_colour_manual(values=colors.set,
                      name="Mode",
                      breaks=trip.modes.all.good,
#                      breaks = order,

                      labels=legend.labels) +

  # size of legend text and titles
  theme(legend.text=element_text(size=30)) +
  theme(legend.title=element_text(size=30)) +

  # remove grey boxes
  theme(legend.key = element_blank()) +
  
  # vertical spacing of legend
  theme(legend.key.height=unit(2,"line")) +

 # line thickness
  guides(colour = guide_legend(override.aes = list(size=line.size))) +
  
  
  # axis text size
  theme(axis.title.x=element_text(size=30)) +
  theme(axis.title.y=element_text(size=30)) +

  # axis labels
  xlab("Rescaled Length") +
  ylab("CCDF") +

  
  # axes as 10^x
  scale_x_continuous(label=scientific_10, limits=c(-3, 0)) +
  scale_y_continuous(label=scientific_10, limits=c(-4, 0)) # +

 

#  annotate("text", x = 1.15, y = -1.1, label = "I", size=9) +
#  annotate("text", x = 1.4, y = -3.8, label = "A", size=9) +
#  annotate("text", x = 2.1, y = -3.5, label = "B", size=9) +
#  annotate("text", x = 2.55, y = -2.9, label = "C", size=9) +
#  annotate("text", x = 2, y = -1.3, label = "D", size=9)# +
  ## annotate("text", x = 2.95, y = -3.2, label = "E", size=9)

plot1 

outfile <- paste(figures.dir, "rescaled_trip_length.pdf" , sep = "_")

print(outfile)
ggsave(outfile, width = 240, height = 210, units = "mm")


       
rm(list = ls())
