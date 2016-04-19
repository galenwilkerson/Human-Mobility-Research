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
library("grid")
library("scales")


# plfit code
source("plfit.r")


# base directory
base.data.dir = "/home/username/Dropbox/Active_Projects/Mobility_Article/Data/"
figures.dir = "../Figures/"


# load travel data
travel.trips <- read.dta(paste(base.data.dir, "MiD2008_PUF_Reisen.dta", sep=""))

# load trip data
load(paste(base.data.dir,"trips.Rdata", sep=""))


puf.max.travel.length <- 20000

puf.max.trip.length <- 950


#############################  PREPARATION for ALL_MODES

# subset reisen
puf.travel.lengths <- subset(travel.trips, p1016 <= puf.max.travel.length & p1016 > 0, select=c(p1016))[,1]
puf.travel.lengths <- na.omit(puf.travel.lengths)

# subset trips
puf.trip.lengths <- subset(trips, wegkm.k <= puf.max.trip.length & wegkm.k > 0, select=c(wegkm.k))[,1]
puf.trip.lengths <- na.omit(puf.trip.lengths)

trip.and.travel.lengths <- c(puf.trip.lengths, puf.travel.lengths)


summary(trip.and.travel.lengths)

#################### fit travels and trips


results <- data.frame()

germany.area <- 357021
germany.diameter <- 2 * sqrt(germany.area/pi)
long.min <- 8667

## find xmin, alpha for SHORT lengths

print("fitting SHORT trips and travel")
params.short <- plfit(trip.and.travel.lengths)

short.trips <- subset(trip.and.travel.lengths, trip.and.travel.lengths <= germany.diameter)

num.trips.short <- length(short.trips)
x.min.short <- as.numeric(params.short[1])
alpha.short <- as.numeric(params.short[2])

print(num.trips.short)
print(x.min.short)
print(alpha.short)


cat.val <- "short"
num.trips <- num.trips.short
alpha <- alpha.short
x.min <- x.min.short
av <- mean(short.trips)
v1 <- var(short.trips)
           
results <- rbind(results, data.frame(cat.val, num.trips, alpha, x.min, av, v1))


## find xmin, alpha for LONG lengths

print("fitting LONG trips and travel")


# start fitting at diameter of germany
params.long <- plfit(trip.and.travel.lengths, "limit", germany.diameter)

long.trips <- subset(trip.and.travel.lengths, trip.and.travel.lengths > germany.diameter & trip.and.travel.lengths <= long.min)

# lt <- subset(travel.trips, p1016 <= puf.max.travel.length & p1016 > long.min)

num.trips.long <- length(long.trips)
x.min.long <- as.numeric(params.long[1])
alpha.long <- as.numeric(params.long[2])


print(num.trips.long)
print(x.min.long)
print(alpha.long)


cat.val <- "long"
num.trips <- num.trips.long
alpha <- alpha.long
x.min <- x.min.long
av <- mean(long.trips)
v1 <- var(long.trips)
           
results <- rbind(results, data.frame(cat.val, num.trips, alpha, x.min, av, v1))








## find xmin, alpha for VERY_LONG lengths

print("fitting VERY.LONG trips and travel")





# start fitting at diameter of germany
params.very.long <- plfit(trip.and.travel.lengths, "limit", long.min)

very.long.trips <- subset(trip.and.travel.lengths, trip.and.travel.lengths > long.min)

num.trips.very.long <- length(very.long.trips)
x.min.very.long <- as.numeric(params.very.long[1])
alpha.very.long <- as.numeric(params.very.long[2])

print(num.trips.very.long)
print(x.min.very.long)
print(alpha.very.long)



cat.val <- "very long"
num.trips <- num.trips.very.long
alpha <- alpha.very.long
x.min <- x.min.very.long
av <- mean(very.long.trips)
v1 <- var(very.long.trips)
           
results <- rbind(results, data.frame(cat.val, num.trips, alpha, x.min, av, v1))


###############  EMPIRICAL CCDF ################


# use hist for finding breaks
h.inter <- hist(trip.and.travel.lengths, breaks = 100000, plot = F)
    
# find xvalues, log x values, log ccdf
my.breaks <-  h.inter$breaks
all.lengths <- my.breaks[-1]
log10.all.lengths <- log10(all.lengths)

## # find CDF, use for CCDF, log CCDF
cdf <- ecdf(trip.and.travel.lengths)
CDF <- cdf(all.lengths)
CCDF1 <- 1 - CDF
log10.ccdf <- log10(CCDF1)

df <- data.frame()
df <- rbind(df, data.frame(log10.all.lengths, log10.ccdf))


# clean up small CCDF values
df <- subset(df, log10.ccdf > -Inf)



############### PLOT CCDF OF BEST FIT FUNCTION ###############

# using vector of all.lengths, compute P(X > x) as vector

log10.all.lengths <- log10(all.lengths)

# SHORT ###########

# P(x) = ((x/x_min)^(1-alpha.short))
P.X.short <- ((all.lengths/x.min.short)^(1-alpha.short))

cdf.at.xmin <- cdf(x.min.short)
ccdf.at.xmin <- 1 - cdf.at.xmin
#ccdf.at.xmin

log.ccdf.at.xmin <- log10(ccdf.at.xmin)
#log.ccdf.at.xmin

# plot CCDF
log10.P.X.short <- log10(P.X.short) + log.ccdf.at.xmin

#best.fit.df <- rbind(best.fit.df, data.frame(log10.all.lengths, log10.P.X.short, cat.val, order))



log10.section.lengths <- log10.all.lengths
log10.P.X.section <- log10.P.X.short

best.fit.df.short <- data.frame()
best.fit.df.short <- rbind(best.fit.df.short, data.frame(log10.section.lengths, log10.P.X.section))







# LONG ############

# P(x) = ((x/x_min)^(1-alpha.long))
P.X.long <- ((all.lengths/x.min.long)^(1-alpha.long))

cdf.at.xmin <- cdf(x.min.long)
ccdf.at.xmin <- 1 - cdf.at.xmin
#ccdf.at.xmin

log.ccdf.at.xmin <- log10(ccdf.at.xmin)
#log.ccdf.at.xmin

# plot CCDF
log10.P.X.long <- log10(P.X.long) + log.ccdf.at.xmin


log10.section.lengths <- log10.all.lengths
log10.P.X.section <- log10.P.X.long

best.fit.df.long <- data.frame()
best.fit.df.long <- rbind(best.fit.df.long, data.frame(log10.section.lengths, log10.P.X.section))


# LONG ############

# P(x) = ((x/x_min)^(1-alpha.long))
P.X.long <- ((all.lengths/x.min.long)^(1-alpha.long))

cdf.at.xmin <- cdf(x.min.long)
ccdf.at.xmin <- 1 - cdf.at.xmin
#ccdf.at.xmin

log.ccdf.at.xmin <- log10(ccdf.at.xmin)
#log.ccdf.at.xmin

# plot CCDF
log10.P.X.long <- log10(P.X.long) + log.ccdf.at.xmin


log10.section.lengths <- log10.all.lengths
log10.P.X.section <- log10.P.X.long

best.fit.df.long <- data.frame()
best.fit.df.long <- rbind(best.fit.df.long, data.frame(log10.section.lengths, log10.P.X.section))



# VERY.LONG ############

# P(x) = ((x/x_min)^(1-alpha.very.long))
P.X.very.long <- ((all.lengths/x.min.very.long)^(1-alpha.very.long))

cdf.at.xmin <- cdf(x.min.very.long)
ccdf.at.xmin <- 1 - cdf.at.xmin
#ccdf.at.xmin

log.ccdf.at.xmin <- log10(ccdf.at.xmin)
#log.ccdf.at.xmin

# plot CCDF
log10.P.X.very.long <- log10(P.X.very.long) + log.ccdf.at.xmin


log10.section.lengths <- log10.all.lengths
log10.P.X.section <- log10.P.X.very.long

best.fit.df.very.long <- data.frame()
best.fit.df.very.long <- rbind(best.fit.df.very.long, data.frame(log10.section.lengths, log10.P.X.section))






# mean, sd TRIP lengths
print("mean:")
print(mean(puf.trip.lengths))
print("sd:")
print(sd(puf.trip.lengths))

# mean, sd TRAVEL lengths
print("mean:")
print(mean(puf.travel.lengths))
print("sd:")
print(sd(puf.travel.lengths))





# mean, sd
print("mean:")
print(mean(trip.and.travel.lengths))
print("sd:")
print(sd(trip.and.travel.lengths))


# save results
write.csv(results, "fit_results_trips_travel.csv")



# convert axes labels to 10^x
 scientific_10 <- function(x) {
   parse(text = gsub("1e", "10^", scientific_format()(10^x)))
 }

line.size <- 1.5

line.alpha <- .2


################################# BUILD PLOT ###################


plot1 <-ggplot(NULL) +
  # plot the data
  # rescale the line based on data frame entry "line.size"
  # rescale alpha value based on "line.alpha"
#  geom_line(data = df, aes(x=log10.all.lengths, y=log10.ccdf, colour=cat.val, group=cat.val, size = line.size, alpha = line.alpha)) +
  geom_line(data = df, aes(x=log10.all.lengths, y=log10.ccdf, size = line.size, alpha = line.alpha)) +

 # the transparency values
  scale_alpha(range = c(0.7,1), guide=FALSE) +
  # the size of the lines
  scale_size(range=c(2, 3), guide = F) +


#  scale_fill_discrete(name="Trips and Travel") +

  
            
  # make grid light grey
  theme_bw() +

  # legend location
  theme(legend.position=c(0.4, 0.25)) +

  # scale axis text
  theme(axis.text.x  = element_text(size=30)) + 
  theme(axis.text.y  = element_text(size=30)) +


  
  ## # color and labels of legend
  
  ## scale_colour_brewer(palette="Set1",
  ##                     name="Municipality Type",
  ##                     breaks=cat.vals,
  ##                     labels=legend.labels) +

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
  theme(axis.title.x=element_text(size=30)) +
  theme(axis.title.y=element_text(size=30)) +

  # axis labels
  xlab("Length (km)") +
  ylab("CCDF") +

  # axes as 10^x
  scale_x_continuous(label=scientific_10, limits=c(-1, 5)) +
  scale_y_continuous(label=scientific_10, limits=c(-5, 0)) +

  ## # draw fit line
#geom_line(data = best.fit.df, aes(x = log10.all.lengths, y = log10.P.X, colour = cat.val, group=cat.val), linetype = 2, size = 1.5, alpha = line.alpha) #+
#geom_line(data = best.fit.df, aes(x = log10.all.lengths, y = log10.P.X), linetype = 2, size = 1.5, alpha = line.alpha) #+
  geom_line(data = best.fit.df.short, aes(x = log10.section.lengths, y = log10.P.X.section), linetype = 2, size = .5, alpha = .7) +

  geom_line(data = best.fit.df.long, aes(x = log10.section.lengths, y = log10.P.X.section), linetype = 2, size = .5, alpha = .7) +

  geom_line(data = best.fit.df.very.long, aes(x = log10.section.lengths, y = log10.P.X.section), linetype = 2, size = .5, alpha = .7)# +

#annotate("text", x = 1, y = -0.8, label = "alpha == 1.44", parse = T, size = 8) +
#annotate("text", x = 2.7, y = -2.1, label = "alpha == 2.17", parse = T, size = 8) + 
#annotate("text", x = 3.5  , y = -3.5, label = "alpha == 5.91", parse = T, size = 8)

#  annotate("text", x = 1.15, y = -1.1, label =  as.character(round(alpha.short, digits = 2), size=8)) +
#  annotate("text", x = 1.15, y = -1.1, label =  as.character(round(alpha.short, digits = 2), size=8)) +
#  annotate("text", x = 1.4, y = -3.8, label = as.character(round(/home/username/Dropbox/Active_Projects/Mobility_Article/Current Latex and Shared Documents/Experiments/Travels+Trips_new/Fit/alpha.long, digits = 2), size=8))

#expression(paste("Sampled values, ", mu, "=5, ", sigma,    "=1"))

############################# PLOT! ############################

plot1 

outfile <- paste(figures.dir, "trips_and_travel_length.pdf" , sep = "_")
print(outfile)

ggsave(outfile, width = 240, height = 210, units = "mm")




rm(list = ls())
