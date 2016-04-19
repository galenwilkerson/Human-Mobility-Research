
# load csv data
# read.csv(file, header = TRUE, sep = ",")
#file <- "../synth_results/all_modes_synth_fit_results.csv"
file <- "../synth_results/Fahrrad_synth_fit_results.csv"

dfr <- read.csv(file, header = FALSE, sep = " ")
names(dfr)

## Continuous MLE
## alpha =      6.36082
## xmin  =      1.20000
## L     =      0.93077
## D     =      0.22903
## p     =      0.99751 (approximation)

colnames(dfr) <- c("file", "cont.or disc.", "alpha", "xmin", "L", "D", "p")

names(dfr)

x <- dfr$p

summary(x)

# plot the p-values
h <- hist(x, breaks = 100, plot=FALSE)

plot(h$breaks[-1],h$count, xlab="p-value", ylab="count", main="p-value distribution")#, log="xy")
#plot(h, xlim = c(0,1), xlab="p-value", ylab="count", main="p-value distribution, zu Fuss")
