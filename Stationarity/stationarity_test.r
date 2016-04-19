# load data

# convert to timeseries

# test unit root
# test if stationary



library(tseries)
library(zoo)


## data.dir = "/home/username/Dropbox/Active_Projects/Mobility_Article/Data/"
## figures.dir = "./Figures/"

## load(paste(data.dir, "trips.Rdata", sep=""))



## breaks.interval <- "min"

## my.table <- table(cut(trips$timestamp.event, breaks = breaks.interval))

#trips.ts <- xts(trips$timestamp.event)
## trips.ts <- ts(my.table)



## gnp <- ts(cumsum(1 + round(rnorm(100), 2)),
##                start = c(1954, 7), frequency = 12)

## plot(gnp) # using 'plot.ts' for time-series plot


t1 <- ts(1:1000, frequency = 4, start = c(1959, 2)) # 2nd Quarter of 1959

print( ts(1:10, frequency = 7, start = c(12, 2)), calendar = TRUE)


