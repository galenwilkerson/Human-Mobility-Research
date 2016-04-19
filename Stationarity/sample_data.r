# generate sample time series

# - one stationary, one not

# - use KPSS test to see if stationary



#zardoz = ts(rnorm(48), start=c(2163,6), frequency=12)

ts1 <- c(0,1,0,1,0,1)
summary(ts1)

ts2 <- c(1,2,3,1,2,3)
summary(ts2)

library(tseries)
 kpss.test(ts1)


 kpss.test(ts2)

ts3 <- c(ts1, ts2)
summary(ts3)

 kpss.test(ts3)


