
library(VGAM)
library(MASS)
library(fitdistrplus)


#pdf('out.pdf')

############## WEIBULL #############

# generate weibull data
x <- rweibull(100000,1, 0.5)

# normalize
x <- x/mean(x)

summary(x)

histo <- hist(x, main="weibull")


log10.breaks <- log10(histo$breaks)
log10.density <- log10(histo$density)
plot(log10.breaks[-1], log10.density, type = "s", main="log-log weibull")

ccdf <- 1 - ecdf(x)(histo$breaks[-1])
plot(log10.breaks[-1], log10(ccdf), type = "s", main="log-log weibull CDF")

# fit to pareto
# (11) Fit of a Pareto distribution by numerical moment matching estimation

#empirical raw moment
memp <- function(x, order)
  ifelse(order == 1, mean(x), sum(x^order)/length(x))

#fit
fP <- fitdist(x, "pareto", start=c(shape=10, scale=10))
#, method="mme", order=c(1, 2), memp="memp",
#              start=c(shape=10, scale=10), lower=1, upper=Inf)
summary(fP)
#plot(fP)

gofstat(fP)





fP <- fitdist(x, "pareto", start=c(shape=1, scale=1), lower=1, upper=Inf)
summary(fP)

gofstat(fP)


# fit to weibull


fW <- fitdist(x, "weibull",  start=c(shape=1, scale=1), lower=1, upper=Inf)
summary(fW)

gofstat(fW)




############## PARETO ##############

#generate pareto data
x <- rpareto(100000,2,1)

# normalize
x <- x/mean(x)

summary(x)

histo <- hist(x, main="pareto")

log10.breaks <- log10(histo$breaks)
log10.density <- log10(histo$density)
plot(log10.breaks[-1], log10.density, type = "s", main="log-log pareto")

ccdf <- 1 - ecdf(x)(histo$breaks[-1])
plot(log10.breaks[-1], log10(ccdf), type = "s", main="log-log weibull CDF")


# fit to weibull
fW <- fitdist(x, "weibull",  start=c(shape=1, scale=1), lower=1, upper=Inf)
summary(fW)

gofstat(fW)

# fit to pareto
fP <- fitdist(x, "pareto", start=c(shape=1, scale=1), lower=1, upper=Inf)
summary(fP)

gofstat(fP)


dev.off()


rm(list = ls())
