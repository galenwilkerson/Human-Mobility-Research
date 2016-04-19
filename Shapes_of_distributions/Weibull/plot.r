x <- rweibull(10000, shape=0.5, scale = 1)

p <- hist(x, breaks = 100, main="lin-lin hist weibull")

plot(p$breaks[-1], p$density, log="xy", main="log-log pdf weibull")

p.cdf <- ecdf(x)
plot(1-p.cdf(p$breaks), log="xy", type="s", main="log-log ccdf weibull")
