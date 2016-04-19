#rlnorm(n, meanlog = 0, sdlog = 1)
x <- rlnorm(10000, meanlog = 0, sdlog = 0.05)


p <- hist(x, breaks = 100, main="lin-lin hist lognormal")

x.vals <- log(p$breaks[-1])
y.vals <- log(p$density)
plot(x.vals, y.vals, main="log-log pdf lognormal")

p.cdf <- ecdf(x)
ccdf <- 1 - p.cdf(p$breaks[-1])
log.ccdf <- log(ccdf)

plot(x.vals, log.ccdf, type="s", main="log-log ccdf lognormal")
