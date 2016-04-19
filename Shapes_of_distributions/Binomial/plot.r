#rbinom(n, size, prob)
                                        
x <- rbinom(10000, 20, 0.5)
                                        
p <- hist(x, breaks = 100, main="lin-lin hist binomial")

x.vals <- log(p$breaks[-1])
y.vals <- log(p$density)
plot(x.vals, y.vals, main="log-log pdf binomial")

p.cdf <- ecdf(x)
ccdf <- 1 - p.cdf(p$breaks[-1])
log.ccdf <- log(ccdf)

plot(x.vals, log.ccdf, type="s", main="log-log ccdf binomial")
