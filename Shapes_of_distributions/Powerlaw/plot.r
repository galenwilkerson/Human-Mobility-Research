# power law

r <- runif(10000)
xmin <- 100
alpha <- 2.5

x <- xmin*(1-r)^(-1/(alpha - 1))
                                        
p <- hist(x, breaks = 100, main="lin-lin hist powerlaw")

x.vals <- log10(p$breaks[-1])
y.vals <- log10(p$density)
plot(x.vals, y.vals, main="log-log pdf powerlaw")

p.cdf <- ecdf(x)
ccdf <- 1 - p.cdf(p$breaks[-1])
log.ccdf <- log10(ccdf)

plot(x.vals, log.ccdf, type="s", main="log-log ccdf powerlaw")
