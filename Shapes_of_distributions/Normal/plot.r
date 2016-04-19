#rnorm(n, mean = 0, sd = 1)
x <- rnorm(10000, mean = 0, sd = 1)
                                        
p <- hist(x, breaks = 100, main="lin-lin hist normal (0,1)")

x.vals <- log(p$breaks[-1])
y.vals <- log(p$density)
plot(x.vals, y.vals, xlim =c(-2,5), ylim = c(-10,0), main="log-log pdf normal")

p.cdf <- ecdf(x)
ccdf <- 1 - p.cdf(p$breaks[-1])
log.ccdf <- log(ccdf)

plot(x.vals, log.ccdf, type="s", xlim =c(-2,5), ylim = c(-10,0), main="log-log ccdf normal")



x <- rnorm(10000, mean = 0, sd = 10)
                                        
p <- hist(x, breaks = 100, main="lin-lin hist normal (0,10)")

x.vals <- log(p$breaks[-1])
y.vals <- log(p$density)
plot(x.vals, y.vals,xlim =c(-2,5), ylim = c(-10,0), main="log-log pdf normal")

p.cdf <- ecdf(x)
ccdf <- 1 - p.cdf(p$breaks[-1])
log.ccdf <- log(ccdf)

plot(x.vals, log.ccdf, type="s", xlim =c(-2,5), ylim = c(-10,0), main="log-log ccdf normal")
