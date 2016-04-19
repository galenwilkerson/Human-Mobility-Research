# power law with cutoff

# generate some exponentials
exps <- rexp(20000, rate = 10)

xmin <- 100
alpha <- 2.5

# get p, probability of accepting
ps <- (exps/xmin)^(-alpha)

rs <- runif(20000)

x <- c()

i <- 1
while(length(x) < 10000) {
  
  if (rs[i] < ps[i])
    append(x, exps[i])

  i <- i + 1
}
                                        
p.hist <- hist(x, breaks = 100, main="lin-lin hist powerlaw with exponential cutoff")

x.vals <- log10(p.hist$breaks[-1])
y.vals <- log10(p.hist$density)
plot(x.vals, y.vals, main="log-log pdf powerlaw with exponential cutoff")

p.cdf <- ecdf(x)
ccdf <- 1 - p.cdf(p$breaks[-1])
log.ccdf <- log10(ccdf)

plot(x.vals, log.ccdf, type="s", main="log-log ccdf powerlaw with exponential cutoff")
