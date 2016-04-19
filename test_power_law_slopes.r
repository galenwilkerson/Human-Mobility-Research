# plot pdfs and cdfs of power laws with different slopes

# set alpha = 1.5, alpha = 2.5

# plot power law function  x^-alpha



x <- 1:100
log10.x <- log10(x)

alpha <- 1.5
y <- x^(-alpha)
log10.y <- log10(y)
plot(x,y)

cdf <- ecdf(y)

plot(cdf)

my.cdf <- cdf(x)

plot(my.cdf)


plot(log10.x, log10.y, col = 2)



# find "ccdf"



alpha <- 2.5
y <- x^(-alpha)
log10.y <- log10(y)
points(log10(x), log10(x^(-alpha)), col = 3)




