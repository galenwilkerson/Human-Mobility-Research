
# read in cities

# scatter plot area against population

library(ggplot2)

cities <- read.csv("german_cities.csv", sep = ";")

# get rid of bad vals

cities <- subset(cities, area < 25000)

small.cities <- subset(cities, population < 30000)

medium.cities <- subset(cities, population <= 100000)
medium.cities <- subset(medium.cities, population >= 30000)


big.cities <- subset(cities, population > 100000)

qplot(area, population, data = cities)
cor(cities$area, cities$population, use = "everything", method = c("pearson"))


qplot(area, population, data = small.cities) +
  theme_bw() 
cor(small.cities$area, small.cities$population, use = "everything", method = c("pearson"))


qplot(area, population, data = medium.cities) +
  theme_bw() 
cor(medium.cities$area, medium.cities$population, use = "everything", method = c("pearson"))

qplot(area, population, data = big.cities) +
  theme_bw()
cor(big.cities$area, big.cities$population, use = "everything", method = c("pearson"))



                                        #cor(x, y = NULL, use = "everything",
#          method = c("pearson", "kendall", "spearman"))
     


qplot(area, data = cities)

hist(cities$area)
