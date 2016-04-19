# load trips

# for Berlin, Bremen, and Hamburg

# find average trip length

# plot trip length against density


##############


rm(list = ls())
library(ggplot2)

##############

# subset events by category, return list of subset data frames
subset.events.category <- function(events, subset.category, cat.values) {

  subsets <- list()

  i <- 1

  for (cat.val in cat.values) {
    
    # get rows by category
    category.events <- subset(events, events[,subset.category] == cat.val)
    subsets[[i]] <- category.events
    i <- i + 1
  }

   return(subsets)
}


##############


data.dir = "/home/username/Dropbox/Active_Projects/Mobility_Article/Data/"
figures.dir = "./Bike_Figs/"

load(paste(data.dir, "trips.Rdata", sep=""))


hamburg.area <- 755 #km^2
bremen.area <- 326.73
berlin.area <- 891.85

hamburg.population <- 1813587
bremen.population <- 547976
berlin.population <- 3292365

hamburg.density <- hamburg.population/hamburg.area
bremen.density <- hamburg.population/hamburg.area
berlin.density <- berlin.population/berlin.area

# REMOVE BAD/SPECIAL VALUES
trips.subset <- subset(trips, wegkm.k <= 950)


area <- hamburg.area
pop <- hamburg.population
density <- pop/area
trips.hamburg <- subset(trips.subset, bland == "Hamburg")
trips.hamburg <- data.frame(trips.hamburg, area, pop, density)


area <- bremen.area
pop <- bremen.population
density <- pop/area
trips.bremen <- subset(trips.subset, bland == "Bremen")
trips.bremen <- data.frame(trips.bremen, area, pop, density)

area <- berlin.area
pop <- berlin.population
density <- pop/area
trips.berlin <- subset(trips.subset, bland == "Berlin")
trips.berlin <- data.frame(trips.berlin, area, pop, density)


trips.w.area <- rbind(trips.hamburg, trips.bremen, trips.berlin)

# plot mean trip length against city area
cities.mean.trip.length <- c(mean(trips.hamburg$wegkm.k), mean(trips.bremen$wegkm.k), mean(trips.berlin$wegkm.k))
cities.area <- c(hamburg.area, bremen.area, berlin.area)
plot(cities.area, cities.mean.trip.length)


# plot mean trip length against city population
cities.pop <- c(hamburg.population, bremen.population, berlin.population)
plot(cities.pop, cities.mean.trip.length)


# plot mean trip length against city density
cities.density <- c(hamburg.density, bremen.density, berlin.density)
plot(cities.density, cities.mean.trip.length)


# plot trip length against area, population, and density
plot(trips.w.area$area, trips.w.area$wegkm.k)
plot(trips.w.area$pop, trips.w.area$wegkm.k)
plot(trips.w.area$density, trips.w.area$wegkm.k)


walking.trips <- subset(trips.w.area, hvm == "zu Fu")
bike.trips <- subset(trips.w.area, hvm == "Fahrrad")
car.trips <- subset(trips.w.area, hvm == "MIV (Fahrer)")

mean.walking <- mean(walking.trips$wegkm.k)
mean.bike <- mean(bike.trips$wegkm.k)
mean.car <- mean(car.trips$wegkm.k)

sd.walking <- sd(walking.trips$wegkm.k)
sd.bike <- sd(bike.trips$wegkm.k)
sd.car <- sd(car.trips$wegkm.k)


boxplot(walking.trips$wegkm.k)
boxplot(bike.trips$wegkm.k)
boxplot(car.trips$wegkm.k)
