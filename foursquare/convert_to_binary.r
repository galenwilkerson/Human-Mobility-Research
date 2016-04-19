# load data, convert to .RData, save

rm(list = ls())

data.dir <- "umn_foursquare_datasets/"
filenames <- c("checkins","ratings","socialgraph","users","venues")

for (index in 1:length(filenames)) {
  print(filenames[index])
  my.data <- read.csv(paste(data.dir,filenames[index],".dat", sep=""), sep="|")
  my.data <- my.data[-1,]

  print(head(my.data))
  save(my.data, file = paste(data.dir,filenames[index],".RData", sep=""), sep="|")
}


rm(list = ls())

