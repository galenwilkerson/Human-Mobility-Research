
# load data

########################################################

rm(list = ls())


########################################################


data.dir = "/home/username/Dropbox/Active_Projects/Mobility_Article/Data/"
figures.dir = "./Figures/"

load(paste(data.dir, "trips.Rdata", sep=""))

write.csv(trips, paste(data.dir, "trips.csv", sep=""))

