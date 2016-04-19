
data.dir = "/home/username/Dropbox/Active_Projects/Mobility_Article/Data/"

load(paste(data.dir, "trips.Rdata", sep=""))

trips <- trips[order(as.POSIXlt(trips$timestamp.event)),]

save(trips, file = paste(data.dir, "trips.Rdata", sep=""))
