
# load data

# remove waiting time

# create absolute start timestamps (POSIXct)

# add as field

# order data frame by timestamp

# save in global directory


rm(list = ls())

load("merged_trips.RData")

trips <- subset(mergedtrips, st.std <= 23 & st.min <= 59, select = -c(waiting.time))

timestamp.event <- as.POSIXlt(paste(trips$zusatz.st.date, " ", trips$st.std, ":", trips$st.min, sep=""))


trips <- data.frame(trips, timestamp.event)

trips <- trips[order(as.POSIXlt(trips$timestamp.event)),]



base.data.dir <- "/home/username/Dropbox/Active_Projects/Mobility_Article/Data/"

save(trips, file = paste(base.data.dir, "trips.Rdata", sep=""))
