# load travels (Reisen)


# for each mode (and "all modes")

# plot histogram of categories
# fit categories


############################


rm(list = ls())
library(ggplot2)
library(foreign)

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
figures.dir = "./Figures/"


load(paste(data.dir, "travels.Rdata", sep=""))


###############
# PLOT THESE AS LOG LOG CCDFS
  
   
# REMOVE BAD/SPECIAL VALUES
travels.subset <- subset(travels, p1016 <= 20000)

this.cat <- "hvm_r"

cat.vals <- as.vector(unique(travels.subset[,this.cat]))

Legend <- travels.subset[,this.cat]


########################### FIND log-log CCDF #############################

# for each subset of cat, find the ccdf

  df <- data.frame()
  
  for(cat.val in cat.vals) {

    travels.subset.cat <- subset(travels.subset, travels.subset[,this.cat] == cat.val)
  

## # use hist for finding breaks
    h.inter <- hist(travels.subset.cat$p1016, breaks = 100000, plot = F)
    
## # find xvalues, log x values, log ccdf
    my.breaks <-  h.inter$breaks
    travel.lengths <- my.breaks[-1]
    log10.travel.length <- log10(travel.lengths)

## # find CDF, use for CCDF, log CCDF
    cdf <- ecdf(travels.subset.cat$p1016)
    CDF <- cdf(travel.lengths)
    CCDF1 <- 1 - CDF
    log10.ccdf <- log10(CCDF1)

## # keep track of results in data frame by category for plotting later
    df <- rbind(df, data.frame(log10.travel.length, log10.ccdf, cat.val))

}

  
  # plot travel length
  qplot(log10.travel.length, log10.ccdf, data = df, geom="line", color = cat.val) +
    ggtitle(paste(this.cat, "log log travel length (km)"))
 

  ggsave(paste(figures.dir, this.cat, "travel_length.pdf" , sep = "_"), width = 297, height = 210, units = "mm")
 
  
}


