####
# MODIFIED REAL CASE TO RETURN 5 SYNTHETIC DATA SETS
# ONLY CALL ON REAL DATA !!!
# added input parameter num.synth.sets, default = 5
####

library(VGAM)

###############################################################################
#
# library 
# 
# source the plfit.r function (from http://www.santafe.edu/~aaronc/powerlaws/)
# if needed
# source("plfit.r")
#
###############################################################################
#
# example zone 
#
###############################################################################
runexample <- function(){
 set.seed(123)
 x <- (1-runif(1000))^(-1/(2.5-1))
 #if plfit exist (with source("plfit.r") for example):
 #plfit(x)
 #plfit R : xmin=1.007756 alpha=2.517830 D=0.02054889
 plpva(x,1)
 #plpva R p=0.233 gof=0.02164176
}
###############################################################################
# PLPVA  calculates the p-value for the given power-law fit to some data.
#    Source: http://www.santafe.edu/~aaronc/powerlaws/
#
#    PLPVA(x, xmin) takes data x and given lower cutoff for the power-law
#    behavior xmin and computes the corresponding p-value for the
#    Kolmogorov-Smirnov test, according to the method described in 
#    Clauset, Shalizi, Newman (2007).
#    PLPVA automatically detects whether x is composed of real or integer
#    values, and applies the appropriate method. For discrete data, if
#    min(x) > 1000, PLPVA uses the continuous approximation, which is 
#    a reliable in this regime.
#   
#    The fitting procedure works as follows:
#    1) For each possible choice of x_min, we estimate alpha via the 
#       method of maximum likelihood, and calculate the Kolmogorov-Smirnov
#       goodness-of-fit statistic D.
#    2) We then select as our estimate of x_min, the value that gives the
#       minimum value D over all values of x_min.
#
#    Note that this procedure gives no estimate of the uncertainty of the 
#    fitted parameters, nor of the validity of the fit.
#
#    Example:
#       x <- (1-runif(10000))^(-1/(2.5-1))
#       plpva(x,1)
#
# Version 1.0   (2012 August)
#
# Copyright (C) 2012 Laurent Dubroca (Sete, France)
# laurent - dot -dubroca - at - gmail - dot - com 
# Distributed under GPL 2.0
# http://www.gnu.org/copyleft/gpl.html
# PLPVA comes with ABSOLUTELY NO WARRANTY
# Matlab to R translation based on the original code of Aaron Clauset (Santa Fe Institute)
# Source: http://www.santafe.edu/~aaronc/powerlaws/
#
# Notes:
#
# 1. In order to implement the integer-based methods in R, the numeric
#    maximization of the log-likelihood function was used. This requires
#    that we specify the range of scaling parameters considered. We set
#    this range to be seq(1.5,3.5,0.01) by default. This vector can be
#    set by the user like so,
#
#       a <- plpva(x,1,vec=seq(1.001,5,0.001))
#
# 2. plvar can be told to limit the range of values considered as estimates
#    for xmin in two ways. First, it can be instructed to sample these
#    possible values like so,
#
#       a <- plpva(x,1,"sample",100)
#
#    which uses 100 uniformly distributed values on the sorted list of
#    unique values in the data set. Alternatively, it can simply omit all
#    candidates below a hard limit, like so
#
#       a <- plpva(x,1,"limit",3.4)
#
#    In the case of discrete data, it rounds the limit to the nearest
#    integer.
#
# 3. The default number of nonparametric repetitions of the fitting
# procedure is 1000. This number can be changed like so
#    
#       a <- plpva(x,Bt=10000)
#    
# 4. To silence the textual output to the screen, do
#    
#       a <- plpva(x,1,quiet=TRUE)
# 
###############################################################################
synthdata <-function(x=rpareto(1000,10,2.5),xmin=1,method="limit",value=c(),Bt=1000,quiet=FALSE,vec=seq(1.5,3.5,.01), num.synth.sets = 5){
   #init method value to NULL	
   sampl <- c() ; limit <- c()
###########################################################################################
#
#  test and trap for bad input
#
   switch(method, 
     sample = sampl <- value,
     limit = limit <- value,
     argok <- 0)
   
   if(exists("argok")){stop("(plvar) Unrecognized method")}

   if( !is.null(vec) && (!is.vector(vec) || min(vec)<=1 || length(vec)<=1) ){
     print(paste("(plvar) Error: ''range'' argument must contain a vector > 1; using default."))
     vec <- c()
   }
   if( !is.null(sampl) && ( !(sampl==floor(sampl)) ||  length(sampl)>1 || sampl<2 ) ){
     print(paste("(plvar) Error: ''sample'' argument must be a positive integer > 2; using default."))
     sample <- c()
   }
   if( !is.null(limit) && (length(limit)>1 || limit<1) ){
     print(paste("(plvar) Error: ''limit'' argument must be a positive >=1; using default."))
     limit <- c()
   }
   if( !is.null(Bt) && (!is.vector(Bt) || Bt<=1 || length(Bt)>1) ){
     print(paste("(plvar) Error: ''Bt'' argument must be a positive value > 1; using default."))
     vec <- c()
   }

#  select method (discrete or continuous) for fitting and test if x is a vector
   fdattype<-"unknow"
   if( is.vector(x,"numeric") ){ fdattype<-"real" }
   if( all(x==floor(x)) && is.vector(x) ){ fdattype<-"integer" }
   if( all(x==floor(x)) && min(x) > 1000 && length(x) > 100 ){ fdattype <- "real" }
   if( fdattype=="unknow" ){ stop("(plfit) Error: x must contain only reals or only integers.") }

   N   <- length(x)
   nof <- rep(0,Bt)

   # store 5 synthetic data sets
   synth <- list()

   if( !quiet ){
     print("synthdata")
     print("Power-law Distribution, parameter error calculation")
     print("Warning: This can be a slow calculation; please be patient.")
     print(paste(" n =",N,"xmin =",xmin,"- reps =",Bt,fdattype))
   } 
#
#  end test and trap for bad input
#
###########################################################################################

###########################################################################################
#
#  estimate xmin and alpha in the continuous case
#
   if( fdattype=="real" ){

     # EMPIRICAL DATA
     # compute D for the empirical distribution
     z     <- x[x>=xmin];
     nz   <- length(z)
     y     <- x[x<xmin];
     ny   <- length(y)
     alpha <- 1 + nz/sum(log(z/xmin))
     cz    <- (0:(nz-1))/nz
     cf    <- 1-(xmin/sort(z))^(alpha-1)

     # D, the KS statistic
     gof   <- max(abs(cz-cf))
     
     # the percent of data > xmin
     pz    <- nz/N

     # SYNTHETIC DATA - ONE DATASET PER ITERATION
     
     # compute distribution of gofs from semi-parametric bootstrap
     # of entire data set with fit
#     for(B in 1:length(nof)){
     for(B in 1:num.synth.sets){
       # semi-parametric bootstrap of data

       # uniform distribution of data above xmin
       n1 <- sum(runif(N)>pz)

       # random sample of data below xmin
       q1 <- y[sample(ny,n1,replace=TRUE)]
       n2 <- N-n1
       
       # CCDF
       q2 <- xmin*(1-runif(n2))^(-1/(alpha-1))

       # RETURNED SYNTHETIC DATA IS A MERGE OF DATA BELOW XMIN AND RANDOM DATA ABOVE XMIN
       q  <- sort(c(q1,q2))

       # estimate xmin and alpha via GoF-method

       # QMINS IS THE UNIQUE LIST OF DATA: WHY??
       # redundant to sort again
       qmins <- sort(unique(q)) 
       qmins <- qmins[-length(qmins)]
       if(!is.null(limit)){
         qmins<-qmins[qmins<=limit] 
       } 
       if(!is.null(sampl)){ 
         qmins <- qmins[unique(round(seq(1,length(qmins),length.out=sampl)))]
       }
       dat <-rep(0,length(qmins))

       # ITERATE THROUGH X VALUES, FIND MAX DISTANCE BETWEEN SYNTH AND FUNCTION CDF -> D
       for(qm in 1:length(qmins)){
         qmin <- qmins[qm]
         zq   <- q[q>=qmin]
         nq   <- length(zq)
         a    <- nq/sum(log(zq/qmin))

         # sythetic cdf
         cq   <- (0:(nq-1))/nq

         # functional cdf
         cf   <- 1-(qmin/zq)^a

         # D, the KS statistic - the max distance between SynthCDF and FCDF
         dat[qm] <- max(abs(cq-cf))
       }
       if(!quiet){
         print(paste(B,sum(nof[1:B]>=gof)/B))
       }
       
       # store distribution of estimated gof values
       # this is the min D between synthetic and functional CDF
       nof[B] <- min(dat)
       print("B is")
       print (B)
       synth[[B]] <- q
     }
   }
   
   print("returning ")
   print(length(synth))
   print(" data sets.")

   # return Ds and datasets
   return(list(nof, synth))
 }
