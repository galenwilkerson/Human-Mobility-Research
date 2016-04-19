###############################################################################
#
# library 
# 
library(VGAM)   # zeta function
#library(R.matlab) # read matlab file just for test purpose



#test zone 



################################################################################
#
# PLFIT fits a power-law distributional model to data.
#
#    PLFIT(x) estimates x_min and alpha according to the goodness-of-fit
#    based method described in Clauset, Shalizi, Newman (2007). x is a
#    vector of observations of some quantity to which we wish to fit the
#    power-law distribution p(x) ~ x^-alpha for x >= xmin.
#    PLFIT automatically detects whether x is composed of real or integer
#    values, and applies the appropriate method. For discrete data, if
#    min(x) > 1000, PLFIT uses the continuous approximation, which is
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
#       plfit(x)
#
#
# Version 1.0   (2008 February)
# Version 1.1   (2008 February)
#    - correction : division by zero if limit >= max(x) because the unique R function do no sort
#                   and the matlab function do...
# Version 1.1   (minor correction 2009 August)
#    - correction : lines 230 zdiff calcul was wrong when xmin=0 (thanks to Naoki Masuda)
#    - gpl version updated to v3.0 (asked by Felipe Ortega)
# Version 1.2 (2011 August)
#    - correction for method "limit" thanks to David R. Pugh 
#      xmins <- xmins[xmins<=limit] is now xmins <- xmins[xmins>=limit]
#    - "fixed" method added for xmins from David R. Pugh
#    - modifications by Alan Di Vittorio:
#       - correction : zdiff calculation was wrong when xmin==1
#       - the previous zdiff correction was incorrect
#       - correction : x has to have at least two unique values
#       - additional discrete x input test : discrete x cannot contain the value 0
#       - added option to truncate continuous xmin search when # of obs gets small
#
# Copyright (C) 2008,2011 Laurent Dubroca laurent.dubroca_at_gmail.com 
# (Stazione Zoologica Anton Dohrn, Napoli, Italy)
# Distributed under GPL 3.0 
# http://www.gnu.org/copyleft/gpl.html
# PLFIT comes with ABSOLUTELY NO WARRANTY
# Matlab to R translation based on the original code of Aaron Clauset (Santa Fe Institute)
# Source: http://www.santafe.edu/~aaronc/powerlaws/
#
# Notes:
#
# 1. In order to implement the integer-based methods in Matlab, the numeric
#    maximization of the log-likelihood function was used. This requires
#    that we specify the range of scaling parameters considered. We set
#    this range to be seq(1.5,3.5,0.01) by default. This vector can be
#    set by the user like so,
#
#       a <- plfit(x,"range",seq(1.001,5,0.001))
#
# 2. PLFIT can be told to limit the range of values considered as estimates
#    for xmin in two ways. First, it can be instructed to sample these
#    possible values like so,
#
#       a <- plfit(x,"sample",100)
#
#    which uses 100 uniformly distributed values on the sorted list of
#    unique values in the data set. Alternatively, it can simply omit all
#    candidates below a hard limit, like so
#
#       a <- plfit(x,"limit",3.4)
#
#    In the case of discrete data, it rounds the limit to the nearest
#    integer.
#
#     Finally, if you wish to force the threshold parameter to take a specific value 
#     (useful for bootstrapping), simply call plfit() like so
#       
#       a <- plfit(x,"fixed",3.5)
#
# 3. When the input sample size is small (e.g., < 100), the estimator is
#    known to be slightly biased (toward larger values of alpha). To
#    explicitly use an experimental finite-size correction, call PLFIT like
#    so
#
#       a <- plfit(x,finite=TRUE)
#
# 4. For continuous data, PLFIT can return erroneously large estimates of 
#    alpha when xmin is so large that the number of obs x >= xmin is very 
#    small. To prevent this, we can truncate the search over xmin values 
#    before the finite-size bias becomes significant by calling PLFIT as
#    
#       a = plfit(x,nosmall=TRUE);
#    
#    which skips values xmin with finite size bias > 0.1.
#
###############################################################################
plfit<-function(x=rpareto(1000,10,2.5),method="limit",value=c(),finite=FALSE,nowarn=FALSE,nosmall=FALSE){
   #init method value to NULL	
   vec <- c() ; sampl <- c() ; limit <- c(); fixed <- c()
###########################################################################################
#
#  test and trap for bad input
#
   switch(method, 
     range = vec <- value,
     sample = sampl <- value,
     limit = limit <- value,
     fixed = fixed <- value,
     argok <- 0)
   
   if(exists("argok")){stop("(plfit) Unrecognized method")}

   if( !is.null(vec) && (!is.vector(vec) || min(vec)<=1 || length(vec)<=1) ){
     print(paste("(plfit) Error: ''range'' argument must contain a vector > 1; using default."))
     vec <- c()
   }
   if( !is.null(sampl) && ( !(sampl==floor(sampl)) ||  length(sampl)>1 || sampl<2 ) ){
     print(paste("(plfit) Error: ''sample'' argument must be a positive integer > 2; using default."))
     sample <- c()
   }
   if( !is.null(limit) && (length(limit)>1 || limit<1) ){
     print(paste("(plfit) Error: ''limit'' argument must be a positive >=1; using default."))
     limit <- c()
   }
   if( !is.null(fixed) && (length(fixed)>1 || fixed<=0) ){
     print(paste("(plfit) Error: ''fixed'' argument must be a positive >0; using default."))
     fixed <- c() 
   }   

#  select method (discrete or continuous) for fitting and test if x is a vector
   fdattype<-"unknow"
   if( is.vector(x,"numeric") ){ fdattype<-"real" }
   if( all(x==floor(x)) && is.vector(x) ){ fdattype<-"integer" }
   if( all(x==floor(x)) && min(x) > 1000 && length(x) > 100 ){ fdattype <- "real" }
   if( fdattype=="unknow" ){ stop("(plfit) Error: x must contain only reals or only integers.") }

#
#  end test and trap for bad input
#
###########################################################################################

###########################################################################################
#
#  estimate xmin and alpha in the continuous case
#
   if( fdattype=="real" ){

    xmins <- sort(unique(x))
    xmins <- xmins[-length(xmins)]

    if( !is.null(limit) ){
      xmins <- xmins[xmins>=limit]
    } 
    if( !is.null(fixed) ){
      xmins <- fixed
    }
    if( !is.null(sampl) ){ 
      xmins <- xmins[unique(round(seq(1,length(xmins),length.out=sampl)))]
    }
 
    dat <- rep(0,length(xmins))
    z   <- sort(x)
    
    for( xm in 1:length(xmins) ){
      xmin <- xmins[xm]
      z    <- z[z>=xmin]
      n    <- length(z)
      # estimate alpha using direct MLE
      a    <- n/sum(log(z/xmin))
      # truncate search if nosmall is selected
      if( nosmall ){
       if((a-1)/sqrt(n) > 0.1){
           dat <- dat[1:(xm-1)]
           print(paste("(plfit) Warning : xmin search truncated beyond",xmins[xm-1]))
           break
       }
      }
      # compute KS statistic
      cx   <- c(0:(n-1))/n
      cf   <- 1-(xmin/z)^a
      dat[xm] <- max(abs(cf-cx))
    }
 
    D     <- min(dat)
    xmin  <- xmins[min(which(dat<=D))]
    z     <- x[x>=xmin]
    n     <- length(z)
    alpha <- 1 + n/sum(log(z/xmin))

    if( finite ){
      alpha <- alpha*(n-1)/n+1/n # finite-size correction
    }
    if( n<50 && !finite && !nowarn){
      print("(plfit) Warning : finite-size bias may be present")
    }

   }
#
#  end continuous case
#
###########################################################################################

###########################################################################################
#
#  estimate xmin and alpha in the discrete case
#
   if( fdattype=="integer" ){

    if( is.null(vec) ){ vec<-seq(1.5,3.5,.01) } # covers range of most practical scaling parameters
    zvec <- zeta(vec)

    xmins <- sort(unique(x))
    xmins <- xmins[-length(xmins)]

    if( !is.null(limit) ){
      limit <- round(limit)
      xmins <- xmins[xmins>=limit]
    } 

    if( !is.null(fixed) ){
      xmins <- fixed
    }

    if( !is.null(sampl) ){ 
      xmins <- xmins[unique(round(seq(1,length(xmins),length.out=sampl)))]
    }

    if( is.null(xmins) || length(xmins) < 2){
      stop("(plfit) error: x must contain at least two unique values.")
    }

    if(length(which(xmins==0) > 0)){
      stop("(plfit) error: x must not contain the value 0.")
    }

    xmax <- max(x)
     dat <- matrix(0,nrow=length(xmins),ncol=2)
       z <- x
    for( xm in 1:length(xmins) ){
      xmin <- xmins[xm]
      z    <- z[z>=xmin]
      n    <- length(z)
      # estimate alpha via direct maximization of likelihood function
      #  vectorized version of numerical calculation
      # matlab: zdiff = sum( repmat((1:xmin-1)',1,length(vec)).^-repmat(vec,xmin-1,1) ,1);
      if(xmin==1){
        zdiff <- rep(0,length(vec))
      }else{
       zdiff <- apply(rep(t(1:(xmin-1)),length(vec))^-t(kronecker(t(array(1,xmin-1)),vec)),2,sum)
      }
      # matlab: L = -vec.*sum(log(z)) - n.*log(zvec - zdiff);
      L <- -vec*sum(log(z)) - n*log(zvec - zdiff);
      I <- which.max(L)
      # compute KS statistic
      fit <- cumsum((((xmin:xmax)^-vec[I])) / (zvec[I] - sum((1:(xmin-1))^-vec[I])))
      cdi <- cumsum(hist(z,c(min(z)-1,(xmin+.5):xmax,max(z)+1),plot=FALSE)$counts/n)
      dat[xm,] <- c(max(abs( fit - cdi )),vec[I])
    }
    D     <- min(dat[,1])
    I     <- which.min(dat[,1])
    xmin  <- xmins[I]
    n     <- sum(x>=xmin)
    alpha <- dat[I,2]

    if( finite ){
      alpha <- alpha*(n-1)/n+1/n # finite-size correction
    }
    if( n<50 && !finite && !nowarn){
      print("(plfit) Warning : finite-size bias may be present")
    }

   }
#
#  end discrete case
#
###########################################################################################

#  return xmin, alpha and D in a list
   return(list(xmin=xmin,alpha=alpha,D=D))
 }