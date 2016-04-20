# Human Mobility Research - Complex systems of human mobility

This was a research project to understand the scaling exponent of human mobility trip length distributions, especially in cities.

Previous research using big data approaches with social media was not able to address urban scale mobility patterns. Our research dramatically expanded on the scale of prior work to trip lengths of 10km and below.

Understanding this data involved a great deal of visualization and categorical visualization, as well as familiarity with methods and results of other related publications.

The original data was Mobility in Germany, 2008.  We investigated many many data features.

Some highlights were those seen in the final paper:
- trip length distribution by trip purpose, transportation mode, time of day, day of week
- histograms of number of trips by time of day and day of week
- rescaled trip length distribution, normalized by maximum trip length per mode

Interesting initial results:
- This data, designed exactly as a mobility survey rather than 'big data', allowed us to observe scaling exponents within cities, not only on the extra-urban distance.
- Confirmation of scaling exponent (fitting trip length as a possible power-law distribution: C * x ^ -alpha) of other research
- Rescaled trips by maximum length seem to indicate that fossil-fuel modes are more heavy-tailed than human-powered modes.
- Trip length seems highly correlated with purpose, time of day, and day of week.

Many other areas were investigated briefly:
- time series
- carpooling
- various fitting methods (symbolic regression)
- comparison with foursquare data
- clustering using PCA
- wavelet transforms

Publication:

http://ieeexplore.ieee.org/xpls/abs_all.jsp?arnumber=6849329&tag=1

Wilkerson, Galen, Ramin Khalili, and Stefan Schmid.
"Urban mobility scaling: Lessons from ‘little data’." 
Computer Communications Workshops (INFOCOM WKSHPS), 
2014 IEEE Conference on. IEEE, 2014.
