# twitter data - loading and preliminary visualisation
tw <- read.csv("results.csv") # load
names(tw)
plot(tw$X_tweet_location, tw$Y_tweet_location) # plot

# load into spatial data frame
sessionInfo() # shows packages loaded - if no geo ones, load them...
x = c("ggplot2", "sp", "rgeos", "mapproj", "rgdal", "maptools")
lapply(x, require, character.only=T) # the R packages we'll be using
tws <- SpatialPointsDataFrame(coords=tw[,4:5], data=tw)
plot(tws) # automatically plots as spatial data

# analysis of frequency of tweets by user
tw$user_id <- as.factor(tw$user_id)
summary(tw$user_id)
nrow(tw) / length(unique(tw$user_id))

# analysis of timescales of twitter data
summary(tw$year) ; summary(tw$month) ; summary(tw$day) ; summary(tw$hour)
ddate <-as.Date(paste(tw$year, tw$month, tw$day, sep="."), format= "%Y.%m.%d")
summary(ddate)
plot(ddate)
range(ddate)
hist(tw$hour)


