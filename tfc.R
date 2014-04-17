## tweet-flow-comparison -tfc

# tws@data <- tws@data[c(1,4,5,13,14,15,16)]
# library(stringr)
# tws$text <- strtrim(tws$text, 20)
# save(tws, file = "tweetGeo.RData")
# run file "si-museums.R" before this for code to work

load("tweetGeo.RData") # load the tweet dataset
twsh <- SpatialPoints(cbind(tws$X_favoured_location, tws$Y_favoured_location))
names(twsh)
plot(w)
points(twsh)


### 
# new approach: use a for loop
library(rgeos)
tws$ward_home <- 1
for(i in 1:nrow(w)) {
  tws$ward_home[gIntersects(twsh, w[i,], byid=T)] <- i 
}

plot(w[16,])
points(twsh[which(tws$ward_home == 16),]) # verification
points(twsh[gIntersects(twsh, w[16,], byid=T),])
points(tws)
points(twsh)

# classification of museum tweets
head(tws$text,100)
plot(gBuffer(m, byid=T, width=200))
points(tws)

# proximity of tweets to museums
tws$minDist <- 1
for(i in 1:nrow(tws)){
  distVec <- spDistsN1(m, tws[i,])
#   distVec <- spDistsN1(m,
#   SpatialPoints(coords=matrix(c(tws$X_favoured_location[i], tws$X_favoured_location[i]), ncol=2)))
  tws$minDist[i] <- min(distVec)
  tws$closest[i] <- which.min(distVec)
}
summary(tws$closest)
tws$museum.closest <- m$name[tws$closest]

summary(tws$minDist)
head(tws$closest)
plot(tws[which(tws$closest == 8),])
points(tws[which(tws$closest == 8 & tws$minDist < 300),], col="green")
points(m[8,], col="red", lwd=5)

### tweets made close to museums
tws[which(tws$minDist < 50),]
nrow(tws[which(tws$minDist < 50),])
tws[which(tws$minDist < 50 & grepl("I'm at", tws$text)),]
# these have a 'hit rate' of almost 100% 27 of the 28 were actually about museums

tws[which(tws$minDist < 200),"text"]
nrow(tws[which(tws$minDist < 200),])

tws.far <- tws[which(tws$minDist > 10000),] # far less identifiable with museums
plot(tws.far)
points(m)

tws.3 <- tws[which(tws$minDist < 300 ),]
plot(tws.3)
points(m, col="red", lwd=5)
# variable of museum-relatedness:

S[1:3,1:3]
St <- matrix(0, nrow=nrow(S), ncol=ncol(S))
dim(St)

### calculate and plot interaction
plot(w, col="grey", 
     xlim=bbox(tws)[1,], ylim=bbox(tws)[2,],
     xlab = "tedt" )
oklabs <- c(410000, 430000, 450000)
axis(side=1, at=oklabs, labels=c(410, 430, "450 km Easting"))
axis(side=2, at = c(430000, 440000), labels =c(430, "440 km Northing"))

points(tws.3)
points(x=tws.3$X_favoured_location, y=tws.3$Y_favoured_location, col="green")
for(i in 1:nrow(tws.3)){
  St[tws.3$ward_home[i], tws.3$closest[i]] <- St[tws.3$ward_home[i], tws.3$closest[i]] + 1
  lines(c(coordinates(m[tws.3$closest[i],])[,1], tws.3$X_favoured_location[i]),
          c(coordinates(m[tws.3$closest[i],])[,2], tws.3$Y_favoured_location[i]), 
        alpha=0/3
          )
  tws.3$dist[i] <- sqrt((coordinates(m[tws.3$closest[i],])[,1] - tws.3$X_favoured_location[i])^2 +
                       (coordinates(m[tws.3$closest[i],])[,2] - tws.3$Y_favoured_location[i])^2)
}
summary(St)
points(x=tws.3$X_favoured_location, y=tws.3$Y_favoured_location, col="green")
points(m, col="red", lwd=5)
which.max(tws.3$minDist)

# analysis of St
length(c(St))
179 * 15
length(which(c(St) == 0))
sum(St)


tws.far <- tws[which(tws.3$dist > 20000),]

summary(St)
points(x=tws.3$X_favoured_location, y=tws.3$Y_favoured_location, col="green")
points(m, col="red", lwd=5)
which.max(tws.3$minDist)
tws.3[147,]
summary(tws$minDist)

### plot aggregated flows
plot(w, col="grey", 
     xlim=bbox(tws)[1,], ylim=bbox(tws)[2,])
oklabs <- c(410000, 430000, 450000)
axis(side=1, at=oklabs, labels=c(410, 430, "450 km Easting"))
axis(side=2, at = c(430000, 440000), labels =c(430, "440 km Northing"))

for(i in 1:nrow(tws.3)){
  lines(c(coordinates(m[tws.3$closest[i],])[,1], coordinates(w[tws.3$ward_home[i],])[,1]),
        c(coordinates(m[tws.3$closest[i],])[,2], coordinates(w[tws.3$ward_home[i],])[,2]), 
        alpha=0.2,
  )
}
summary(St)
points(x=tws.3$X_favoured_location, y=tws.3$Y_favoured_location, col="green")
points(m, col="red", lwd=5)
### extract information 
aggregate(tws.3@data, by=list(tws.3$museum.closest), length)
# write.csv(aggregate(tws.3@data, by=list(tws.3$museum.closest), length), "" )
# write.csv(aggregate(tws.3@data, by=list(tws.3$museum.closest), mean)$dist, "")
# write.csv(aggregate(tws.3@data, by=list(tws.3$museum.closest), mean)$minDist, "")
aggregate(tws.3@data, by=list(tws.3$museum.closest), length)$Group.1
# write.csv(m@data[which(m$name %in% aggregate(tws.3@data, by=list(tws.3$museum.closest), length)$Group.1),], "")

### now create estimated flows
inc <- 0.1
beta <- 0.3
P <- pops$totpop # zone population
W <- A <- rep(1, times=nrow(m@coords))
S <- D^0
head(S)

# unconstrained model, no attractiveness or income variability set
inc <- 1 # all incomes the same
W <- W^0

for(i in 1:nrow(w)){
  for(j in 1:nrow(m)){
    S[i,j] <- inc * P[i] * W[j] * exp(-beta * D[i,j])
  }  
}
head(S)
head(St)
cor(c(S),c(St)) # great - positive relationships! - 0.31 in base

# now do in for loop to find best beta
to = 200
SSt <- data.frame(beta=rep(1,to), un=rep(1,to))
for(k in 1:to){
  b <- k*0.01
  for(i in 1:nrow(w)){
    for(j in 1:nrow(m)){
    S[i,j] <- inc * P[i] * W[j] * exp(-b * D[i,j])
      }  
    }
  SSt$beta[k] <- b
  SSt$un[k] <- cor(c(S),c(St))
}

SSt[which.max(SSt$un),]
plot(SSt)
# now rerun it manually with beta at its optimal value
beta <- SSt[which.max(SSt$un),1]

for(i in 1:nrow(w)){
  for(j in 1:nrow(m)){
    S[i,j] <- inc * P[i] * W[j] * exp(-beta * D[i,j])
  }  
}
sum(S * D) / sum(S) # average distance in this model
sum(St * D) / sum(St)

### now add attractiveness of museum variable
# source("museum-attractiveness.R")
to = 200
# SSt <- data.frame(beta=rep(1,to), un=rep(1,to))
for(k in 1:to){
  b <- k*0.01
  for(i in 1:nrow(w)){
    for(j in 1:nrow(m)){
      S[i,j] <- inc * P[i] * W[j] * exp(-b * D[i,j])
    }  
  }
#   SSt$beta[k] <- b
  SSt$un.w[k] <- cor(c(S),c(St))
}

SSt[which.max(SSt$un),]
SSt[which.max(SSt$un.w),]
plot(SSt$beta, SSt$un.w)
points(SSt$beta, SSt$un)

### Now rerun for variable incomes!
# source("demand.R", echo=T)
summary(inc)
to = 200
# SSt <- data.frame(beta=rep(1,to), un=rep(1,to))
for(k in 1:to){
  b <- k*0.01
  for(i in 1:nrow(w)){
    for(j in 1:nrow(m)){
      S[i,j] <- inc[i] * P[i] * W[j] * exp(-b * D[i,j])
    }  
  }
  #   SSt$beta[k] <- b
  SSt$un.winc[k] <- cor(c(S),c(St))
}

SSt[which.max(SSt$un),]

SSt[which.max(SSt$un.w),]
plot(SSt$beta, SSt$un.w)
points(SSt$beta, SSt$un)
points(SSt$beta, SSt$un.winc)
library(plyr)
library(reshape2)
SSt <- rename(SSt, c("un" = "Baseline", "un.w" = "W set", "un.winc" = "W & Inc set"))

### ggplot results
SSt.m <- melt(SSt, id.vars="beta", value.name="Correlation", varnames="Model specification")
head(SSt.m)
ggplot() +
  geom_point(data=SSt.m, aes(x=beta, y=Correlation, color=variable)) +
  geom_vline( xintercept = c(SSt$beta[which.max(SSt[,2])],
                            SSt$beta[which.max(SSt[,3])],
                            SSt$beta[which.max(SSt[,4])]
                            )
                           ) +
  scale_color_discrete(name = "Model spec.") +
  theme_classic() 

ggsave("beta-tests-2.png")

