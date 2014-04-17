x = c("ggplot2", "sp", "rgeos", "rgdal", "maptools")
lapply(x, require, character.only=T)
load("wards.RData") # load w, the wards dataset
pops <- SpatialPointsDataFrame(coords=coordinates(w), 
  data=w@data, proj4string = CRS("+init=epsg:27700"), match.ID=F) # convert to points

plot(w)
points(pops) # geographical centroids (population weighted would be better)
load("museums.RData") # load m, the museums dataset
coordinates(m)
m.os <- spTransform(m, CRSobj=CRS("+init=epsg:27700"))
m <- SpatialPointsDataFrame(m.os, data=m@data) 
points(m, col="red", lwd=5); rm(m.os) # plot the museums

# setup SI model
library(rgeos)
gDistance(m[1,], pops[1,])
D <- gDistance(m, pops, byid=T)/1000
inc <- 0.1
beta <- 0.3
P <- pops$totpop # zone population
W <- A <- rep(1, times=nrow(m@coords))
S <- D^0
head(S)

# unconstrained model
for(i in 1:nrow(w)){
  for(j in 1:nrow(m)){
    S[i,j] <- inc * P[i] * W[j] * exp(-beta * D[i,j])
  }  
}

head(S); summary(S)
head(rowSums(S))

plot(S[,1:2])
plot(D[,1], S[,1]) # distance vs flow rate
simodel <- lm(S[,1] ~ pops$totpop + D[,1])
summary(simodel)

plot(w)
points(pops, col = "red")
ws <- sample(1:nrow(w), size=20)
for(i in ws){
  for(j in 1:nrow(m)){
    lines(matrix(c(coordinates(m[j,]),coordinates(w[i,])), ncol=2, byrow=T),
    lwd = (S[i,j] / mean(S)))
    points(pops[i,], col="green", lwd=3)
  }
}
points(m, col="red", lwd=5)

# constrained model
A <- D^0

for(i in 1:nrow(w)){ 
  A[i,] <- 1/(sum(W * exp(-beta * D[i,]))) 
}

for(i in 1:nrow(w)){
  for(j in 1:nrow(m)){
    S[i,j] <- A[i,j]  * inc * pops$totpop[i] * W[j] * exp(-beta * D[i,j])    
  }
}

plot(w)
for(i in ws){
  for(j in 1:nrow(m)){
    lines(matrix(c(coordinates(m[j,]),coordinates(w[i,])), ncol=2, byrow=T),
          lwd = (S[i,j] / mean(S)))
    points(pops[i,], col="green", lwd=
             pops$totpop[i]*2 / mean(pops$totpop) # makes origin size proportional to pop.
           )
  }
}
points(m, col="red", lwd=5)


head(rowSums(S))
