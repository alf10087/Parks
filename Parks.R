setwd("/Users/Alfonso/Google Drive/Harvard/Parks")
library(basic)
library(MASS)
library(ggplot2)
library(gdata)
library(sp)
library(ggmap)
library(foreach)
library(RgoogleMaps)
library(rgdal)
library(maps)
library(gdistance)
library(rgeos)

rm(list = ls())
options(digits=11)
### 1. Data cleaning

incidents <- read.csv("incidents.csv")
parks <- read.csv("parks.csv")
parks = parks[-1,]
parks <- na.omit(parks)
parks$psamanager <- NULL
parks$email <- NULL
parks$number <- NULL
parks$zipcode <- NULL
parks$parkservicearea <- NULL

################### Map crime and parks

sf <- readOGR("RPD_Parks.shp", layer="RPD_Parks")
sf <- sf[-c(219, 201),]

sfoutline <- readOGR("sfoutline.shp", layer="sfoutline")

proj4string(sf)
class(incidents)
graphData <- data.frame(incidents$X, incidents$Y)
class(graphData)
coordinates(graphData) <- ~incidents.X+incidents.Y
class(graphData)
proj4string(graphData)
proj4string(graphData)<-CRS("+proj=longlat +datum=NAD83")
graphData <- spTransform(graphData, CRS(proj4string(sf)))
identical(proj4string(sf), proj4string(graphData))
graphData <- data.frame(graphData)

df.sf_water <- read.csv(url("http://www.sharpsightlabs.com/wp-content/uploads/2014/12/sf_water_boundaries.txt"))
df.sf_neighborhoods <- read.csv(url("http://www.sharpsightlabs.com/wp-content/uploads/2014/12/sf_neighborhood_boundaries.txt"))

ggplot() +
  geom_polygon(data=df.sf_neighborhoods,aes(x=long,y=lat,group=group) ,fill="#F5FFFA",colour= "black", lwd=0.5) +
  geom_polygon(data=sf, aes(x= long, y= lat, group=group), colour ='green', fill='green', alpha = 0.2) +
  geom_point(data = graphData, aes(x = incidents.X, y = incidents.Y), colour = "red", fill="red", size=0.75, alpha = 0.5) + 
  stat_density2d(data=graphData, aes(x=incidents.X, y=incidents.Y))
  
graphData <- matrix(graphData)
graphData <- data.frame(graphData)
lambdahat(graphData, 5)

###### Math each crime with nearest park
graphData <- data.frame(incidents$X, incidents$Y)
coordinates(graphData) <- ~incidents.X+incidents.Y
proj4string(graphData)<-CRS("+proj=longlat +datum=NAD83")
graphData <- spTransform(graphData, CRS(proj4string(sf)))
identical(proj4string(sf), proj4string(graphData))

lambdahat(graphData, 0.5, poly = sfoutline)
a <- density(graphData)
Fdist <- list()
for(i in 1:150174[1]) {
  pDist <- vector()
  for(j in 1:dim(sf)[1]) { 
    pDist <- append(pDist, gDistance(graphData[i,],sf[j,])) 
  }
  Fdist[[i]] <- pDist
} 

min.dist <- unlist(lapply(Fdist, FUN=function(x) which(x == min(x))[1]))


graphData <- data.frame(graphData)


ggplot() + 
  geom_point(data = graphData, aes(x = incidents.X, y = incidents.Y), colour = "red", fill="red", size=0.75, alpha = 0.5) +
  stat_bin2d(bins = 2)

r <- raster(graphData)
