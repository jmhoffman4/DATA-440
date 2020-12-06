rm(list=ls(all=TRUE))

# install.packages("raster", dependencies = TRUE)
# install.packages("sf", dependencies = TRUE)
# install.packages("tidyverse", dependencies = TRUE)
# install.packages("maptools", dependencies = TRUE)
# install.packages("spatstat", dependencies = TRUE)
# install.packages("units", dependencies = TRUE)
#install.packages(c("sp", "MASS", "reshape2","geojsonio","rgdal","downloader","maptools","dplyr","broom","stplanr", "ggplot2", "leaflet"), dependencies = TRUE)
#install.packages("arulesViz", dependencies = TRUE)
library(arulesViz)
library(raster)
library(sf)
library(tidyverse)
library(maptools)
library(spatstat)
library(units)
library(sp)
library(MASS)
library(reshape2)
library(geojsonio)
library(rgdal)
library(downloader)
library(maptools)
library(dplyr)
library(broom) 
library(stplanr)
library(ggplot2)
library(leaflet)
library(png)
setwd("~/JMHFall2020/DATA440/R/Togo/Data")

EW <- geojson_read("http://geoportal.statistics.gov.uk/datasets/8edafbe3276d4b56aec60991cbddda50_2.geojson", what = "sp")
plot(EW)

head(EW@data)
#pull out london using grep and the regex wildcard for'start of the string' (^) to to look for the bit of the district code that relates to London (E09) from the 'lad15cd' column in the data slot of our spatial polygons dataframe
London <- EW[grep("^E09",EW@data$lad15cd),]
#plot it
plot(London)
summary(London)

#first transfrom to BNG - this will be important for calculating distances using spTransform
BNG = "+init=epsg:27700"
LondonBNG <- spTransform(London, BNG)
#now, order by borough code - *this step will be imporant later on*
LondonBNG <- LondonBNG[order(LondonBNG$lad15cd),]
#now use spDists to generate a big distance matrix of all distances between boroughs in London
dist <- spDists(LondonBNG)
#melt this matrix into a list of origin/destination pairs using melt. Melt in in the reshape2 package. Reshape2, dplyr and ggplot, together, are some of the best packages in R, so if you are not familiar with them, get googling and your life will be much better!
distPair <- melt(dist)

#read in your London Commuting Data
cdata <- read.csv("https://www.dropbox.com/s/7c1fi1txbvhdqby/LondonCommuting2001.csv?raw=1")
#read in a lookup table for translating between old borough codes and new borough codes
CodeLookup <- read.csv("https://www.dropbox.com/s/h8mpvnepdkwa1ac/CodeLookup.csv?raw=1")
#read in some population and income data
popincome <- read.csv("https://www.dropbox.com/s/84z22a4wo3x2p86/popincome.csv?raw=1")

#now merge these supplimentary data into your flow data dataframe
cdata$OrigCodeNew <- CodeLookup$NewCode[match(cdata$OrigCode, CodeLookup$OldCode)]
cdata$DestCodeNew <- CodeLookup$NewCode[match(cdata$DestCode, CodeLookup$OldCode)]
cdata$vi1_origpop <- popincome$pop[match(cdata$OrigCodeNew, popincome$code)]
cdata$vi2_origsal <- popincome$med_income[match(cdata$OrigCodeNew, popincome$code)]
cdata$wj1_destpop <- popincome$pop[match(cdata$DestCodeNew, popincome$code)]
cdata$wj2_destsal <- popincome$med_income[match(cdata$DestCodeNew, popincome$code)]

#Data needs to be ordered by borough code, if it's not, we will run into problems when we try to merge our distance data back in later, so to make sure, we can arrange by orign and then destination using dplyr's 'arrange' function
cdata <- arrange(cdata, OrigCodeNew, DestCodeNew)

#First create a new total column which excludes intra-borough flow totals (well sets them to a very very small number for reasons you will see later...)
cdata$TotalNoIntra <- ifelse(cdata$OrigCode == cdata$DestCode,0,cdata$Total)
cdata$offset <- ifelse(cdata$OrigCode == cdata$DestCode,0.0000000001,1)
# now add the distance column into the dataframe
cdata$dist <- distPair$value
head(cdata)


#We'll just use the first 7 boroughs by code, so first, create a vector of these 7 to match with our data
toMatch<-c("00AA", "00AB", "00AC", "00AD", "00AE", "00AF", "00AG")
#subset the data by the 7 sample boroughs
#first the origins
cdatasub <- cdata[grep(paste(toMatch,collapse = "|"), cdata$OrigCode),]
#then the destinations
cdatasub <- cdatasub[grep(paste(toMatch,collapse = "|"), cdata$DestCode),]
#now chop out the intra-borough flows
cdatasub <- cdatasub[cdatasub$OrigCode!=cdatasub$DestCode,]
#now unfortunately if you look at the file, for some reason the grep process has left a lot of empty data cells in the dataframe, so let's just chop out everything after the 7*7 - 7 (42) pairs we are interested in...
cdatasub <- cdatasub[1:42,]
#now re-order so that OrigCodeNew, DestCodeNew and TotalNoIntra are the first three columns *note that you have to be explicit about the select function in the dplyr package as MASS also has a 'select' function and R will try and use this by default. We can be explict by using the syntax package::function
cdatasub <- dplyr::select(cdatasub, OrigCodeNew, DestCodeNew, Total, everything())

#use the od2line function from RObin Lovelace's excellent stplanr package
travel_network <- od2line(flow = cdatasub, zones = LondonBNG)
#and set the line widths to some sensible value according to the flow
w <- cdatasub$Total / max(cdatasub$Total) * 10
#now plot it...
plot(travel_network, lwd = w)
plot(LondonBNG, add=T)
#transform to wgs84
travel_networkwgs <- spTransform(travel_network, "+init=epsg:4326")
#plot in leaflet
leaflet() %>% addTiles() %>% addPolylines(data = travel_networkwgs)

#now we can create pivot table to turn paired list into matrix (and compute the margins as well)
cdatasubmat <- dcast(cdatasub, Orig ~ Dest, sum, value.var = "Total", margins=c("Orig", "Dest"))
cdatasubmat

#First plot the commuter flows against distance and then fit a model line with a ^-2 parameter
plot1 <- qplot(cdata$dist, cdata$Total)
#and now the model fit...
plot1 + stat_function(fun=function(x)x^-2, geom="line", aes(colour="^-2"))
plot2 <- qplot(cdata$vi1_origpop, cdata$Total)
plot2 + stat_function(fun=function(x)x^1, geom="line", aes(colour="^1"))

plot3 <- qplot(cdata$wj2_destsal, cdata$Total)
plot3 + stat_function(fun=function(x)x^1, geom="line", aes(colour="^1"))
#set up some variables to hold our parameter values in:
mu <- 1
alpha <- 1
beta <- -2
k <- 1
T2 <- sum(cdatasub$Total)

vi1_mu <- cdatasub$vi1_origpop^mu
wj2_alpha <- cdatasub$wj2_destsal^alpha
dist_beta <- cdatasub$dist^beta
T1 <- vi1_mu*wj2_alpha*dist_beta
k <- T2/sum(T1)

#run the model and store all of the new flow estimates in a new column in the dataframe
cdatasub$unconstrainedEst1 <- round(k*vi1_mu*wj2_alpha*dist_beta,0)
#check that the sum of these estimates makes sense
sum(cdatasub$unconstrainedEst1)
#turn it into a little matrix and have a look at your handy work
cdatasubmat1 <- dcast(cdatasub, Orig ~ Dest, sum, value.var = "unconstrainedEst1", margins=c("Orig", "Dest"))
cdatasubmat1

cdatasubmat
png::writePNG(cdatasubmat, "cdatasubmat.png")
