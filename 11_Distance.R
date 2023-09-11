#plot the relationship between similarity and distance

#install.packages("geosphere")
library(geosphere)

pairs<-read.csv("Outputs/Stand1_pairs.csv")
#pairs$lon<-NA
#pairs$lat<-NA

d<-read.csv("Outputs/Site_info.csv")
#d$dist<-NA
#d$dist_to<-NA

#d$Lat<-as.numeric(d$Lat)
#d$Lon<-as.numeric(d$Lon)

library(data.table)

d <- data.table(d)
for (i in 1:nrow(d)) {
  print (d[i,]$id)
  for (j in 1:nrow(d)) {
    
    d1 <- d[id == d[i,]$id, dist:=distm(c(d[i,]$lon, d[i,]$lat), c(d[j,]$lon, d[j,]$lat), fun = distHaversine)/1000, ]
    d1 <- d1[, dist_to:= d[j,]$id,]
    if(exists('d2')){ d2<-rbindlist(list(d2,d1))} else {d2<-copy(d1)}
  }
}

#d2 <- na.omit(d2)
d2$distance <- paste(d2$id, d2$dist_to, sep=" ")
library(dplyr)
d2<-d2[!duplicated(d2$distance), ]

#write.csv(d2, "Outputs/min_d2.csv")

data<-read.csv("Outputs/Stand1_dat.csv")

names(d2)[6]<-"name"

data2<-merge(data, d2, by="name")

#write.csv(data2, "test.csv")
#https://stackoverflow.com/questions/44119236/how-to-make-a-loop-for-distance-r

#data2<-read.csv("test.csv")

library(lme4)
library(visreg)
library(lmerTest)
mod1<-lm(sims ~ dist, data=data2)
visreg(mod1,ylab="Pairwise Bray-Curtis Similarity", xlab="Distance (km)")
mod2<-lmer(sims ~ dist +(1|name), data=data2)
summary(mod2)


plot(sims~dist,data=data2, col=data2$times, pch=20)
abline(lm(sims~dist,data=data2))

#Create a function to generate a continuous color palette
rbPal <- colorRampPalette(c('red','blue'))

#This adds a column of color values
# based on the y values
data2$Col <- rbPal(24)[as.numeric(cut(data2$times,breaks = 24))]

plot(data2$dist,data2$sims,pch = 20,col = data2$Col)
