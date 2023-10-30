#Cyperaceae removed Stand1####

library(ggplot2)
library(reshape)
library(vegan)

data<-read.csv("Outputs/Stand1_binned.csv", check.names = FALSE)
data<-data[-c(1)]

#columns 55, 158, 353 are metadata
data<-data[c(55, 158, 353, 1:54, 56:157, 159:352, 354:397)]

#Remove Poaceae in column 102
data<-data[-c(102)]

names(data)[2]<-"interval"
data$interval<-gsub("]","",as.character(data$interval))
data$interval<-gsub("\\(|\\)","",as.character(data$interval))

split<-strsplit(data$interval, split = ",")
matrix<-matrix(unlist(split),ncol=2,byrow=T)
matrix<-as.data.frame(matrix)
matrix$V1 <- as.numeric(matrix$V1)
matrix$V2 <- as.numeric(matrix$V2)
matrix$mean_interval_group<-rowMeans(matrix[1:2], na.rm=TRUE)

drop <- c("interval")
data = data[,!(names(data) %in% drop)]
data$mean_interval_age<-matrix$mean_interval_group
data<-data[c(396, 1:395)]

#drop columns when the column sum= 0
meta<-data[1:3]
data<-data[4:396]
data<-Filter(function(data) sum(abs(data), na.rm = TRUE) > 0, data)
data<-cbind(meta,data)

# Build unique pairs to compare each with each
pairs<-expand.grid(unique(data$Site),unique(data$Site))
pairs<-pairs[pairs[,1]!=pairs[,2],]
for(i in 1:nrow(pairs)){	
  if(i>nrow(pairs)){break} else {
    a<-paste(pairs[,1],pairs[,2])
    b<-paste(pairs[,2],pairs[,1])	
    pairs<-pairs[-which(a[i]==b),]
  }}

comp<-list()
for(i in 1:nrow(pairs)){
  a<-data[data$Site ==pairs[i,1],]
  b<-data[data$Site ==pairs[i,2],]	
  times<-intersect(a$mean_interval_age, b$mean_interval_age)
  sims<-times
  sims[]<-NA
  for(d in times){
    sims[times==d]<-1-vegdist(rbind(a[a$mean_interval_age==d,4:367],b[b$mean_interval_age==d,4:367]),method="bray")}
  comp[[i]]<-data.frame(times,sims)
}

names(comp)<-paste(pairs[,1],pairs[,2])

dat<-do.call("rbind", comp)
dat$name<-unlist(lapply(strsplit(rownames(dat),split="[.]"),function(x){x[[1]]}))
head(dat)
#write.csv(dat, "Outputs/Stand1_without_Cyperaceae_dat.csv")

#Cyperaceae removed Stand2####

data<-read.csv("Outputs/Stand2_binned.csv", check.names = FALSE)
data<-data[-c(1)]

#columns 51, 153, 336 are metadata
data<-data[c(51, 153, 336, 1:50, 52:152, 154:335, 337:373)]

#Remove Cyperaceae in column 100
data<-data[-c(100)]

names(data)[2]<-"interval"
data$interval<-gsub("]","",as.character(data$interval))
data$interval<-gsub("\\(|\\)","",as.character(data$interval))

split<-strsplit(data$interval, split = ",")
matrix<-matrix(unlist(split),ncol=2,byrow=T)
matrix<-as.data.frame(matrix)
matrix$V1 <- as.numeric(matrix$V1)
matrix$V2 <- as.numeric(matrix$V2)
matrix$mean_interval_group<-rowMeans(matrix[1:2], na.rm=TRUE)

drop <- c("interval")
data = data[,!(names(data) %in% drop)]
data$mean_interval_age<-matrix$mean_interval_group
data<-data[c(372, 1:371)]

#drop columns when the column sum= 0
meta<-data[1:3]
data<-data[4:372]
data<-Filter(function(data) sum(abs(data), na.rm = TRUE) > 0, data)
data<-cbind(meta,data)

# Build unique pairs to compare each with each
pairs<-expand.grid(unique(data$Site),unique(data$Site))
pairs<-pairs[pairs[,1]!=pairs[,2],]
for(i in 1:nrow(pairs)){	
  if(i>nrow(pairs)){break} else {
    a<-paste(pairs[,1],pairs[,2])
    b<-paste(pairs[,2],pairs[,1])	
    pairs<-pairs[-which(a[i]==b),]
  }}

comp<-list()
for(i in 1:nrow(pairs)){
  a<-data[data$Site ==pairs[i,1],]
  b<-data[data$Site ==pairs[i,2],]	
  times<-intersect(a$mean_interval_age, b$mean_interval_age)
  sims<-times
  sims[]<-NA
  for(d in times){
    sims[times==d]<-1-vegdist(rbind(a[a$mean_interval_age==d,4:349],b[b$mean_interval_age==d,4:349]),method="bray")}
  comp[[i]]<-data.frame(times,sims)
}

names(comp)<-paste(pairs[,1],pairs[,2])

dat<-do.call("rbind", comp)
dat$name<-unlist(lapply(strsplit(rownames(dat),split="[.]"),function(x){x[[1]]}))
head(dat)
#write.csv(dat, "Outputs/Stand2_without_Cyperaceae_dat.csv")

#Poaceae removed Stand1####

data<-read.csv("Outputs/Stand1_binned.csv", check.names = FALSE)
data<-data[-c(1)]

#columns 55, 158, 353 are metadata
data<-data[c(55, 158, 353, 1:54, 56:157, 159:352, 354:397)]

#Remove Poaceae in column 298
data<-data[-c(298)]

names(data)[2]<-"interval"
data$interval<-gsub("]","",as.character(data$interval))
data$interval<-gsub("\\(|\\)","",as.character(data$interval))

split<-strsplit(data$interval, split = ",")
matrix<-matrix(unlist(split),ncol=2,byrow=T)
matrix<-as.data.frame(matrix)
matrix$V1 <- as.numeric(matrix$V1)
matrix$V2 <- as.numeric(matrix$V2)
matrix$mean_interval_group<-rowMeans(matrix[1:2], na.rm=TRUE)

drop <- c("interval")
data = data[,!(names(data) %in% drop)]
data$mean_interval_age<-matrix$mean_interval_group
data<-data[c(396, 1:395)]

#drop columns when the column sum= 0
meta<-data[1:3]
data<-data[4:396]
data<-Filter(function(data) sum(abs(data), na.rm = TRUE) > 0, data)
data<-cbind(meta,data)

# Build unique pairs to compare each with each
pairs<-expand.grid(unique(data$Site),unique(data$Site))
pairs<-pairs[pairs[,1]!=pairs[,2],]
for(i in 1:nrow(pairs)){	
  if(i>nrow(pairs)){break} else {
    a<-paste(pairs[,1],pairs[,2])
    b<-paste(pairs[,2],pairs[,1])	
    pairs<-pairs[-which(a[i]==b),]
  }}

comp<-list()
for(i in 1:nrow(pairs)){
  a<-data[data$Site ==pairs[i,1],]
  b<-data[data$Site ==pairs[i,2],]	
  times<-intersect(a$mean_interval_age, b$mean_interval_age)
  sims<-times
  sims[]<-NA
  for(d in times){
    sims[times==d]<-1-vegdist(rbind(a[a$mean_interval_age==d,4:367],b[b$mean_interval_age==d,4:367]),method="bray")}
  comp[[i]]<-data.frame(times,sims)
}

names(comp)<-paste(pairs[,1],pairs[,2])

dat<-do.call("rbind", comp)
dat$name<-unlist(lapply(strsplit(rownames(dat),split="[.]"),function(x){x[[1]]}))
head(dat)
#write.csv(dat, "Outputs/Stand1_without_Poaceae_dat.csv")

#Poaceae removed Stand2####

data<-read.csv("Outputs/Stand2_binned.csv", check.names = FALSE)
data<-data[-c(1)]

#columns 51, 153, 336 are metadata
data<-data[c(51, 153, 336, 1:50, 52:152, 154:335, 337:373)]

#Remove Poaceae in column 284
data<-data[-c(284)]

names(data)[2]<-"interval"
data$interval<-gsub("]","",as.character(data$interval))
data$interval<-gsub("\\(|\\)","",as.character(data$interval))

split<-strsplit(data$interval, split = ",")
matrix<-matrix(unlist(split),ncol=2,byrow=T)
matrix<-as.data.frame(matrix)
matrix$V1 <- as.numeric(matrix$V1)
matrix$V2 <- as.numeric(matrix$V2)
matrix$mean_interval_group<-rowMeans(matrix[1:2], na.rm=TRUE)

drop <- c("interval")
data = data[,!(names(data) %in% drop)]
data$mean_interval_age<-matrix$mean_interval_group
data<-data[c(372, 1:371)]

#drop columns when the column sum= 0
meta<-data[1:3]
data<-data[4:372]
data<-Filter(function(data) sum(abs(data), na.rm = TRUE) > 0, data)
data<-cbind(meta,data)

# Build unique pairs to compare each with each
pairs<-expand.grid(unique(data$Site),unique(data$Site))
pairs<-pairs[pairs[,1]!=pairs[,2],]
for(i in 1:nrow(pairs)){	
  if(i>nrow(pairs)){break} else {
    a<-paste(pairs[,1],pairs[,2])
    b<-paste(pairs[,2],pairs[,1])	
    pairs<-pairs[-which(a[i]==b),]
  }}

comp<-list()
for(i in 1:nrow(pairs)){
  a<-data[data$Site ==pairs[i,1],]
  b<-data[data$Site ==pairs[i,2],]	
  times<-intersect(a$mean_interval_age, b$mean_interval_age)
  sims<-times
  sims[]<-NA
  for(d in times){
    sims[times==d]<-1-vegdist(rbind(a[a$mean_interval_age==d,4:349],b[b$mean_interval_age==d,4:349]),method="bray")}
  comp[[i]]<-data.frame(times,sims)
}
  
names(comp)<-paste(pairs[,1],pairs[,2])

dat<-do.call("rbind", comp)
dat$name<-unlist(lapply(strsplit(rownames(dat),split="[.]"),function(x){x[[1]]}))
head(dat)
#write.csv(dat, "Outputs/Stand2_without_Poaceae_dat.csv")