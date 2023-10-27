#Similarity analysis based on rank####
#only taxa within the same rank are compared

library(ggplot2)
library(reshape)
library(tidyverse)
library(vegan)

#Stand1####
data<-read.csv("Outputs/Stand1_binned.csv", check.names = FALSE)
data<-data[-c(1)]

#columns 55, 158, 353 are metadata
data<-data[c(55, 158, 353, 1:54, 56:157, 159:352, 354:397)]

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
data<-data[c(397, 1:396)]

#drop columns when the column sum= 0
meta<-data[1:3]
data<-data[4:397]
data<-Filter(function(data) sum(abs(data), na.rm = TRUE) > 0, data)
data<-cbind(meta,data)

#Match the taxa names to rank####
# aggregate columns into groups

#family
look_up<- read.csv("Outputs/Stand1_rank.csv", header = F, check.names = FALSE)
look_up<-look_up[-c(1)]
look_up<-t(look_up)
look_up<-as.data.frame(look_up)
colnames(look_up)[colnames(look_up) == "V1"] <- "name"
colnames(look_up)[colnames(look_up) == "V2"] <- "rank"
fam<- look_up[look_up$rank == "family", ]
fam<-fam$name
names.use <- names(data)[(names(data) %in% fam)]
fam.subset <- data[, names.use]
fam.subset<-cbind(meta,fam.subset)

fam.subset<-fam.subset[rowSums(fam.subset[4:79])>0,]

#genus
gen<- look_up[look_up$rank == "genus", ]
gen<-gen$name
names.use <- names(data)[(names(data) %in% gen)]
gen.subset <- data[, names.use]
gen.subset<-cbind(meta,gen.subset)

gen.subset<-gen.subset[rowSums(gen.subset[4:244])>0,]

#species
#sometimes spaces become full stops in the .csv
spe<- look_up[look_up$rank == "species", ]
spe<-spe$name
names.use <- names(data)[(names(data) %in% spe)]
spe.subset <- data[, names.use]
spe.subset<-cbind(meta,spe.subset)

#note that full stops can stop species names from matching
spe.subset<-spe.subset[rowSums(spe.subset[4:51])>0,]

#Fam Similarity analysis ####
# Build unique pairs to compare each with each

data<-fam.subset
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
    sims[times==d]<-1-vegdist(rbind(a[a$mean_interval_age==d,4:79],b[b$mean_interval_age==d,4:79]),method="bray")}
  comp[[i]]<-data.frame(times,sims)
}

unique(data$Site)
sites<-list()
homogen<-matrix(NA,15,15)
colnames(homogen)<-unique(data$Site)
rownames(homogen)<-unique(data$Site)
R<-N<-homogen

for(i in unique(data$Site))
  abfrage<-pairs[,1] %in% i | pairs[,2] %in% i
unique(data$Site)

for(e in which(abfrage)){
  homogen[i,pairs[e,which(pairs[e,]!=i)]]<-coef(lm(sims~times,data=comp[[e]]))[2]
  N[i,pairs[e,which(pairs[e,]!=i)]]<-nrow(comp[[e]])
  R[i,pairs[e,which(pairs[e,]!=i)]]<-summary(lm(sims~times,data=comp[[e]]))$adj	
}	

names(comp)<-paste(pairs[,1],pairs[,2])

dat.fam<-do.call("rbind", comp)
dat.fam$name<-unlist(lapply(strsplit(rownames(dat.fam),split="[.]"),function(x){x[[1]]}))

#Gen Similarity analysis ####
# Build unique pairs to compare each with each

data<-gen.subset
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
    sims[times==d]<-1-vegdist(rbind(a[a$mean_interval_age==d,4:244],b[b$mean_interval_age==d,4:244]),method="bray")}
  comp[[i]]<-data.frame(times,sims)
}

unique(data$Site)
sites<-list()
homogen<-matrix(NA,15,15)
colnames(homogen)<-unique(data$Site)
rownames(homogen)<-unique(data$Site)
R<-N<-homogen

for(i in unique(data$Site))
  abfrage<-pairs[,1] %in% i | pairs[,2] %in% i
unique(data$Site)

for(e in which(abfrage)){
  homogen[i,pairs[e,which(pairs[e,]!=i)]]<-coef(lm(sims~times,data=comp[[e]]))[2]
  N[i,pairs[e,which(pairs[e,]!=i)]]<-nrow(comp[[e]])
  R[i,pairs[e,which(pairs[e,]!=i)]]<-summary(lm(sims~times,data=comp[[e]]))$adj	
}	

names(comp)<-paste(pairs[,1],pairs[,2])

dat.gen<-do.call("rbind", comp)
dat.gen$name<-unlist(lapply(strsplit(rownames(dat.gen),split="[.]"),function(x){x[[1]]}))

#Spe Similarity analysis ####
# Build unique pairs to compare each with each

data<-spe.subset
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
    sims[times==d]<-1-vegdist(rbind(a[a$mean_interval_age==d,4:51],b[b$mean_interval_age==d,4:51]),method="bray")}
  comp[[i]]<-data.frame(times,sims)
}

unique(data$Site)
sites<-list()
#IMPORTANT once rows=0 have been removed####
#there may be <15 sites
homogen<-matrix(NA,13,13)
colnames(homogen)<-unique(data$Site)
rownames(homogen)<-unique(data$Site)
R<-N<-homogen

for(i in unique(data$Site))
  abfrage<-pairs[,1] %in% i | pairs[,2] %in% i
unique(data$Site)

names(comp)<-paste(pairs[,1],pairs[,2])

dat.spe<-do.call("rbind", comp)
dat.spe$name<-unlist(lapply(strsplit(rownames(dat.spe),split="[.]"),function(x){x[[1]]}))

#Combine fam, gen, and spe####

dat.fam$rank<-"family"
dat.gen$rank<-"genus"
dat.spe$rank<-"species"

dat<-rbind(dat.fam, dat.gen)
dat<-rbind(dat, dat.spe)
#write.csv(dat, "Outputs/Stand1_by_rank_dat.csv")

#Stand2####
data<-read.csv("Outputs/Stand2_binned.csv", check.names = FALSE)
data<-data[-c(1)]

#columns 51, 153, 336 are metadata
data<-data[c(51, 153, 336, 1:50, 52:152, 154:335, 337:373)]

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
data<-data[c(373, 1:372)]

#drop columns when the column sum= 0
meta<-data[1:3]
data<-data[4:373]
data<-Filter(function(data) sum(abs(data), na.rm = TRUE) > 0, data)
data<-cbind(meta,data)

#Match the taxa names to rank####
# aggregate columns into groups

#family
look_up<- read.csv("Outputs/Stand2_rank.csv", header = F, check.names = FALSE)
look_up<-look_up[-c(1)]
look_up<-t(look_up)
look_up<-as.data.frame(look_up)
colnames(look_up)[colnames(look_up) == "V1"] <- "name"
colnames(look_up)[colnames(look_up) == "V2"] <- "rank"
fam<- look_up[look_up$rank == "family", ]
fam<-fam$name
names.use <- names(data)[(names(data) %in% fam)]
fam.subset <- data[, names.use]
fam.subset<-cbind(meta,fam.subset)

fam.subset<-fam.subset[rowSums(fam.subset[4:99])>0,]

#genus
gen<- look_up[look_up$rank == "genus", ]
gen<-gen$name
names.use <- names(data)[(names(data) %in% gen)]
gen.subset <- data[, names.use]
gen.subset<-cbind(meta,gen.subset)

gen.subset<-gen.subset[rowSums(gen.subset[4:214])>0,]

#species
#sometimes spaces become full stops in the .csv
spe<- look_up[look_up$rank == "species", ]
spe<-spe$name
names.use <- names(data)[(names(data) %in% spe)]
spe.subset <- data[, names.use]
spe.subset<-cbind(meta,spe.subset)

#note that full stops can stop species names from matching
spe.subset<-spe.subset[rowSums(spe.subset[4:43])>0,]

#Fam Similarity analysis ####
# Build unique pairs to compare each with each

data<-fam.subset
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
    sims[times==d]<-1-vegdist(rbind(a[a$mean_interval_age==d,4:99],b[b$mean_interval_age==d,4:99]),method="bray")}
  comp[[i]]<-data.frame(times,sims)
}

unique(data$Site)
sites<-list()
homogen<-matrix(NA,15,15)
colnames(homogen)<-unique(data$Site)
rownames(homogen)<-unique(data$Site)
R<-N<-homogen

for(i in unique(data$Site))
  abfrage<-pairs[,1] %in% i | pairs[,2] %in% i
  unique(data$Site)
  
  for(e in which(abfrage)){
    homogen[i,pairs[e,which(pairs[e,]!=i)]]<-coef(lm(sims~times,data=comp[[e]]))[2]
    N[i,pairs[e,which(pairs[e,]!=i)]]<-nrow(comp[[e]])
    R[i,pairs[e,which(pairs[e,]!=i)]]<-summary(lm(sims~times,data=comp[[e]]))$adj	
  }	

names(comp)<-paste(pairs[,1],pairs[,2])

dat.fam<-do.call("rbind", comp)
dat.fam$name<-unlist(lapply(strsplit(rownames(dat.fam),split="[.]"),function(x){x[[1]]}))

#Gen Similarity analysis ####
# Build unique pairs to compare each with each

data<-gen.subset
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
    sims[times==d]<-1-vegdist(rbind(a[a$mean_interval_age==d,4:214],b[b$mean_interval_age==d,4:214]),method="bray")}
  comp[[i]]<-data.frame(times,sims)
}

unique(data$Site)
sites<-list()
homogen<-matrix(NA,15,15)
colnames(homogen)<-unique(data$Site)
rownames(homogen)<-unique(data$Site)
R<-N<-homogen

for(i in unique(data$Site))
  abfrage<-pairs[,1] %in% i | pairs[,2] %in% i
  unique(data$Site)
  
  for(e in which(abfrage)){
    homogen[i,pairs[e,which(pairs[e,]!=i)]]<-coef(lm(sims~times,data=comp[[e]]))[2]
    N[i,pairs[e,which(pairs[e,]!=i)]]<-nrow(comp[[e]])
    R[i,pairs[e,which(pairs[e,]!=i)]]<-summary(lm(sims~times,data=comp[[e]]))$adj	
  }	
  
  names(comp)<-paste(pairs[,1],pairs[,2])
  
  dat.gen<-do.call("rbind", comp)
  dat.gen$name<-unlist(lapply(strsplit(rownames(dat.gen),split="[.]"),function(x){x[[1]]}))
  
  #Spe Similarity analysis ####
  # Build unique pairs to compare each with each
  
  data<-spe.subset
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
      sims[times==d]<-1-vegdist(rbind(a[a$mean_interval_age==d,4:43],b[b$mean_interval_age==d,4:43]),method="bray")}
    comp[[i]]<-data.frame(times,sims)
  }
  
  unique(data$Site)
  sites<-list()
  #IMPORTANT once rows=0 have been removed####
  #there may be <15 sites
  homogen<-matrix(NA,11,11)
  colnames(homogen)<-unique(data$Site)
  rownames(homogen)<-unique(data$Site)
  R<-N<-homogen
  
  for(i in unique(data$Site))
    abfrage<-pairs[,1] %in% i | pairs[,2] %in% i
  unique(data$Site)
  
  names(comp)<-paste(pairs[,1],pairs[,2])
  
  dat.spe<-do.call("rbind", comp)
  dat.spe$name<-unlist(lapply(strsplit(rownames(dat.spe),split="[.]"),function(x){x[[1]]}))
  
  #Combine fam, gen, and spe####
  
  dat.fam$rank<-"family"
  dat.gen$rank<-"genus"
  dat.spe$rank<-"species"
  
  dat<-rbind(dat.fam, dat.gen)
  dat<-rbind(dat, dat.spe)
  #write.csv(dat, "Outputs/Stand2_by_rank_dat.csv")