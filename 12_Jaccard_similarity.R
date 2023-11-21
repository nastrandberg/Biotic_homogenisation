#Similarity analysis using Jaccard index instead of Bray-Curtis,
#binary=T for presence/absence data
#Note that the Bray-Curtis results were used in the main paper and SI
#This script is only for data exploration

#Similarity analysis of standardisation 1####

library(ggplot2)
library(reshape)
library(vegan)

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
    sims[times==d]<-1-vegdist(rbind(a[a$mean_interval_age==d,4:368],b[b$mean_interval_age==d,4:368]),method="jaccard", binary=T)}
  comp[[i]]<-data.frame(times,sims)
}

unique(data$Site)
sites<-list()
homogen<-matrix(NA,15,15)
colnames(homogen)<-unique(data$Site)
rownames(homogen)<-unique(data$Site)
R<-N<-homogen

for(i in unique(data$Site)){
  abfrage<-pairs[,1] %in% i | pairs[,2] %in% i
  unique(data$Site)
  
  for(e in which(abfrage)){
    homogen[i,pairs[e,which(pairs[e,]!=i)]]<-coef(lm(sims~times,data=comp[[e]]))[2]
    N[i,pairs[e,which(pairs[e,]!=i)]]<-nrow(comp[[e]])
    R[i,pairs[e,which(pairs[e,]!=i)]]<-summary(lm(sims~times,data=comp[[e]]))$adj	
  }	
}

par(mfrow=c(1,1))
plot(sims~times,data=comp[[3]])
abline(lm(sims~times,data=comp[[3]]))
abline(h=mean(comp[[3]]$sims),col="red")
# negative trend is homogenisation
# boxplots ordered by longitude
par(mfrow=c(1,1))
homogen <- homogen[, c("lou", "plu", "ano", "wai", "vol", "bon", "tag", "yac", "fin", "lot", "ngo", "ava", "lan", "tuk", "aro")]
cols<- c("St. Louis Lac", "Plum Swamp", "Anouwe Swamp", "Waitetoke", "Volivoli", "Bonatoa Bog","Lake Tagimaucia","Yacata","Finemui Swamp","Lotofoa Swamp","Ngofe Marsh","Avai’o’vuna Swamp","Lake Lanoto'o","Tukou Marsh","Rano Aroi")
colnames(homogen) <- cols

names(comp)<-paste(pairs[,1],pairs[,2])

dat<-do.call("rbind", comp)
dat$name<-unlist(lapply(strsplit(rownames(dat),split="[.]"),function(x){x[[1]]}))
head(dat)
#write.csv(dat, "Outputs/Stand1_jaccard_dat.csv")

#Similarity analysis of standardisation 2####

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
    sims[times==d]<-1-vegdist(rbind(a[a$mean_interval_age==d,4:350],b[b$mean_interval_age==d,4:350]),method="jaccard", binary=T)}
  comp[[i]]<-data.frame(times,sims)
}

unique(data$Site)
sites<-list()
homogen<-matrix(NA,15,15)
colnames(homogen)<-unique(data$Site)
rownames(homogen)<-unique(data$Site)
R<-N<-homogen

for(i in unique(data$Site)){
  abfrage<-pairs[,1] %in% i | pairs[,2] %in% i
  unique(data$Site)
  
  for(e in which(abfrage)){
    homogen[i,pairs[e,which(pairs[e,]!=i)]]<-coef(lm(sims~times,data=comp[[e]]))[2]
    N[i,pairs[e,which(pairs[e,]!=i)]]<-nrow(comp[[e]])
    R[i,pairs[e,which(pairs[e,]!=i)]]<-summary(lm(sims~times,data=comp[[e]]))$adj	
  }	
}

par(mfrow=c(1,1))
plot(sims~times,data=comp[[3]])
abline(lm(sims~times,data=comp[[3]]))
abline(h=mean(comp[[3]]$sims),col="red")
# negative trend is homogenisation
# boxplots ordered by longitude
par(mfrow=c(1,1))
homogen <- homogen[, c("lou", "plu", "ano", "wai", "vol", "bon", "tag", "yac", "fin", "lot", "ngo", "ava", "lan", "tuk", "aro")]
cols<- c("St. Louis Lac", "Plum Swamp", "Anouwe Swamp", "Waitetoke", "Volivoli", "Bonatoa Bog","Lake Tagimaucia","Yacata","Finemui Swamp","Lotofoa Swamp","Ngofe Marsh","Avai’o’vuna Swamp","Lake Lanoto'o","Tukou Marsh","Rano Aroi")
colnames(homogen) <- cols

names(comp)<-paste(pairs[,1],pairs[,2])

dat<-do.call("rbind", comp)
dat$name<-unlist(lapply(strsplit(rownames(dat),split="[.]"),function(x){x[[1]]}))
head(dat)
#write.csv(dat, "Outputs/Stand2_jaccard_dat.csv")

#Figures####

library(npreg)
library(dplyr)
library(tidyverse)
library(ggplot2)

#Smooth splines####
dat1<-read.csv("Outputs/Stand1_jaccard_dat.csv", check.names = FALSE)
dat2<-read.csv("Outputs/Stand2_jaccard_dat.csv", check.names = FALSE)

mod1 <- ss(dat1$times, dat1$sims, nknots = 5)
mod2 <- ss(dat2$times, dat2$sims, nknots = 5)

names(mod1)

plot(mod2,level=0.95, lwd=3,xlim=c(4650,150),ylim=c(0,1),ylab="Pairwise Jaccard Similarity", xlab="Cal. years BP", col="royalblue4")
points(dat1$sims ~ dat1$times,pch=21,col=alpha("royalblue4",0.4))
points(dat2$sims ~ dat2$times,pch=24,col=alpha("royalblue1",0.4))
lines(mod2, lwd=3,xlim=c(4650,150),ylim=c(0,1), col="royalblue1")
lines(mod1, lwd=3,xlim=c(4650,150),ylim=c(0,1), col="royalblue4")
#can only display 1 conf. interval so the two
#were merged in Illustrator later

#Standardisation 1 boxplot####
dat<-read.csv("Outputs/Stand1_jaccard_dat.csv")

dat<-dat[-c(1)]

dat$group <- cut(dat$times, breaks = c(-Inf, 650, 3150, Inf), 
                 labels = c("both", "tbc", "neither"))

tbc<- dat[dat$group == "tbc", ]
both<- dat[dat$group == "both", ]
neither<- dat[dat$group == "neither", ]

tbc<-tbc[-c(4)]

tbc$group <- case_when(
  grepl("^(?:aro|tuk|wai) (aro|tuk|wai)$",
        tbc$name, ignore.case=TRUE) ~ "neither",
  grepl("\\b(?:ano|tuk|wai)\\b", tbc$name, ignore.case=TRUE) ~ "one",
  TRUE ~ "both")  

dat<-rbind(both,neither,tbc)

levels(dat$group)
dat$group <- factor(dat$group , levels=c("neither", "one", "both"))
boxplot(sims ~ group, data = dat, xlab = "Proportion of pair settled by humans",
        ylab = "Pairwise Jaccard Similarity",
        col="royalblue4", outpch=21,outbg=alpha("royalblue4",0.2))

stand1<-dat
stand1$type<-"Standardisation 1"

if(1){
  #Standardisation 2 boxplot####
  par(mfrow = c(1, 1))
  dat<-read.csv("Outputs/Stand2_jaccard_dat.csv")
  dat<-dat[-c(1)]
  
  dat$group <- cut(dat$times, breaks = c(-Inf, 650, 3150, Inf), 
                   labels = c("both", "tbc", "neither"))
  
  tbc<- dat[dat$group == "tbc", ]
  both<- dat[dat$group == "both", ]
  neither<- dat[dat$group == "neither", ]
  
  tbc<-tbc[-c(4)]
  
  tbc$group <- case_when(
    grepl("^(?:aro|tuk|wai) (aro|tuk|wai)$",
          tbc$name, ignore.case=TRUE) ~ "neither",
    grepl("\\b(?:ano|tuk|wai)\\b", tbc$name, ignore.case=TRUE) ~ "one",
    TRUE ~ "both")  
  
  dat<-rbind(both,neither,tbc)
  
  levels(dat$group)
  dat$group <- factor(dat$group , levels=c("neither", "one", "both"))
  
  boxplot(sims ~ group, data = dat, xlab = "Proportion of pair settled by humans",
          ylab = "Pairwise Jaccard Similarity",
          col="royalblue1", outpch=24,outbg=alpha("royalblue1",0.2))
  
  df<-dat %>%
    group_by(name,group) %>%
    dplyr::summarize(n())
  
  df<-df %>%
    group_by(name) %>%
    dplyr::summarize(n())
  
  stand2<-dat
  stand2$type<-"Standardisation 2"
}

data<-rbind(stand1, stand2)
