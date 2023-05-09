#Similarity analysis####

library(ggplot2)
library(reshape)
library(vegan)

data<-read.csv("mixed_grouped.csv")
drop <- c("X")
data = data[,!(names(data) %in% drop)]

#columns 51, 147, 329 are metadata
data<-data[c(51, 147, 329, 1:50, 52:146, 148:328, 330:367)]

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
data<-data[c(367, 1:366)]

#drop columns when the column sum= 0
meta<-data[1:3]
data<-data[4:367]
data<-Filter(function(data) sum(abs(data), na.rm = TRUE) > 0, data)
data<-cbind(meta,data)

# Build unique pairs to compare each with each####
pairs<-expand.grid(unique(data$Site),unique(data$Site))
pairs<-pairs[pairs[,1]!=pairs[,2],]
for(i in 1:nrow(pairs)){	
  if(i>nrow(pairs)){break} else {
    a<-paste(pairs[,1],pairs[,2])
    b<-paste(pairs[,2],pairs[,1])	
    pairs<-pairs[-which(a[i]==b),]
  }}

#write.csv(pairs, "pairs.csv")

comp<-list()
for(i in 1:nrow(pairs)){
  a<-data[data$Site ==pairs[i,1],]
  b<-data[data$Site ==pairs[i,2],]	
  times<-intersect(a$mean_interval_age, b$mean_interval_age)
  sims<-times
  sims[]<-NA
  for(d in times){
    sims[times==d]<-1-vegdist(rbind(a[a$mean_interval_age==d,4:348],b[b$mean_interval_age==d,4:348]),method="bray")}
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
if(1){
boxplot(homogen,range=0,ylab="Pairwise Bray-Curtis Similarity slope coefficients", las=3, col= "darkorange1")
abline(h=0, col= "steelblue", lwd=2, lty=5)

library('plyr')
freq<-count(data$Site)
freq<-count(data$mean_interval_age)
hist(data$mean_interval_age,
        xlab="Pollen assemblage mean time interval (cal. years BP)", ylab = "Frequency", col = "steelblue", main = NULL)

}

if(1){
  # boxplots ordered by elevation
  homogen <- homogen[, c("Avai’o’vuna Swamp", "Waitetoke", "Volivoli", "Yacata", "Lotofoa Swamp", "Tukou Marsh", "Anouwe Swamp", "Bonatoa Bog", "Ngofe Marsh", "St. Louis Lac", "Finemui Swamp", "Plum Swamp", "Rano Aroi", "Lake Tagimaucia", "Lake Lanoto'o")]
  boxplot(homogen,range=0,ylab="Pairwise Bray-Curtis Similarity slope coefficients", las=3, col= "darkorange1")
  abline(h=0, col= "black", lwd=2, lty=5)
}

par(mfrow=c(1,1))
for(i in unique(data$Site)){
  pie(c(sum(homogen[i,]<0,na.rm=T),sum(homogen[i,]>0,na.rm=T)),main=i,col=c("darkorange1","lightblue"),labels=NA)
  }

names(comp)<-paste(pairs[,1],pairs[,2])

dat<-do.call("rbind", comp)
dat$name<-unlist(lapply(strsplit(rownames(dat),split="[.]"),function(x){x[[1]]}))
head(dat)
#write.csv(dat, "dat.csv")

library(segmented)
m1<-lm(sims ~ times, data=dat)
seg.mod<-segmented(m1) #1 breakpoint for x
plot(seg.mod,ylim=c(0,0.82),lwd=3,xlim=c(5000,1),ylab="Pairwise Bray-Curtis Similarity", xlab="Cal. years BP")
points(sims ~ times, data=dat,pch=16,col="steelblue")
plot(seg.mod,ylim=c(0,1),lwd=4,add=T, col="darkorange1")
plot(seg.mod,ylim=c(0,1),lwd=1,add=T,col="darkorange1", conf.level=0.95, shade = T)
abline(v=3000, col="black", lwd=2, lty=2)

plot(seg.mod,lwd=3,xlim=c(5000,1),ylab="Pairwise Bray-Curtis Similarity", xlab="Cal. years BP")
points(sims ~ times, data=dat,pch=16,col="steelblue")
plot(seg.mod,ylim=c(0,1),lwd=4,add=T, col="darkorange1")
plot(seg.mod,ylim=c(0,1),lwd=1,add=T,col="darkorange1")

library(npreg)
mod <- ss(times, sims, nknots = 5)
plot(mod)

library(npreg)
mod <- ss(dat$times, dat$sims, nknots = 5)
plot(mod)
plot(mod,ylim=c(0.05,0.2),lwd=3,xlim=c(4650,150),ylab="Pairwise Bray-Curtis Similarity", xlab="Cal. years BP")
plot(mod,ylim=c(0,1),lwd=4,add=T, col="darkorange1")
points(sims ~ times, data=dat,pch=16,col="steelblue")

