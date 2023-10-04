#Similarity analysis of standardisation 2

library(ggplot2)
library(reshape)
library(vegan)

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
data<-data[4:372]
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
library('plyr')
freq<-count(data$Site)
freq<-count(data$mean_interval_age)
hist(data$mean_interval_age,
        xlab="Pollen assemblage mean time interval (cal. years BP)", ylab = "Frequency", col = "steelblue", main = NULL)

}

#write.csv(homogen, "Outputs/Stand2_homogen.csv")

if(1){
  # boxplots ordered by elevation
  homogen <- homogen[, c("Avai’o’vuna Swamp", "Waitetoke", "Volivoli", "Yacata", "Lotofoa Swamp", "Tukou Marsh", "Anouwe Swamp", "Bonatoa Bog", "Ngofe Marsh", "St. Louis Lac", "Finemui Swamp", "Plum Swamp", "Rano Aroi", "Lake Tagimaucia", "Lake Lanoto'o")]
  boxplot(homogen,range=0,ylab="Pairwise Bray-Curtis Similarity slope coefficients", las=3, col= "royalblue1")
  abline(h=0, col= "gold", lwd=2, lty=5)
}

par(mfrow=c(1,1))
for(i in unique(data$Site)){
  pie(c(sum(homogen[i,]<0,na.rm=T),sum(homogen[i,]>0,na.rm=T)),main=i,col=c("royalblue1","gold"),labels=NA)
  }

names(comp)<-paste(pairs[,1],pairs[,2])

dat<-do.call("rbind", comp)
dat$name<-unlist(lapply(strsplit(rownames(dat),split="[.]"),function(x){x[[1]]}))
head(dat)
#write.csv(dat, "Outputs/stand2_dat.csv")

#Calculate the slope coefficient with the min number of data points
minimum_points<-dat$name
min(minimum_points)