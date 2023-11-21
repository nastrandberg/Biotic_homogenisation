#This script was used to make Fig. 3.
library(npreg)
library(dplyr)
library(tidyverse)
library(ggplot2)

#Smooth splines####
dat1<-read.csv("Outputs/Stand1_dat.csv", check.names = FALSE)
dat2<-read.csv("Outputs/Stand2_dat.csv", check.names = FALSE)

mod1 <- ss(dat1$times, dat1$sims, nknots = 5)
mod2 <- ss(dat2$times, dat2$sims, nknots = 5)

names(mod1)

par(mfrow = c(1, 2))
plot(mod2,level=0.95, lwd=3,xlim=c(4650,150),ylim=c(0,1),ylab="Pairwise Bray-Curtis Similarity", xlab="Cal. years BP", col="royalblue4")
points(dat1$sims ~ dat1$times,pch=21,col=alpha("royalblue4",0.4))
points(dat2$sims ~ dat2$times,pch=24,col=alpha("royalblue1",0.4))
lines(mod2, lwd=3,xlim=c(4650,150),ylim=c(0,1), col="royalblue1")
lines(mod1, lwd=3,xlim=c(4650,150),ylim=c(0,1), col="royalblue4")
#can only display 1 conf. interval so the two
#were merged together later in Illustrator

#Standardisation 1 boxplot####
dat<-read.csv("Outputs/Stand1_dat.csv")

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
        ylab = "Pairwise Bray-Curtis Similarity",
        col="royalblue4", outpch=21,outbg=alpha("royalblue4",0.2))

stand1<-dat
stand1$type<-"Standardisation 1"

if(1){
#Standardisation 2 boxplot####
par(mfrow = c(1, 1))
dat<-read.csv("Outputs/Stand2_dat.csv")
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
        ylab = "Pairwise Bray-Curtis Similarity",
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
#write.csv(data, "Outputs/settlement_boxplot_data.csv")

#Grouped boxplot####
if(1){
data<-read.csv("Outputs/settlement_boxplot_data.csv")
data<-data[-c(1)]

data$order <- factor(data$order , levels=c("neither (Stand. 1)", "neither (Stand. 2)", "one (Stand. 1)", "one (Stand. 2)", "both (Stand. 1)", "both (Stand. 2)"))
boxplot(sims ~ order, data = data, las=3,
        col = c("#27408B","#4876ff"),
        pch= c(21, 24))
}

