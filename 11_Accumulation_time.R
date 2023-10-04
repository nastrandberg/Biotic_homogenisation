#Comparing accumulation times with total and rarefied pollen richness

library(dplyr)

data<-read.csv("Outputs/mixed_original.csv", check.names = FALSE)
data<-data[-c(1)]
drop <- c("min", "max")
data = data[,!(names(data) %in% drop)]
names(data)[1]<-"mean"

par(mfrow = c(3, 3))
#St. Louis Lac
loui<- data[data$Site == "St. Louis Lac", ]
loui_age<-read.csv("Bacon_runs/St_Louis_Lac/Loui_accum.csv")
plot(loui_age$depth, loui_age$accum_time)
loui_age<-loui_age[-c(1)]
loui<-loui[-c(2)]
loui<-merge(loui, loui_age, by="mean")
loui<-loui[-c(1)]
accum_time<-loui$accum_time
loui<-loui[-c(718)]
alpha <- with(loui, specnumber(loui))
alpha<-as.data.frame(alpha)
accum_time<-as.data.frame(accum_time)
loui<-cbind(alpha, accum_time)

lou<-dplyr::filter(data, Site == "St. Louis Lac")
(raremax_lou <- min(rowSums(lou[c(3:719)])))
rare_lou<-rarefy(lou[c(3:719)],raremax_lou)
rare_lou<-as.data.frame(rare_lou)
loui<-cbind(loui, rare_lou)

#Ngofe Marsh
ngof<- data[data$Site == "Ngofe Marsh", ]
ngof_age<-read.csv("Bacon_runs/Ngofe_dates/Ngofe_accum.csv")
plot(ngof_age$depth, ngof_age$accum_time)
ngof_age<-ngof_age[-c(1)]
ngof<-ngof[-c(2)]
ngof<-merge(ngof, ngof_age, by="mean")
ngof<-ngof[-c(1)]
accum_time<-ngof$accum_time
ngof<-ngof[-c(718)]
alpha <- with(ngof, specnumber(ngof))
alpha<-as.data.frame(alpha)
accum_time<-as.data.frame(accum_time)
ngof<-cbind(alpha, accum_time)

ngo<-dplyr::filter(data, Site == "Ngofe Marsh")
(raremax_ngo <- min(rowSums(ngo[c(3:719)])))
rare_ngo<-rarefy(ngo[c(3:719)],raremax_ngo)
rare_ngo<-as.data.frame(rare_ngo)
ngof<-cbind(ngof, rare_ngo)

#Lake Lanoto'o
lano<- data[data$Site == "Lake Lanoto'o", ]
lano_age<-read.csv("Bacon_runs/Lake_Lanotoo/Lake_Lanotoo_accum.csv")
plot(lano_age$depth, lano_age$accum_time)
lano_age<-lano_age[-c(1)]
lano<-lano[-c(2)]
lano<-merge(lano, lano_age, by="mean")
lano<-lano[-c(1)]
accum_time<-lano$accum_time
lano<-lano[-c(718)]
alpha <- with(lano, specnumber(lano))
alpha<-as.data.frame(alpha)
accum_time<-as.data.frame(accum_time)
lano<-cbind(alpha, accum_time)

lan<-dplyr::filter(data, Site == "Lake Lanoto'o")
(raremax_lan <- min(rowSums(lan[c(3:719)])))
rare_lan<-rarefy(lan[c(3:719)],raremax_lan)
rare_lan<-as.data.frame(rare_lan)
lano<-cbind(lano, rare_lan)

#plot###

plot(loui$alpha, loui$accum_time)
plot(ngof$alpha, ngof$accum_time)
plot(lano$alpha, lano$accum_time)

plot(loui$rare_lou, loui$accum_time)
plot(ngof$rare_ngo, ngof$accum_time)
plot(lano$rare_lan, lano$accum_time)
