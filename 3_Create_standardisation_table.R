#Produce a table which forms the basis of the
#standardisation of taxa names####

#1. St Louis Lac####
loui<-read.csv("Pollen_data_with_ages/loui_pollen_ages.csv", check.names = FALSE)
drop <- c("depth", "Cal_yrs_BP", "min", "max")
loui = loui[,!(names(loui) %in% drop)]
loui <- as.data.frame(colnames(loui[,1:ncol(loui)]))
names(loui)[1]<-"original"

#2. Plum Swamp####
plum<-read.csv("Pollen_data_with_ages/plum_pollen_ages.csv", check.names = FALSE)
plum = plum[,!(names(plum) %in% drop)]
plum <- as.data.frame(colnames(plum[,1:ncol(plum)]))
names(plum)[1]<-"original"

#3. Anouwe Swamp####
anou<-read.csv("Pollen_data_with_ages/anou_pollen_ages.csv", check.names = FALSE)
anou = anou[,!(names(anou) %in% drop)]
anou <- as.data.frame(colnames(anou[,1:ncol(anou)]))
names(anou)[1]<-"original"

#4. Waitetoke####
wait<-read.csv("Pollen_data_with_ages/wait_pollen_ages.csv", check.names = FALSE)
wait = wait[,!(names(wait) %in% drop)]
wait <- as.data.frame(colnames(wait[,1:ncol(wait)]))
names(wait)[1]<-"original"

#5. Volivoli####
voli<-read.csv("Pollen_data_with_ages/voli_pollen_ages.csv", check.names = FALSE)
voli = voli[,!(names(voli) %in% drop)]
voli <- as.data.frame(colnames(voli[,1:ncol(voli)]))
names(voli)[1]<-"original"

#6. Bonatoa Bog####
bona<-read.csv("Pollen_data_with_ages/bona_pollen_ages.csv", check.names = FALSE)
bona = bona[,!(names(bona) %in% drop)]
bona <- as.data.frame(colnames(bona[,1:ncol(bona)]))
names(bona)[1]<-"original"

#7. Lake Tagimaucia####
tagi<-read.csv("Pollen_data_with_ages/tagi_pollen_ages.csv", check.names = FALSE)
tagi = tagi[,!(names(tagi) %in% drop)]
tagi <- as.data.frame(colnames(tagi[,1:ncol(tagi)]))
names(tagi)[1]<-"original"

#8. Yacata####
yaca<-read.csv("Pollen_data_with_ages/yaca_pollen_ages.csv", check.names = FALSE)
yaca = yaca[,!(names(yaca) %in% drop)]
yaca <- as.data.frame(colnames(yaca[,1:ncol(yaca)]))
names(yaca)[1]<-"original"

#9. Finemui Swamp####
fine<-read.csv("Pollen_data_with_ages/fine_pollen_ages.csv", check.names = FALSE)
fine = fine[,!(names(fine) %in% drop)]
fine <- as.data.frame(colnames(fine[,1:ncol(fine)]))
names(fine)[1]<-"original"

#10. Lotofoa Swamp####
loto<-read.csv("Pollen_data_with_ages/loto_pollen_ages.csv", check.names = FALSE)
loto = loto[,!(names(loto) %in% drop)]
loto <- as.data.frame(colnames(loto[,1:ncol(loto)]))
names(loto)[1]<-"original"

#11. Ngofe Marsh####
ngof<-read.csv("Pollen_data_with_ages/ngof_pollen_ages.csv", check.names = FALSE)
ngof = ngof[,!(names(ngof) %in% drop)]
ngof <- as.data.frame(colnames(ngof[,1:ncol(ngof)]))
names(ngof)[1]<-"original"

#12. Avai'o'vuna Swamp####
avai<-read.csv("Pollen_data_with_ages/avai_pollen_ages.csv", check.names = FALSE)
avai = avai[,!(names(avai) %in% drop)]
avai <- as.data.frame(colnames(avai[,1:ncol(avai)]))
names(avai)[1]<-"original"

#13. Lake Lanoto'o####
lano<-read.csv("Pollen_data_with_ages/lano_pollen_ages.csv", check.names = FALSE)
lano = lano[,!(names(lano) %in% drop)]
lano <- as.data.frame(colnames(lano[,1:ncol(lano)]))
names(lano)[1]<-"original"

#14. Tukou Marsh####
tuko<-read.csv("Pollen_data_with_ages/tuko_pollen_ages.csv", check.names = FALSE)
tuko = tuko[,!(names(tuko) %in% drop)]
tuko <- as.data.frame(colnames(tuko[,1:ncol(tuko)]))
names(tuko)[1]<-"original"

#15. Rano Aroi####
aroi<-read.csv("Pollen_data_with_ages/aroi_pollen_ages.csv", check.names = FALSE)
aroi = aroi[,!(names(aroi) %in% drop)]
aroi <- as.data.frame(colnames(aroi[,1:ncol(aroi)]))
names(aroi)[1]<-"original"

#combine####
har_table<-rbind(loui,plum,anou,wait,voli,bona,tagi,yaca,fine,loto,ngof,avai,lano,tuko,aroi)

#remove duplicate names####
library(tidyverse)
har_table<-har_table$original[!duplicated(har_table$original)]
har_table<-as.data.frame(har_table)
names(har_table)[1]<-"original"
#write.csv(har_table, "Outputs/harmonisation.csv")
#I then added updated taxa names in the excel csv
