#merge datasets together
library(dplyr)

loui<-read.csv("Pollen_data_with_ages/loui_pollen_ages.csv", check.names = FALSE)
loui<-loui[-c(1)]
loui$site<-"loui"

plum<-read.csv("Pollen_data_with_ages/plum_pollen_ages.csv", check.names = FALSE)
plum<-plum[-c(1)]
plum$site<-"plum"

anou<-read.csv("Pollen_data_with_ages/anou_pollen_ages.csv", check.names = FALSE)
anou<-anou[-c(1)]
anou$site<-"anou"

wait<-read.csv("Pollen_data_with_ages/wait_pollen_ages.csv", check.names = FALSE)
wait<-wait[-c(1)]
wait$site<-"wait"

voli<-read.csv("Pollen_data_with_ages/voli_pollen_ages.csv", check.names = FALSE)
voli<-voli[-c(1)]
voli$site<-"voli"

bona<-read.csv("Pollen_data_with_ages/bona_pollen_ages.csv", check.names = FALSE)
bona<-bona[-c(1)]
bona$site<-"bona"

tagi<-read.csv("Pollen_data_with_ages/tagi_pollen_ages.csv", check.names = FALSE)
tagi<-tagi[-c(1)]
tagi$site<-"tagi"

yaca<-read.csv("Pollen_data_with_ages/yaca_pollen_ages.csv", check.names = FALSE)
yaca<-yaca[-c(1)]
yaca$site<-"yaca"

fine<-read.csv("Pollen_data_with_ages/fine_pollen_ages.csv", check.names = FALSE)
fine<-fine[-c(1)]
fine$site<-"fine"

loto<-read.csv("Pollen_data_with_ages/loto_pollen_ages.csv", check.names = FALSE)
loto<-loto[-c(1)]
loto$site<-"loto"

ngof<-read.csv("Pollen_data_with_ages/ngof_pollen_ages.csv", check.names = FALSE)
ngof<-ngof[-c(1)]
ngof$site<-"ngof"

avai<-read.csv("Pollen_data_with_ages/avai_pollen_ages.csv", check.names = FALSE)
avai<-avai[-c(1)]
avai$site<-"avai"

lano<-read.csv("Pollen_data_with_ages/lano_pollen_ages.csv", check.names = FALSE)
lano<-lano[-c(1)]
lano$site<-"lano"

tuko<-read.csv("Pollen_data_with_ages/tuko_pollen_ages.csv", check.names = FALSE)
tuko<-tuko[-c(1)]
tuko$site<-"tuko"

aroi<-read.csv("Pollen_data_with_ages/aroi_pollen_ages.csv", check.names = FALSE)
aroi<-aroi[-c(1)]
aroi$site<-"aroi"

#merge
df_list<-list(anou,aroi,avai,bona,fine,lano,loto,loui,ngof,plum,tagi,tuko,voli,wait,yaca)

#merge all data frames together
mixed<-Reduce(function(x, y) merge(x, y, all=TRUE), df_list) 

mixed <- mixed %>% replace(is.na(.), 0)

#aggregate
meta<-mixed[c(1:4)]
mixed<-mixed[-c(1:4)]
mixed2<-mixed

#Standardisation 2####
#match original names to min and max change names
# aggregate columns into groups
if(1){
  har_table<- read.csv("Outputs/harmonisation.csv", check.names = FALSE)
  names(mixed) <- har_table$standardisation_2[match(names(mixed), har_table$original)]
  mixed<-as.data.frame(do.call(cbind,
                               by(t(mixed),INDICES=names(mixed),FUN=colSums)))
  #remove remove column
  drop <- c("remove")
  mixed = mixed[,!(names(mixed) %in% drop)]
  #merge with meta data again
  mixed<-cbind(meta, mixed)
  #write.csv(mixed, "Outputs/Stand2_unbinned.csv")
}

#Stanardisation 1####
if(1){
  har_table<- read.csv("Outputs/harmonisation.csv", check.names = FALSE)
  names(mixed2) <- har_table$standardisation_1[match(names(mixed2), har_table$original)]
  mixed<-as.data.frame(do.call(cbind,
                               by(t(mixed2),INDICES=names(mixed2),FUN=colSums)))
  #remove remove column
  drop <- c("remove")
  mixed = mixed[,!(names(mixed) %in% drop)]
  #merge with meta data again
  mixed<-cbind(meta, mixed)
  #write.csv(mixed, "Outputs/Stand1_unbinned.csv")
}
