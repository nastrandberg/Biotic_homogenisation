#Binning of the ungrouped mixed taxa into 500 year intervals####
#after pollen samples with low counts have been removed.

#Binning of the ungrouped mixed taxa into 500 year intervals####

library(dplyr)
library(ggplot2)
library(vegan)

#Stand2####
data<-read.csv("Outputs/Stand2_unbinned_int.csv", check.names = FALSE)
data<-data[-c(1,3:5)]

#remove rows if they add up to <300
keep <- rowSums(data[,3:372]) >= 300
mixed <- data[keep,]

Cal_yrs_BP<- mixed$Cal_yrs_BP
Site<- mixed$site
meta_data <- c("Cal_yrs_BP", "site")
mixed = mixed[,!(names(mixed) %in% meta_data)]

#convert to percentages
mixed<- mixed / rowSums(mixed) * 100

#Add age information
mixed$Cal_yrs_BP <-Cal_yrs_BP

#Add site information
mixed$Site <-Site

#split into new dataframe subsets
lou_mixed<- mixed[mixed$Site == "loui", ]
plu_mixed<- mixed[mixed$Site == "plum", ]
ano_mixed<- mixed[mixed$Site == "anou", ] #no obs
wai_mixed<- mixed[mixed$Site == "wait", ]
vol_mixed<- mixed[mixed$Site == "voli", ]
bon_mixed<- mixed[mixed$Site == "bona", ]
tag_mixed<- mixed[mixed$Site == "tagi", ]
yac_mixed<- mixed[mixed$Site == "yaca", ]
#fin_mixed<- mixed[mixed$Site == "fine", ] #% data
#lot_mixed<- mixed[mixed$Site == "loto", ] #% data
ngo_mixed<- mixed[mixed$Site == "ngof", ]
ava_mixed<- mixed[mixed$Site == "avai", ]
lan_mixed<- mixed[mixed$Site == "lano", ] #no obs
tuk_mixed<- mixed[mixed$Site == "tuko", ] #no obs
#aro_mixed<- mixed[mixed$Site == "aroi", ] #% data

#delete site column
lou_mixed<-lou_mixed[-c(372)]
plu_mixed<-plu_mixed[-c(372)]
#ano_mixed<-ano_mixed[-c(372)]
wai_mixed<-wai_mixed[-c(372)]
vol_mixed<-vol_mixed[-c(372)]
bon_mixed<-bon_mixed[-c(372)]
tag_mixed<-tag_mixed[-c(372)]
yac_mixed<-yac_mixed[-c(372)]
#fin_mixed<-fin_mixed[-c(372)]
#lot_mixed<-lot_mixed[-c(372)]
ngo_mixed<-ngo_mixed[-c(372)]
ava_mixed<-ava_mixed[-c(372)]
#lan_mixed<-lan_mixed[-c(372)]
#tuk_mixed<-tuk_mixed[-c(372)]
#aro_mixed<-aro_mixed[-c(372)]

# bin the data in 10 bins of 500 years and take the mean value of each bin
lou_mixed <- aggregate(lou_mixed, by=list(cut(lou_mixed$Cal_yrs_BP,seq(-100,4900,500))), mean)
plu_mixed <- aggregate(plu_mixed, by=list(cut(plu_mixed$Cal_yrs_BP,seq(-100,4900,500))), mean)
#ano_mixed <- aggregate(ano_mixed, by=list(cut(ano_mixed$Cal_yrs_BP,seq(-100,4900,500))), mean)
wai_mixed <- aggregate(wai_mixed, by=list(cut(wai_mixed$Cal_yrs_BP,seq(-100,4900,500))), mean)
vol_mixed <- aggregate(vol_mixed, by=list(cut(vol_mixed$Cal_yrs_BP,seq(-100,4900,500))), mean)
bon_mixed <- aggregate(bon_mixed, by=list(cut(bon_mixed$Cal_yrs_BP,seq(-100,4900,500))), mean)
tag_mixed <- aggregate(tag_mixed, by=list(cut(tag_mixed$Cal_yrs_BP,seq(-100,4900,500))), mean)
yac_mixed <- aggregate(yac_mixed, by=list(cut(yac_mixed$Cal_yrs_BP,seq(-100,4900,500))), mean)
#fin_mixed <- aggregate(fin_mixed, by=list(cut(fin_mixed$Cal_yrs_BP,seq(-100,4900,500))), mean)
#lot_mixed <- aggregate(lot_mixed, by=list(cut(lot_mixed$Cal_yrs_BP,seq(-100,4900,500))), mean)
ngo_mixed <- aggregate(ngo_mixed, by=list(cut(ngo_mixed$Cal_yrs_BP,seq(-100,4900,500))), mean)
ava_mixed <- aggregate(ava_mixed, by=list(cut(ava_mixed$Cal_yrs_BP,seq(-100,4900,500))), mean)
#lan_mixed <- aggregate(lan_mixed, by=list(cut(lan_mixed$Cal_yrs_BP,seq(-100,4900,500))), mean)
#tuk_mixed <- aggregate(tuk_mixed, by=list(cut(tuk_mixed$Cal_yrs_BP,seq(-100,4900,500))), mean)
#aro_mixed <- aggregate(aro_mixed, by=list(cut(aro_mixed$Cal_yrs_BP,seq(-100,4900,500))), mean)

# add site names back in
lou_mixed$Site<-"lou"
plu_mixed$Site<-"plu"
#ano_mixed$Site<-"ano"
wai_mixed$Site<-"wai"
vol_mixed$Site<-"vol"
bon_mixed$Site<-"bon"
tag_mixed$Site<-"tag"
yac_mixed$Site<-"yac"
#fin_mixed$Site<-"fin"
#lot_mixed$Site<-"lot"
ngo_mixed$Site<-"ngo"
ava_mixed$Site<-"ava"
#lan_mixed$Site<-"lan"
#tuk_mixed$Site<-"tuk"
#aro_mixed$Site<-"aro"

#merge
mixed<-merge(lou_mixed, plu_mixed, all=TRUE)
#mixed<-merge(mixed, ano_mixed, all=TRUE)
mixed<-merge(mixed, wai_mixed, all=TRUE)
mixed<-merge(mixed, vol_mixed, all=TRUE)
mixed<-merge(mixed, bon_mixed, all=TRUE)
mixed<-merge(mixed, tag_mixed, all=TRUE)
mixed<-merge(mixed, yac_mixed, all=TRUE)
#mixed<-merge(mixed, fin_mixed, all=TRUE)
#mixed<-merge(mixed, lot_mixed, all=TRUE)
mixed<-merge(mixed, ngo_mixed, all=TRUE)
mixed<-merge(mixed, ava_mixed, all=TRUE)
#mixed<-merge(mixed, lan_mixed, all=TRUE)
#mixed<-merge(mixed, tuk_mixed, all=TRUE)
#mixed<-merge(mixed, aro_mixed, all=TRUE)

# reorder the column names
mixed<-mixed[ , order(names(mixed))]

# replace all NA with 0
mixed[is.na(mixed)] <- 0

# sort by age
mixed <- mixed[order(mixed$Group.1),]

#write.csv(mixed, "Outputs/Stand2_binned_subset.csv")

#Stand1####
data<-read.csv("Outputs/Stand1_unbinned_int.csv", check.names = FALSE)
data<-data[-c(1,3:5)]

#remove rows if they add up to <300
keep <- rowSums(data[,3:396]) >= 300
mixed <- data[keep,]

Cal_yrs_BP<- mixed$Cal_yrs_BP
Site<- mixed$site
meta_data <- c("Cal_yrs_BP", "site")
mixed = mixed[,!(names(mixed) %in% meta_data)]

#convert to percentages
mixed<- mixed / rowSums(mixed) * 100

#Add age information
mixed$Cal_yrs_BP <-Cal_yrs_BP

#Add site information
mixed$Site <-Site

#split into new dataframe subsets
lou_mixed<- mixed[mixed$Site == "loui", ]
plu_mixed<- mixed[mixed$Site == "plum", ]
ano_mixed<- mixed[mixed$Site == "anou", ] #no obs
wai_mixed<- mixed[mixed$Site == "wait", ]
vol_mixed<- mixed[mixed$Site == "voli", ]
bon_mixed<- mixed[mixed$Site == "bona", ]
tag_mixed<- mixed[mixed$Site == "tagi", ]
yac_mixed<- mixed[mixed$Site == "yaca", ]
#fin_mixed<- mixed[mixed$Site == "fine", ] #% data
#lot_mixed<- mixed[mixed$Site == "loto", ] #% data
ngo_mixed<- mixed[mixed$Site == "ngof", ]
ava_mixed<- mixed[mixed$Site == "avai", ]
lan_mixed<- mixed[mixed$Site == "lano", ] #no obs
tuk_mixed<- mixed[mixed$Site == "tuko", ] #no obs
#aro_mixed<- mixed[mixed$Site == "aroi", ] #% data

#delete site column
lou_mixed<-lou_mixed[-c(396)]
plu_mixed<-plu_mixed[-c(396)]
#ano_mixed<-ano_mixed[-c(396)]
wai_mixed<-wai_mixed[-c(396)]
vol_mixed<-vol_mixed[-c(396)]
bon_mixed<-bon_mixed[-c(396)]
tag_mixed<-tag_mixed[-c(396)]
yac_mixed<-yac_mixed[-c(396)]
#fin_mixed<-fin_mixed[-c(396)]
#lot_mixed<-lot_mixed[-c(396)]
ngo_mixed<-ngo_mixed[-c(396)]
ava_mixed<-ava_mixed[-c(396)]
#lan_mixed<-lan_mixed[-c(396)]
#tuk_mixed<-tuk_mixed[-c(396)]
#aro_mixed<-aro_mixed[-c(396)]

# bin the data in 10 bins of 500 years and take the mean value of each bin
lou_mixed <- aggregate(lou_mixed, by=list(cut(lou_mixed$Cal_yrs_BP,seq(-100,4900,500))), mean)
plu_mixed <- aggregate(plu_mixed, by=list(cut(plu_mixed$Cal_yrs_BP,seq(-100,4900,500))), mean)
#ano_mixed <- aggregate(ano_mixed, by=list(cut(ano_mixed$Cal_yrs_BP,seq(-100,4900,500))), mean)
wai_mixed <- aggregate(wai_mixed, by=list(cut(wai_mixed$Cal_yrs_BP,seq(-100,4900,500))), mean)
vol_mixed <- aggregate(vol_mixed, by=list(cut(vol_mixed$Cal_yrs_BP,seq(-100,4900,500))), mean)
bon_mixed <- aggregate(bon_mixed, by=list(cut(bon_mixed$Cal_yrs_BP,seq(-100,4900,500))), mean)
tag_mixed <- aggregate(tag_mixed, by=list(cut(tag_mixed$Cal_yrs_BP,seq(-100,4900,500))), mean)
yac_mixed <- aggregate(yac_mixed, by=list(cut(yac_mixed$Cal_yrs_BP,seq(-100,4900,500))), mean)
#fin_mixed <- aggregate(fin_mixed, by=list(cut(fin_mixed$Cal_yrs_BP,seq(-100,4900,500))), mean)
#lot_mixed <- aggregate(lot_mixed, by=list(cut(lot_mixed$Cal_yrs_BP,seq(-100,4900,500))), mean)
ngo_mixed <- aggregate(ngo_mixed, by=list(cut(ngo_mixed$Cal_yrs_BP,seq(-100,4900,500))), mean)
ava_mixed <- aggregate(ava_mixed, by=list(cut(ava_mixed$Cal_yrs_BP,seq(-100,4900,500))), mean)
#lan_mixed <- aggregate(lan_mixed, by=list(cut(lan_mixed$Cal_yrs_BP,seq(-100,4900,500))), mean)
#tuk_mixed <- aggregate(tuk_mixed, by=list(cut(tuk_mixed$Cal_yrs_BP,seq(-100,4900,500))), mean)
#aro_mixed <- aggregate(aro_mixed, by=list(cut(aro_mixed$Cal_yrs_BP,seq(-100,4900,500))), mean)

# add site names back in
lou_mixed$Site<-"lou"
plu_mixed$Site<-"plu"
#ano_mixed$Site<-"ano"
wai_mixed$Site<-"wai"
vol_mixed$Site<-"vol"
bon_mixed$Site<-"bon"
tag_mixed$Site<-"tag"
yac_mixed$Site<-"yac"
#fin_mixed$Site<-"fin"
#lot_mixed$Site<-"lot"
ngo_mixed$Site<-"ngo"
ava_mixed$Site<-"ava"
#lan_mixed$Site<-"lan"
#tuk_mixed$Site<-"tuk"
#aro_mixed$Site<-"aro"

#merge
mixed<-merge(lou_mixed, plu_mixed, all=TRUE)
#mixed<-merge(mixed, ano_mixed, all=TRUE)
mixed<-merge(mixed, wai_mixed, all=TRUE)
mixed<-merge(mixed, vol_mixed, all=TRUE)
mixed<-merge(mixed, bon_mixed, all=TRUE)
mixed<-merge(mixed, tag_mixed, all=TRUE)
mixed<-merge(mixed, yac_mixed, all=TRUE)
#mixed<-merge(mixed, fin_mixed, all=TRUE)
#mixed<-merge(mixed, lot_mixed, all=TRUE)
mixed<-merge(mixed, ngo_mixed, all=TRUE)
mixed<-merge(mixed, ava_mixed, all=TRUE)
#mixed<-merge(mixed, lan_mixed, all=TRUE)
#mixed<-merge(mixed, tuk_mixed, all=TRUE)
#mixed<-merge(mixed, aro_mixed, all=TRUE)

# reorder the column names
mixed<-mixed[ , order(names(mixed))]

# replace all NA with 0
mixed[is.na(mixed)] <- 0

# sort by age
mixed <- mixed[order(mixed$Group.1),]

#write.csv(mixed, "Outputs/Stand1_binned_subset.csv")
