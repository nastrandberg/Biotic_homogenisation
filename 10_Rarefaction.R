#Rarefaction of pollen data where counts are available####

library(dplyr)
library(tidyverse)
library(vegan)

loui<-read.csv("Pollen_data_with_ages/loui_pollen_ages.csv", check.names = FALSE)
loui<-loui[-c(1)]
plum<-read.csv("Pollen_data_with_ages/plum_pollen_ages.csv", check.names = FALSE)
plum<-plum[-c(1)]
anou<-read.csv("Pollen_data_with_ages/anou_pollen_ages.csv", check.names = FALSE)
anou<-anou[-c(1)]
wait<-read.csv("Pollen_data_with_ages/wait_pollen_ages.csv", check.names = FALSE)
wait<-wait[-c(1)]
voli<-read.csv("Pollen_data_with_ages/voli_pollen_ages.csv", check.names = FALSE)
voli<-voli[-c(1)]
bona<-read.csv("Pollen_data_with_ages/bona_pollen_ages.csv", check.names = FALSE)
bona<-bona[-c(1)]
tagi<-read.csv("Pollen_data_with_ages/tagi_pollen_ages.csv", check.names = FALSE)
tagi<-tagi[-c(1)]
yaca<-read.csv("Pollen_data_with_ages/yaca_pollen_ages.csv", check.names = FALSE)
yaca<-yaca[-c(1)]
ngof<-read.csv("Pollen_data_with_ages/ngof_pollen_ages.csv", check.names = FALSE)
ngof<-ngof[-c(1)]
avai<-read.csv("Pollen_data_with_ages/avai_pollen_ages.csv", check.names = FALSE)
avai<-avai[-c(1)]
lano<-read.csv("Pollen_data_with_ages/lano_pollen_ages.csv", check.names = FALSE)
lano<-lano[-c(1)]
tuko<-read.csv("Pollen_data_with_ages/tuko_pollen_ages.csv", check.names = FALSE)
tuko<-tuko[-c(1)]

loui$Site<-"lou"
plum$Site<-"plu"
anou$Site<-"ano"
wait$Site<-"wai"
voli$Site<-"vol"
bona$Site<-"bon"
tagi$Site<-"tag"
yaca$Site<-"yac"
ngof$Site<-"ngo"
avai$Site<-"ava"
lano$Site<-"lan"
tuko$Site<-"tuk"

df_list<-list(anou,avai,bona,lano,loui,ngof,plum,tagi,tuko,voli,wait,yaca)

#merge all data frames together
mixed<-Reduce(function(x, y) merge(x, y, all=TRUE), df_list) 

#write.csv(mixed, "Outputs/original_merged_data.csv")

drop <- c("ALNUS (exotic marker)",
          "Concentration",
          "Debarya Type",
          "Indeterminant",
          "Sum unknown",
          "Total",
          "Zygnema Type",
          "Foraminifera",
          "Dam/deg",
          "Damaged/Crumpled",
          "Indet",
          "Indeterminate Other",
          "Other",
          "Unknown")

mixed = mixed[,!(names(mixed) %in% drop)]
mixed <- mixed %>% replace(is.na(.), 0)

#get mean counts
counts<-rowSums(mixed[c(5:721)])
counts<-as.data.frame(counts)
counts$Site<-mixed$Site
mean_counts<-aggregate(counts, list(counts$Site), mean)
#write.csv(mean_counts, "Outputs/original_mean_counts.csv")

#Rarefy all sites separately

#1. St. Louis Lac####
lou<-dplyr::filter(mixed, Site == "lou")
(raremax_lou <- min(rowSums(lou[c(5:721)])))
rare_lou<-rarefy(lou[c(5:721)],raremax_lou)
rare_lou<-as.data.frame(rare_lou)
rare_lou$Cal_yrs_BP<-lou$Cal_yrs_BP
rare_lou$Site<-lou$Site

#2. Plum Swamp####
plu<-dplyr::filter(mixed, Site == "plu")
(raremax_plu <- min(rowSums(plu[c(5:721)])))
rare_plu<-rarefy(plu[c(5:721)],raremax_plu)
rare_plu<-as.data.frame(rare_plu)
rare_plu$Cal_yrs_BP<-plu$Cal_yrs_BP
rare_plu$Site<-plu$Site

#3. Anouwe Swamp####
ano<-dplyr::filter(mixed, Site == "ano")
(raremax_ano <- min(rowSums(ano[c(5:721)])))
rare_ano<-rarefy(ano[c(5:721)],raremax_ano)
rare_ano<-as.data.frame(rare_ano)
rare_ano$Cal_yrs_BP<-ano$Cal_yrs_BP
rare_ano$Site<-ano$Site

#4. Waitetoke####
wai<-dplyr::filter(mixed, Site == "wai")
(raremax_wai <- min(rowSums(wai[c(5:721)])))
rare_wai<-rarefy(wai[c(5:721)],raremax_wai)
rare_wai<-as.data.frame(rare_wai)
rare_wai$Cal_yrs_BP<-wai$Cal_yrs_BP
rare_wai$Site<-wai$Site

#5. Volivol ####
vol<-dplyr::filter(mixed, Site == "vol")
(raremax_vol <- min(rowSums(vol[c(5:721)])))
rare_vol<-rarefy(vol[c(5:721)],raremax_vol)
rare_vol<-as.data.frame(rare_vol)
rare_vol$Cal_yrs_BP<-vol$Cal_yrs_BP
rare_vol$Site<-vol$Site

#6. Bonatoa Bog####
bon<-dplyr::filter(mixed, Site == "bon")
(raremax_bon <- min(rowSums(bon[c(5:721)])))
rare_bon<-rarefy(bon[c(5:721)],raremax_bon)
rare_bon<-as.data.frame(rare_bon)
rare_bon$Cal_yrs_BP<-bon$Cal_yrs_BP
rare_bon$Site<-bon$Site

#7. Lake Tagimaucia####
tag<-dplyr::filter(mixed, Site == "tag")
(raremax_tag <- min(rowSums(tag[c(5:721)])))
rare_tag<-rarefy(tag[c(5:721)],raremax_tag)
rare_tag<-as.data.frame(rare_tag)
rare_tag$Cal_yrs_BP<-tag$Cal_yrs_BP
rare_tag$Site<-tag$Site

#8. Yacata####
yac<-dplyr::filter(mixed, Site == "yac")
(raremax_yac <- min(rowSums(yac[c(5:721)])))
rare_yac<-rarefy(yac[c(5:721)],raremax_yac)
rare_yac<-as.data.frame(rare_yac)
rare_yac$Cal_yrs_BP<-yac$Cal_yrs_BP
rare_yac$Site<-yac$Site

#9. Finemui Swamp % data####
#10. Lotofoa Swamp % data####

#11. Ngofe Marsh####
ngo<-dplyr::filter(mixed, Site == "ngo")
(raremax_ngo <- min(rowSums(ngo[c(5:721)])))
rare_ngo<-rarefy(ngo[c(5:721)],raremax_ngo)
rare_ngo<-as.data.frame(rare_ngo)
rare_ngo$Cal_yrs_BP<-ngo$Cal_yrs_BP
rare_ngo$Site<-ngo$Site

#12. Avai’o’vuna Swamp####
ava<-dplyr::filter(mixed, Site == "ava")
(raremax_ava <- min(rowSums(ava[c(5:721)])))
rare_ava<-rarefy(ava[c(5:721)],raremax_ava)
rare_ava<-as.data.frame(rare_ava)
rare_ava$Cal_yrs_BP<-ava$Cal_yrs_BP
rare_ava$Site<-ava$Site

#13. Lake Lanoto'o####
lan<-dplyr::filter(mixed, Site == "lan")
(raremax_lan <- min(rowSums(lan[c(5:721)])))
rare_lan<-rarefy(lan[c(5:721)],raremax_lan)
rare_lan<-as.data.frame(rare_lan)
rare_lan$Cal_yrs_BP<-lan$Cal_yrs_BP
rare_lan$Site<-lan$Site

#14. Tukou Marsh####
tuk<-dplyr::filter(mixed, Site == "tuk")
(raremax_tuk <- min(rowSums(tuk[c(5:721)])))
rare_tuk<-rarefy(tuk[c(5:721)],raremax_tuk)
rare_tuk<-as.data.frame(rare_tuk)
rare_tuk$Cal_yrs_BP<-tuk$Cal_yrs_BP
rare_tuk$Site<-tuk$Site

#15. Rano Aroi % data####

#Combine####
names(rare_lou)[1]<-"Rarefied_diversity"
names(rare_plu)[1]<-"Rarefied_diversity"
names(rare_ano)[1]<-"Rarefied_diversity"
names(rare_wai)[1]<-"Rarefied_diversity"
names(rare_vol)[1]<-"Rarefied_diversity"
names(rare_bon)[1]<-"Rarefied_diversity"
names(rare_tag)[1]<-"Rarefied_diversity"
names(rare_yac)[1]<-"Rarefied_diversity"
names(rare_ngo)[1]<-"Rarefied_diversity"
names(rare_ava)[1]<-"Rarefied_diversity"
names(rare_lan)[1]<-"Rarefied_diversity"
names(rare_tuk)[1]<-"Rarefied_diversity"

rare_lou$Site<-"St. Louis Lac"
rare_plu$Site<-"Plum Swamp"
rare_ano$Site<-"Anouwe Swamp"
rare_wai$Site<-"Waitetoke"
rare_vol$Site<-"Volivoli"
rare_bon$Site<-"Bonatoa Bog"
rare_tag$Site<-"Lake Tagimaucia"
rare_yac$Site<-"Yacata"
rare_ngo$Site<-"Ngofe Marsh"
rare_ava$Site<-"Avai'o'vuna Swamp"
rare_lan$Site<-"Lake Lanoto'o"
rare_tuk$Site<-"Tukou Marsh"

all<-rbind(rare_lou,
           rare_plu,
           rare_ano,
           rare_wai,
           rare_vol,
           rare_bon,
           rare_tag,
           rare_yac,
           rare_ngo,
           rare_ava,
           rare_lan,
           rare_tuk)

mean_rarefied<-aggregate(all, list(all$Site), mean)
#write.csv(mean_rarefied, "Outputs/original_mean_rarefied.csv")

#remove data >5000 years old
all <- all[all$Cal_yrs_BP<=5000,]

library(tidypaleo)
ggplot(all, aes(x = all$Rarefied_diversity, y = all$Cal_yrs_BP)) +
  geom_lineh() +
  geom_point() +
  scale_y_reverse() +
  facet_abundanceh(vars(Site)) +
  labs(x = "Rarefied pollen diversity", y = "Cal. years BP")

#facet_abundanceh
#facet_geochem_gridh

#alpha diversity of each site####
loui<-read.csv("Pollen_data_with_ages/loui_pollen_ages.csv", check.names = FALSE)
loui<-loui[-c(1)]
plum<-read.csv("Pollen_data_with_ages/plum_pollen_ages.csv", check.names = FALSE)
plum<-plum[-c(1)]
anou<-read.csv("Pollen_data_with_ages/anou_pollen_ages.csv", check.names = FALSE)
anou<-anou[-c(1)]
wait<-read.csv("Pollen_data_with_ages/wait_pollen_ages.csv", check.names = FALSE)
wait<-wait[-c(1)]
voli<-read.csv("Pollen_data_with_ages/voli_pollen_ages.csv", check.names = FALSE)
voli<-voli[-c(1)]
bona<-read.csv("Pollen_data_with_ages/bona_pollen_ages.csv", check.names = FALSE)
bona<-bona[-c(1)]
tagi<-read.csv("Pollen_data_with_ages/tagi_pollen_ages.csv", check.names = FALSE)
tagi<-tagi[-c(1)]
yaca<-read.csv("Pollen_data_with_ages/yaca_pollen_ages.csv", check.names = FALSE)
yaca<-yaca[-c(1)]
ngof<-read.csv("Pollen_data_with_ages/ngof_pollen_ages.csv", check.names = FALSE)
ngof<-ngof[-c(1)]
avai<-read.csv("Pollen_data_with_ages/avai_pollen_ages.csv", check.names = FALSE)
avai<-avai[-c(1)]
lano<-read.csv("Pollen_data_with_ages/lano_pollen_ages.csv", check.names = FALSE)
lano<-lano[-c(1)]
tuko<-read.csv("Pollen_data_with_ages/tuko_pollen_ages.csv", check.names = FALSE)
tuko<-tuko[-c(1)]

loui$Site<-"St. Louis Lac"
plum$Site<-"Plum Swamp"
anou$Site<-"Anouwe Swamp"
wait$Site<-"Waitetoke"
voli$Site<-"Volivoli"
bona$Site<-"Bonatoa Bog"
tagi$Site<-"Lake Tagimaucia"
yaca$Site<-"Yacata"
ngof$Site<-"Ngofe Marsh"
avai$Site<-"Avai'o'vuna Swamp"
lano$Site<-"Lake Lanoto'o"
tuko$Site<-"Tukou Marsh"

df_list<-list(anou,avai,bona,lano,loui,ngof,plum,tagi,tuko,voli,wait,yaca)

#merge all data frames together
mixed<-Reduce(function(x, y) merge(x, y, all=TRUE), df_list) 

drop <- c("ALNUS (exotic marker)",
          "Concentration",
          "Debarya Type",
          "Indeterminant",
          "Sum unknown",
          "Total",
          "Zygnema Type",
          "Foraminifera",
          "Dam/deg",
          "Damaged/Crumpled",
          "Indet",
          "Indeterminate Other",
          "Other",
          "Unknown")

mixed = mixed[,!(names(mixed) %in% drop)]
mixed <- mixed %>% replace(is.na(.), 0)
mixed = mixed[,!(names(mixed) %in% drop)]
alpha <- with(mixed, specnumber(mixed, Site))
alpha<-as.data.frame(alpha)
#write.csv(alpha, "Outputs/original_a_diversity.csv")
