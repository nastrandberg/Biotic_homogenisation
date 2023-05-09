#Rarefaction of pollen data where counts are available####

library(dplyr)
library(tidyverse)
library(vegan)

#"mixed_ungrouped_rounded.csv is the same as
#mixed_ungrouped.csv except I rounded the values up
#to the nearest 1 in excel first.

mixed<-read.csv("mixed_ungrouped_rounded.csv")
mixed<-mixed[-c(1)]
#remove rows which contain all 0s
mixed = mixed[rowSums(mixed[2,4:366])>0,]

#remove the sites which do not have count data
mixed<-mixed[mixed$Site!="Finemui Swamp",]
mixed<-mixed[mixed$Site!="Lotofoa Swamp",]
mixed<-mixed[mixed$Site!="Rano Aroi",]
mixed<-mixed[mixed$Site!="Yacata",]
mixed<-mixed[mixed$Site!="Volivoli",]

#get mean counts
counts<-rowSums(mixed[c(2,4:366)])
counts<-as.data.frame(counts)
counts$site<-mixed$Site
mean_counts<-aggregate(counts, list(counts$site), mean)
#write.csv(mean_counts, "mean_counts.csv")

#Rarefy all sites separately

#1. St. Louis Lac####
lou<-dplyr::filter(mixed, Site == "St. Louis Lac")
(raremax_lou <- min(rowSums(lou[c(2,4:366)])))
rare_lou<-rarefy(lou[c(2,4:366)],raremax_lou)
rare_lou<-as.data.frame(rare_lou)
rare_lou$Cal_yrs_BP<-lou$Cal_yrs_BP
rare_lou$Site<-lou$Site

#2. Plum Swamp####
plu<-dplyr::filter(mixed, Site == "Plum Swamp")
(raremax_plu <- min(rowSums(plu[c(2,4:366)])))
rare_plu<-rarefy(plu[c(2,4:366)],raremax_plu)
rare_plu<-as.data.frame(rare_plu)
rare_plu$Cal_yrs_BP<-plu$Cal_yrs_BP
rare_plu$Site<-plu$Site

#3. Anouwe Swamp####
ano<-dplyr::filter(mixed, Site == "Anouwe Swamp")
(raremax_ano <- min(rowSums(ano[c(2,4:366)])))
rare_ano<-rarefy(ano[c(2,4:366)],raremax_ano)
rare_ano<-as.data.frame(rare_ano)
rare_ano$Cal_yrs_BP<-ano$Cal_yrs_BP
rare_ano$Site<-ano$Site

#4. Waitetoke####
wai<-dplyr::filter(mixed, Site == "Waitetoke")
(raremax_wai <- min(rowSums(wai[c(2,4:366)])))
rare_wai<-rarefy(wai[c(2,4:366)],raremax_wai)
rare_wai<-as.data.frame(rare_wai)
rare_wai$Cal_yrs_BP<-wai$Cal_yrs_BP
rare_wai$Site<-wai$Site

#5. Volivol % data####

#6. Bonatoa Bog####
bon<-dplyr::filter(mixed, Site == "Bonatoa Bog")
(raremax_bon <- min(rowSums(bon[c(2,4:366)])))
rare_bon<-rarefy(bon[c(2,4:366)],raremax_bon)
rare_bon<-as.data.frame(rare_bon)
rare_bon$Cal_yrs_BP<-bon$Cal_yrs_BP
rare_bon$Site<-bon$Site

#7. Lake Tagimaucia####
tag<-dplyr::filter(mixed, Site == "Lake Tagimaucia")
(raremax_tag <- min(rowSums(tag[c(2,4:366)])))
rare_tag<-rarefy(tag[c(2,4:366)],raremax_tag)
rare_tag<-as.data.frame(rare_tag)
rare_tag$Cal_yrs_BP<-tag$Cal_yrs_BP
rare_tag$Site<-tag$Site

#8. Yacata % data####
#9. Finemui Swamp % data####
#10. Lotofoa Swamp % data####

#11. Ngofe Marsh####
ngo<-dplyr::filter(mixed, Site == "Ngofe Marsh")
(raremax_ngo <- min(rowSums(ngo[c(2,4:366)])))
rare_ngo<-rarefy(ngo[c(2,4:366)],raremax_ngo)
rare_ngo<-as.data.frame(rare_ngo)
rare_ngo$Cal_yrs_BP<-ngo$Cal_yrs_BP
rare_ngo$Site<-ngo$Site

#12. Avai’o’vuna Swamp####
ava<-dplyr::filter(mixed, Site == "Avai'o'vuna Swamp")
(raremax_ava <- min(rowSums(ava[c(2,4:366)])))
rare_ava<-rarefy(ava[c(2,4:366)],raremax_ava)
rare_ava<-as.data.frame(rare_ava)
rare_ava$Cal_yrs_BP<-ava$Cal_yrs_BP
rare_ava$Site<-ava$Site

#13. Lake Lanoto'o####
lan<-dplyr::filter(mixed, Site == "Lake Lanoto'o")
(raremax_lan <- min(rowSums(lan[c(2,4:366)])))
rare_lan<-rarefy(lan[c(2,4:366)],raremax_lan)
rare_lan<-as.data.frame(rare_lan)
rare_lan$Cal_yrs_BP<-lan$Cal_yrs_BP
rare_lan$Site<-lan$Site

#14. Tukou Marsh####
tuk<-dplyr::filter(mixed, Site == "Tukou Marsh")
(raremax_tuk <- min(rowSums(tuk[c(2,4:366)])))
rare_tuk<-rarefy(tuk[c(2,4:366)],raremax_tuk)
rare_tuk<-as.data.frame(rare_tuk)
rare_tuk$Cal_yrs_BP<-tuk$Cal_yrs_BP
rare_tuk$Site<-tuk$Site

#15. Rano Aroi % data####
#16. Rano Raraku % data####

#Combine####
names(rare_lou)[1]<-"Rarefied_diversity"
names(rare_plu)[1]<-"Rarefied_diversity"
names(rare_ano)[1]<-"Rarefied_diversity"
names(rare_wai)[1]<-"Rarefied_diversity"
names(rare_bon)[1]<-"Rarefied_diversity"
names(rare_tag)[1]<-"Rarefied_diversity"
names(rare_ngo)[1]<-"Rarefied_diversity"
names(rare_ava)[1]<-"Rarefied_diversity"
names(rare_lan)[1]<-"Rarefied_diversity"
names(rare_tuk)[1]<-"Rarefied_diversity"

all<-rbind(rare_lou,
           rare_plu,
           rare_ano,
           rare_wai,
           rare_bon,
           rare_tag,
           rare_ngo,
           rare_ava,
           rare_lan,
           rare_tuk)

mean_rarefied<-aggregate(all, list(all$Site), mean)
#write.csv(mean_rarefied, "mean_rarefied.csv")

library(tidypaleo)
ggplot(all, aes(x = all$Rarefied_diversity, y = all$Cal_yrs_BP)) +
  geom_lineh() +
  geom_point() +
  scale_y_reverse() +
  facet_geochem_gridh(vars(Site)) +
  labs(x = "Rarefied pollen diversity", y = "Cal. years BP")

#alpha diversity of each site
mixed<-read.csv("mixed_ungrouped.csv")
drop <- c("X", "Cal_yrs_BP")
mixed = mixed[,!(names(mixed) %in% drop)]
alpha <- with(mixed, specnumber(mixed, Site))
alpha<-as.data.frame(alpha)
#write.csv(alpha, "alpha_diversity.csv")
