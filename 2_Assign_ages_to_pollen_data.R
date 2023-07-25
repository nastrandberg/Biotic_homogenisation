#Download Neotoma pollen records and assign age 
#information to all datasets####

library(rlang)
library(dplyr)
library("remotes")
#remotes::install_github("NeotomaDB/neotoma2")
library(neotoma2)
library(tidyverse)

#1. St. Louis Lac (New Caledonia)####
if(1){
  counts<-read.csv("Original_pollen_data/loui_pollen.csv", check.names = F)
  ages<-read.delim("Bacon_runs/St_Louis_Lac/St_Louis_Lac_77_ages.txt")
  counts<-merge(counts, ages, by="depth")
  drop <- c("depth", "median")
  counts = counts[,!(names(counts) %in% drop)]
  colnames(counts)[colnames(counts) == "mean"] ="Cal_yrs_BP"
  counts <- counts[, c(157, 155, 156, 1:154)]
  #write.csv(counts, "Pollen_data_with_ages/loui_pollen_ages.csv")
}

#2. Plum Swamp (New Caledonia)####
if(1){
counts<-read.csv("Original_pollen_data/plum_pollen.csv",check.names = F)
ages<-read.delim("Bacon_runs/Plum_Swamp/Plum_Swamp_124_ages.txt")
counts<-merge(counts, ages, by="depth")
drop <- c("depth", "median")
counts = counts[,!(names(counts) %in% drop)]
colnames(counts)[colnames(counts) == "mean"] ="Cal_yrs_BP"
counts <- counts[, c(149, 147, 148, 1:146)]
#write.csv(counts, "Pollen_data_with_ages/plum_pollen_ages.csv")
}

#3. Anouwe Swamp (Vanuatu)####
if(1){
  site <- get_sites(sitename = "%Anouwe%")
  site.poll <- site %>%
    neotoma2::filter(datasettype == "pollen")
  data <- get_downloads(site.poll)
  pSamples <- samples(data) %>%
    dplyr::filter(units == "NISP")
  counts <- tidyr::pivot_wider(pSamples,
                               id_cols = age,
                               names_from = variablename,
                               values_from = value,
                               values_fill = 0)
  ages<-pSamples[,c("age","ageolder","ageyounger")]
  ages <- ages[!duplicated(ages), ]
  counts<-merge(counts, ages, by="age")
  colnames(counts)[colnames(counts) == "age"] ="Cal_yrs_BP"
  colnames(counts)[colnames(counts) == "ageolder"] ="max"
  colnames(counts)[colnames(counts) == "ageyounger"] ="min"
  counts <- counts[, c(1, 55, 54, 2:53)]
  #write.csv(counts, "Pollen_data_with_ages/anou_pollen_ages.csv")
}

#4. Waitetoke (New Zealand)####
if(1){
  site <- get_sites(sitename = "%Waitetoke%")
  site.poll <- site %>%
    neotoma2::filter(datasettype == "pollen")
  data <- get_downloads(site.poll)
  pSamples <- samples(data) %>%
    dplyr::filter(units == "NISP")
  counts <- tidyr::pivot_wider(pSamples,
                               id_cols = age,
                               names_from = variablename,
                               values_from = value,
                               values_fill = 0)
  ages<-pSamples[,c("age","ageolder","ageyounger")]
  ages <- ages[!duplicated(ages), ]
  counts<-merge(counts, ages, by="age")
  colnames(counts)[colnames(counts) == "age"] ="Cal_yrs_BP"
  colnames(counts)[colnames(counts) == "ageolder"] ="max"
  colnames(counts)[colnames(counts) == "ageyounger"] ="min"
  counts <- counts[, c(1, 104, 103, 2:102)]
  #write.csv(counts, "Pollen_data_with_ages/wait_pollen_ages.csv")
}

#5. Volivoli (Fiji)####
if(1){
counts<-read.csv("Original_pollen_data/voli_pollen.csv",check.names = F)
ages<-read.delim("Bacon_runs/Volivoli/Volivoli_42_ages.txt")
counts<-merge(counts, ages, by="depth")
drop <- c("depth", "median")
counts = counts[,!(names(counts) %in% drop)]
colnames(counts)[colnames(counts) == "mean"] ="Cal_yrs_BP"
counts <- counts[, c(72, 70, 71, 1:69)]
#write.csv(counts, "Pollen_data_with_ages/voli_pollen_ages.csv")
}

#6. Bonatoa Bog (Fiji)####
if(1){
  site <- get_sites(sitename = "%Bonatoa%")
  site.poll <- site %>%
    neotoma2::filter(datasettype == "pollen")
  data <- get_downloads(site.poll)
  pSamples <- samples(data) %>%
    dplyr::filter(units == "NISP")
  counts <- tidyr::pivot_wider(pSamples,
                               id_cols = age,
                               names_from = variablename,
                               values_from = value,
                               values_fill = 0)
  ages<-pSamples[,c("age","ageolder","ageyounger")]
  ages <- ages[!duplicated(ages), ]
  counts<-merge(counts, ages, by="age")
  colnames(counts)[colnames(counts) == "age"] ="Cal_yrs_BP"
  colnames(counts)[colnames(counts) == "ageolder"] ="max"
  colnames(counts)[colnames(counts) == "ageyounger"] ="min"
  counts <- counts[, c(1, 97, 96, 2:95)]
  #write.csv(counts, "Pollen_data_with_ages/bona_pollen_ages.csv")
}

#7. Lake Tagimaucia (Fiji)####
if(1){
  site <- get_sites(sitename = "%Tagimaucia%")
  site.poll <- site %>%
    neotoma2::filter(datasettype == "pollen")
  data <- get_downloads(site.poll)
  pSamples <- samples(data) %>%
    dplyr::filter(units == "NISP")
  counts <- tidyr::pivot_wider(pSamples,
                               id_cols = age,
                               names_from = variablename,
                               values_from = value,
                               values_fill = 0)
  ages<-pSamples[,c("age","ageolder","ageyounger")]
  ages <- ages[!duplicated(ages), ]
  counts<-merge(counts, ages, by="age")
  colnames(counts)[colnames(counts) == "age"] ="Cal_yrs_BP"
  colnames(counts)[colnames(counts) == "ageolder"] ="max"
  colnames(counts)[colnames(counts) == "ageyounger"] ="min"
  counts <- counts[, c(1, 131, 130, 2:129)]
  #write.csv(counts, "Pollen_data_with_ages/tagi_pollen_ages.csv")
}

#8. Yacata (Fiji)####
if(1){
counts<-read.csv("Original_pollen_data/yaca_pollen.csv",check.names = F)
ages<-read.delim("Bacon_runs/Yacata/Yacata_149_ages.txt")
counts<-merge(counts, ages, by="depth")
drop <- c("depth", "median")
counts = counts[,!(names(counts) %in% drop)]
colnames(counts)[colnames(counts) == "mean"] ="Cal_yrs_BP"
counts <- counts[, c(110, 108, 109, 1:107)]
#write.csv(counts, "Pollen_data_with_ages/yaca_pollen_ages.csv")
}

#9. Finemui Swamp (Tonga)####
if(1){
counts<-read.csv("Original_pollen_data/fine_pollen.csv",check.names = F)
ages<-read.delim("Bacon_runs/Finemui_dates/Finemui_dates_46_ages.txt")
counts<-merge(counts, ages, by="depth")
drop <- c("depth", "median")
counts = counts[,!(names(counts) %in% drop)]
colnames(counts)[colnames(counts) == "mean"] ="Cal_yrs_BP"
counts <- counts[, c(52, 50, 51, 1:49)]
#write.csv(counts, "Pollen_data_with_ages/fine_pollen_ages.csv")
}

#10. Lotofoa Swamp (Tonga)####
if(1){
counts<-read.csv("Original_pollen_data/loto_pollen.csv",check.names = F)
ages<-read.delim("Bacon_runs/Lotofoa_dates/Lotofoa_dates_49_ages.txt")
counts<-merge(counts, ages, by="depth")
drop <- c("depth", "median")
counts = counts[,!(names(counts) %in% drop)]
colnames(counts)[colnames(counts) == "mean"] ="Cal_yrs_BP"
counts <- counts[, c(44, 42, 43, 1:41)]
#write.csv(counts, "Pollen_data_with_ages/loto_pollen_ages.csv")
}

#11. Ngofe Marsh (Tonga)####
if(1){
counts<-read.csv("Original_pollen_data/ngof_pollen.csv",check.names = F)
ages<-read.delim("Bacon_runs/Ngofe_dates/Ngofe_dates_134_ages.txt")
counts<-merge(counts, ages, by="depth")
drop <- c("depth", "median")
counts = counts[,!(names(counts) %in% drop)]
colnames(counts)[colnames(counts) == "mean"] ="Cal_yrs_BP"
counts <- counts[, c(77, 75, 76, 1:74)]
#write.csv(counts, "Pollen_data_with_ages/ngof_pollen_ages.csv")
}

#12. Avai'o'vuna Swamp (Tonga)####
if(1){
counts<-read.csv("Original_pollen_data/avai_pollen.csv",check.names = F)
ages<-read.delim("Bacon_runs/Avaiovuna_dates/Avaiovuna_dates_31_ages.txt")
counts<-merge(counts, ages, by="depth")
drop <- c("depth", "median")
counts = counts[,!(names(counts) %in% drop)]
colnames(counts)[colnames(counts) == "mean"] ="Cal_yrs_BP"
counts <- counts[, c(83, 81, 82, 1:80)]
#write.csv(counts, "Pollen_data_with_ages/avai_pollen_ages.csv")
}

#13. Lake Lanoto'o (Samoa)####
if(1){
  counts<-read.csv("Original_pollen_data/lano_pollen.csv", check.names = F)
  ages<-read.csv("Bacon_runs/Lake_Lanotoo/Lake_Lanotoo.csv")
  counts<-merge(counts, ages, by="depth")
  drop <- c("depth", "median")
  counts = counts[,!(names(counts) %in% drop)]
  colnames(counts)[colnames(counts) == "mean"] ="Cal_yrs_BP"
  counts <- counts[, c(43, 41, 42, 1:40)]
  #write.csv(counts, "Pollen_data_with_ages/lano_pollen_ages.csv")
}

#14. Tukou Marsh (French Polynesia)####
if(1){
  site <- get_sites(sitename = "%Tukou%")
  site.poll <- site %>%
    neotoma2::filter(datasettype == "pollen")
  data <- get_downloads(site.poll)
  pSamples <- samples(data) %>%
    dplyr::filter(units == "NISP")
  counts <- tidyr::pivot_wider(pSamples,
                               id_cols = age,
                               names_from = variablename,
                               values_from = value,
                               values_fill = 0)
  ages<-pSamples[,c("age","ageolder","ageyounger")]
  ages <- ages[!duplicated(ages), ]
  counts<-merge(counts, ages, by="age")
  colnames(counts)[colnames(counts) == "age"] ="Cal_yrs_BP"
  colnames(counts)[colnames(counts) == "ageolder"] ="max"
  colnames(counts)[colnames(counts) == "ageyounger"] ="min"
  counts <- counts[, c(1, 53, 52, 2:51)]
  #write.csv(counts, "Pollen_data_with_ages/tuko_pollen_ages.csv")
  }

#15. Rano Aroi (Chile)####
if(1){
  counts<-read.csv("Original_pollen_data/aroi_pollen.csv", check.names = F)
  ages<-read.delim("Bacon_runs/Rano_Aroi/Rano_Aroi_115_ages.txt")
  counts<-merge(counts, ages, by="depth")
  drop <- c("depth", "median")
  counts = counts[,!(names(counts) %in% drop)]
  colnames(counts)[colnames(counts) == "mean"] ="Cal_yrs_BP"
  counts <- counts[, c(29, 27, 28, 1:26)]
  #write.csv(counts, "Pollen_data_with_ages/aroi_pollen_ages.csv")
}