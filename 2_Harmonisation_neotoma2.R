#Harmonisation of pollen records####

library(rlang)
library(dplyr)
library("remotes")
#remotes::install_github("NeotomaDB/neotoma2")
library(neotoma2)

#1. St. Louis Lac (New Caledonia)####
if(1){
  site <- get_sites(sitename = "%St. Louis Lac%")
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
  
  names(counts)[1]<-"Cal_yrs_BP"
  Cal_yrs_BP<- counts$Cal_yrs_BP
  meta_data <- c("Cal_yrs_BP")
  counts = counts[,!(names(counts) %in% meta_data)]
  
  #delete uncertain taxa
  drop <- c("Indeterminable", "Unknown", "Cunoniaceae/Elaeocarpaceae", "Polypodiales")
  counts = counts[,!(names(counts) %in% drop)]
  
  #rename some of the columns
  names(counts)[names(counts) == 'Sterculioideae-type'] <- 'Malvaceae'
  names(counts)[names(counts) == 'Wikstroemia-type'] <- 'Thymelaeaceae'#a few genera with this name
  names(counts)[names(counts) == 'Exocarpos-type'] <- 'Santalaceae'
  names(counts)[names(counts) == 'Symplocos-type'] <- 'Symplocaceae'
  
  #Add age information
  counts$Cal_yrs_BP <-Cal_yrs_BP
  
  #aggregate all taxa names
  lou_mixed<-t(rowsum(t(counts), group = colnames(counts), na.rm = T))
  lou_mixed<-as.data.frame(lou_mixed)
}

#2. Plum Swamp (New Caledonia)####
if(1){
  counts<-read.csv("plum_pollen.csv", check.names = FALSE)
  names(counts)[1]<-"Cal_yrs_BP"
  Cal_yrs_BP<- counts$Cal_yrs_BP
  meta_data <- c("Cal_yrs_BP")
  counts = counts[,!(names(counts) %in% meta_data)]
  
  #delete cf. taxa
  counts = counts[,!grepl("^cf.",names(counts))]
  
  #delete uncertain taxa
  drop <- c("Beauprea ?", "CUNONIACEAE/ELAEOCARPACEAE", "LORANTHACEAE ?", "PIPERACEAE Type", "Pittosporum ?", "RUTACEAE ?", 
            "SAPINDACEAE Sync. Type", "Smilax?", "Trema ?", "VIOLACEAE?", 
            "Typha?", "Monolete Psilate Small", 
            "Monolete Psilate Medium", "Monolete Psilate Large", 
            "Round Hairy Spore", "340 Type", "Type A Mono", 
            "Type B Mono", "Type C Mono", "Other Monolete Ferns", "Trilete Psilate ", 
            "NC-SS-32 Type", "Type A Tri", "Other Trilete Ferns",
            "Type 4", "Type 10", "Type 19", "Type 29",
            "Type 49", "Type 63", "Type 65", "Type 66",
            "Type 77", "Type 84", "Type 89", "Type 150",
            "Type 151", "Type 152", "Type 153", "Type 157",
            "Type 158", "Type 159", "Type 160", "Type 161",
            "Type 164", "Type 165", "Type 166", "Type 167",
            "Type 168", "Type 171", "Total", "Concentration",
            "ALNUS (exotic marker)","Dam/deg", "Indet", 
            "Debarya Type", "Zygnema Type", "Desmodium Type ?", "Trilete Psilate")
  counts = counts[,!(names(counts) %in% drop)]
  
  #rename some of the columns
  names(counts)[names(counts) == 'OTHER PODOCARPACEAE'] <- 'Podocarpaceae'
  names(counts)[names(counts) == 'Alyxia Type'] <- 'Apocynaceae'
  names(counts)[names(counts) == 'Antidesma Type'] <- 'Phyllanthaceae'
  names(counts)[names(counts) == 'Argophyllum Type'] <- 'Argophyllaceae'
  names(counts)[names(counts) == 'Austrobuxus buxoides Type'] <- 'Longetia' #synonym
  names(counts)[names(counts) == 'Austrobuxus Other'] <- 'Longetia'
  names(counts)[names(counts) == 'Citronella Type'] <- 'Cardiopteridaceae'
  names(counts)[names(counts) == 'Cupaniopsis Type'] <- 'Sapindaceae'
  names(counts)[names(counts) == 'Cyathaceae'] <- 'Cyatheaceae' #spelling mistake
  names(counts)[names(counts) == 'Dodonaea Type'] <- 'Sapindaceae'
  names(counts)[names(counts) == 'EPACRIDACEAE'] <- 'Ericaceae'#not on POWO
  names(counts)[names(counts) == 'Euroshinus Type'] <- 'Anacardiaceae'#spelling mistake Euroschinus
  names(counts)[names(counts) == 'Geniostoma Type'] <- 'Loganiaceae'
  names(counts)[names(counts) == 'Glochidion Type'] <- 'Phyllanthaceae'
  names(counts)[names(counts) == 'Grevillea Type'] <- 'Proteaceae'
  names(counts)[names(counts) == 'GUTTIFERAE'] <- 'Clusiaceae'#synonym not on POWO
  names(counts)[names(counts) == 'Hedycarya Type'] <- 'Monimiaceae'
  names(counts)[names(counts) == 'Hibbertia Type'] <- 'Dilleniaceae'
  names(counts)[names(counts) == 'Homalanthus Type'] <- 'Euphorbiaceae'
  names(counts)[names(counts) == 'Kermadecia/Stenocarpus'] <- 'Proteaceae'
  names(counts)[names(counts) == 'Labiateae Type'] <- 'Lamiaceae'#synonym not on POWO
  names(counts)[names(counts) == 'Carpolepis Type'] <- 'Metrosideros'#synonym
  names(counts)[names(counts) == 'OTHER MYRTACEAE'] <- 'Myrtaceae'
  names(counts)[names(counts) == 'Planchonella Type'] <- 'Sapotaceae'
  names(counts)[names(counts) == 'Soulamia Type'] <- 'Simaroubaceae' #spelling mistake Soulamea
  names(counts)[names(counts) == 'Spathodea Type'] <- 'Bignoniaceae'
  names(counts)[names(counts) == 'Sphenomeris Type'] <- 'Lindsaeaceae'
  names(counts)[names(counts) == 'Tapeinosperma Type'] <- 'Primulaceae'
  names(counts)[names(counts) == 'Colocasia Type'] <- 'Araceae'
  names(counts)[names(counts) == 'Palmae'] <- 'Arecaceae'
  names(counts)[names(counts) == 'Pandanus krauelianis Type'] <- 'Pandanus'
  names(counts)[names(counts) == 'Other PANDANACEAE'] <- 'Pandanaceae'
  names(counts)[names(counts) == 'Psidium guava'] <- 'Psidium guajava' #Spelling mistake
  names(counts)[names(counts) == 'Psychotira'] <- 'Psychotria' #Spelling mistake
  names(counts)[names(counts) == 'FLACOURTICEAE'] <- 'Salicaceae' #spelling mistake and synonym
  names(counts)[names(counts) == 'Neocallitropsis'] <- 'Callitris' #spelling mistake and synonym
  names(counts)[names(counts) == 'STERCULICEAE'] <- 'Malvaceae' #spelling mistake and synonym
  
  #Change upper case to sentence case
  names(counts)[names(counts) == 'ACANTHACEAE'] <- 'Acanthaceae'
  names(counts)[names(counts) == 'AMARANTHACEAE'] <- 'Amaranthaceae'
  names(counts)[names(counts) == 'APOCYNACEAE'] <- 'Apocynaceae'
  names(counts)[names(counts) == 'ARALIACEAE'] <- 'Araliaceae'
  names(counts)[names(counts) == 'ASTERACEAE'] <- 'Asteraceae'
  names(counts)[names(counts) == 'CASUARINACEAE'] <- 'Casuarinaceae'
  names(counts)[names(counts) == 'DILLENIACEAE'] <- 'Dilleniaceae'
  names(counts)[names(counts) == 'ESCALLIONACEAE'] <- 'Escalloniaceae' #spelling mistake
  names(counts)[names(counts) == 'GESNERIACEAE'] <- 'Gesneriaceae'
  names(counts)[names(counts) == 'GOODENIACEAE'] <- 'Goodeniaceae'
  names(counts)[names(counts) == 'MALPIGHIACEAE'] <- 'Malpighiaceae'
  names(counts)[names(counts) == 'PROTEACEAE'] <- 'Proteaceae'
  names(counts)[names(counts) == 'RUBIACEAE'] <- 'Rubiaceae'
  names(counts)[names(counts) == 'SAPINDACEAE'] <- 'Sapindaceae'
  names(counts)[names(counts) == 'SAPOTACEAE'] <- 'Sapotaceae'
  names(counts)[names(counts) == 'SOLANACEAE'] <- 'Solanaceae'
  names(counts)[names(counts) == 'URTICACEAE'] <- 'Urticaceae'
  names(counts)[names(counts) == 'VERBENACEAE'] <- 'Verbenaceae'
  names(counts)[names(counts) == 'CYPERACEAE'] <- 'Cyperaceae'
  names(counts)[names(counts) == 'POACEAE'] <- 'Poaceae'
  names(counts)[names(counts) == 'POLYPODIACEAE'] <- 'Polypodiaceae'
  
  
  #Add age information
  counts$Cal_yrs_BP <-Cal_yrs_BP
  
  #aggregate all taxa names
  plu_mixed<-t(rowsum(t(counts), group = colnames(counts), na.rm = T))
  plu_mixed<-as.data.frame(plu_mixed)
  
}

#3. Anouwe Swamp (Vanuatu)####
if(1){
  counts<-read.csv("anou_pollen.csv")
  names(counts)[1]<-"Cal_yrs_BP"
  Cal_yrs_BP<- counts$Cal_yrs_BP
  meta_data <- c("Cal_yrs_BP")
  counts = counts[,!(names(counts) %in% meta_data)]
  
  #delete uncertain taxa
  drop <- c("Triletes","Unknown", "Urticaceae.Moraceae", "Polypodiales")
  counts = counts[,!(names(counts) %in% drop)]
  
  #rename some of the columns
  names(counts)[names(counts) == 'Lycopodiella.cernua.var..cernua.type'] <- 'Lycopodiella'
  names(counts)[names(counts) == 'Pseudodiphasium.volubile'] <- 'Lycopodium'
  names(counts)[names(counts) == 'Rapanea'] <- 'Myrsine'
  
  #Add age information
  ano_counts<-cbind(counts, Cal_yrs_BP)
  
  #aggregate all taxa names
  ano_mixed<-t(rowsum(t(ano_counts), group = colnames(ano_counts), na.rm = T))
  ano_mixed<-as.data.frame(ano_mixed)
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
  
  names(counts)[1]<-"Cal_yrs_BP"
  Cal_yrs_BP<- counts$Cal_yrs_BP
  meta_data <- c("Cal_yrs_BP")
  counts = counts[,!(names(counts) %in% meta_data)]

  #delete uncertain taxa
  drop <- c("Unknown (monolete, psilate)", "Bryophyta/Marchantiophyta", "Spermatophyta undiff.", "cf. Hydrocotyle", "cf. Quintinia")
  counts = counts[,!(names(counts) %in% drop)]
  
  #rename some of the columns
  names(counts)[names(counts) == 'Geranium retrorsum group'] <- 'Geranium'
  names(counts)[names(counts) == 'Asteroideae undiff.'] <- 'Asteroideae'
  names(counts)[names(counts) == 'Sonchus cf. S. kirkii'] <- 'Sonchus'
  names(counts)[names(counts) == 'Podocarpaceae undiff.'] <- 'Podocarpaceae'
  names(counts)[names(counts) == 'Fabaceae undiff.'] <- 'Fabaceae'
  names(counts)[names(counts) == 'Neomyrtus-type'] <- 'Myrtaceae'
  names(counts)[names(counts) == 'Blechnaceae'] <- 'Aspleniaceae' #synonym
  names(counts)[names(counts) == 'Fuscospora'] <- 'Nothofagus' #synonym
  names(counts)[names(counts) == 'Pellaea'] <- 'Hemionitis' #synonym
  names(counts)[names(counts) == 'Phlegmariurus varius'] <- 'Huperzia varia' #synonym
  names(counts)[names(counts) == 'Prumnopitys ferruginea'] <- 'Pectinopitys ferruginea'#synonym
  names(counts)[names(counts) == 'Pseudopanax arboreus'] <- 'Neopanax arboreus'#synonym
  names(counts)[names(counts) == 'Thelypteridaceae'] <- 'Aspleniaceae' #synonym
  
  #IMPORTANT COLUMN INDEX MAY CHANGE####
  #Because of the special characters it was not possible to rename them in the same way as above
  names(counts)[24]<-"Poaceae"
  names(counts)[25]<-"Poaceae"
  #Add age information
  counts$Cal_yrs_BP <-Cal_yrs_BP
  
  #aggregate all taxa names
  wai_mixed<-t(rowsum(t(counts), group = colnames(counts), na.rm = T))
  wai_mixed<-as.data.frame(wai_mixed)
}

#5. Volivoli (Fiji)####
#values are percentages which add up to 1

if(1){
  counts<-read.csv("voli_pollen.csv")
  names(counts)[1]<-"Cal_yrs_BP"
  Cal_yrs_BP<- counts$Cal_yrs_BP
  meta_data <- c("Cal_yrs_BP")
  counts = counts[,!(names(counts) %in% meta_data)]
  
  #delete uncertain taxa
  drop <- c("Treeferns", "Triletes.Other", "Monolete.Other", "Monolete.Psilate", "Indeterminate.3C3P", "Indeterminate.Other", "Type.11")
  counts = counts[,!(names(counts) %in% drop)]
  
  #rename some of the columns
  names(counts)[names(counts) == 'Canavalia.Type'] <- 'Fabaceae'
  names(counts)[names(counts) == 'Malvaceae..other'] <- 'Malvaceae'
  names(counts)[names(counts) == 'Terminalia.Type'] <- 'Terminalia'
  names(counts)[names(counts) == 'Arytera.Type'] <- 'Sapindaceae'
  names(counts)[names(counts) == 'Homalanthus.Exocoaria'] <- 'Euphorbiaceae'
  names(counts)[names(counts) == 'Legumes'] <- 'Fabaceae'
  names(counts)[names(counts) == 'Lonicera.Type'] <- 'Caprifoliaceae'
  names(counts)[names(counts) == 'Macaranga.Mallotus'] <- 'Euphorbiaceae'
  names(counts)[names(counts) == 'Neonauclea.Type'] <- 'Rubiaceae'
  names(counts)[names(counts) == 'Olacaceae.6p.triangle'] <- 'Olacaceae'
  names(counts)[names(counts) == 'Pandandus.tectorius.Type'] <- 'Pandandus'
  names(counts)[names(counts) == 'Pandanus.krauliensis.Type'] <- 'Pandanus'
  names(counts)[names(counts) == 'Type.13.Simaroubaceae'] <- 'Simaroubaceae'
  names(counts)[names(counts) == 'Asteraceae.Type.A'] <- 'Asteraceae'
  names(counts)[names(counts) == 'Asteraceae.Type.B'] <- 'Asteraceae'
  names(counts)[names(counts) == 'Poaceae.70.um'] <- 'Poaceae'
  names(counts)[names(counts) == 'Nymphoides.indica'] <- 'Nymphoides indica'
  names(counts)[names(counts) == 'Acryanthes'] <- 'Achyranthes' #spelling mistake
  names(counts)[names(counts) == 'Apiacaeae'] <- 'Apiaceae' #spelling mistake
  names(counts)[names(counts) == 'Palms'] <- 'Arecaceae'
  names(counts)[names(counts) == 'Pandandus'] <- 'Pandanus'#spelling mistake
  names(counts)[names(counts) == 'Portulaceae'] <- 'Portulacaceae' #spelling mistake
  names(counts)[names(counts) == 'Barringtoniaceae'] <- 'Lecythidaceae'#synonym
  names(counts)[names(counts) == 'Dodonea'] <- 'Dodonaea' #spelling mistake
  
  #Add age information
  vol_counts<-cbind(counts, Cal_yrs_BP)
  
  #aggregate all taxa names
  vol_mixed<-t(rowsum(t(vol_counts), group = colnames(vol_counts), na.rm = T))
  vol_mixed<-as.data.frame(vol_mixed)
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
  
  names(counts)[1]<-"Cal_yrs_BP"
  Cal_yrs_BP<- counts$Cal_yrs_BP
  meta_data <- c("Cal_yrs_BP")
  counts = counts[,!(names(counts) %in% meta_data)]
  
  #delete cf. taxa
  counts = counts[,!grepl("^cf.",names(counts))]
  
  #delete uncertain taxa
  drop <- c("Spermatophyta undiff. (aquatics)","Unknown (monolete, type A)","Unknown (monolete, type B)")
  counts = counts[,!(names(counts) %in% drop)]
  
  #rename some of the columns
  names(counts)[names(counts) == 'Arecaceae undiff.'] <- 'Arecaceae'
  names(counts)[names(counts) == 'Euphorbiaceae undiff.'] <- 'Euphorbiaceae'
  names(counts)[names(counts) == 'Rhamnaceae undiff.'] <- 'Rhamnaceae'
  names(counts)[names(counts) == 'Rubiaceae undiff.'] <- 'Rubiaceae'
  names(counts)[names(counts) == 'Sphaerostephanos'] <- 'Thelypteris'#synonym
  names(counts)[names(counts) == 'Rapanea'] <- 'Myrsine'#synonym
  names(counts)[names(counts) == 'Limnanthemum'] <- 'Nymphoides'#synonym
  
  #Add age information
  counts$Cal_yrs_BP <-Cal_yrs_BP
  
  #aggregate all taxa names
  bon_mixed<-t(rowsum(t(counts), group = colnames(counts), na.rm = T))
  bon_mixed<-as.data.frame(bon_mixed)
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
  
  names(counts)[1]<-"Cal_yrs_BP"
  Cal_yrs_BP<- counts$Cal_yrs_BP
  meta_data <- c("Cal_yrs_BP")
  counts = counts[,!(names(counts) %in% meta_data)]
  
  #delete uncertain taxa
  drop <- c("Unknown (monolete, type A)", "Unknown (monolete, type B)", "Unknown (trilete)")
  counts = counts[,!(names(counts) %in% drop)]
  
  #rename some of the columns
  names(counts)[names(counts) == 'Acronychia comp.'] <- 'Acronychia'
  names(counts)[names(counts) == 'Araliaceae undiff.'] <- 'Araliaceae'
  names(counts)[names(counts) == 'Blechnum comp.'] <- 'Blechnum'
  names(counts)[names(counts) == 'Collospermum'] <- 'Astelia'#synonym
  names(counts)[names(counts) == 'Cunoniaceae (dicolpate)'] <- 'Cunoniaceae'
  names(counts)[names(counts) == 'Cunoniaceae (tricolpate)'] <- 'Cunoniaceae'
  names(counts)[names(counts) == 'Cyperaceae (type 1)'] <- 'Cyperaceae'
  names(counts)[names(counts) == 'Eleocharis comp.'] <- 'Eleocharis'
  names(counts)[names(counts) == 'Euphorbiaceae undiff.'] <- 'Euphorbiaceae'
  names(counts)[names(counts) == 'Fabaceae (type 1)'] <- 'Fabaceae'
  names(counts)[names(counts) == 'Fabaceae (type 2)'] <- 'Fabaceae'
  names(counts)[names(counts) == 'Haloragis sim.'] <- 'Haloragis'
  names(counts)[names(counts) == 'Lepironia comp.'] <- 'Lepironia'
  names(counts)[names(counts) == 'Medinilla comp.'] <- 'Medinilla'
  names(counts)[names(counts) == 'Myrtaceae (type 1)'] <- 'Myrtaceae'
  names(counts)[names(counts) == 'Myrtaceae (type 2)'] <- 'Myrtaceae'
  names(counts)[names(counts) == 'Myrtaceae (type 3)'] <- 'Myrtaceae'
  names(counts)[names(counts) == 'Myrtaceae (type 4)'] <- 'Myrtaceae'
  names(counts)[names(counts) == 'Neonauclea comp.'] <- 'Neonauclea'
  names(counts)[names(counts) == 'Parasponia'] <- 'Trema'#synonym
  names(counts)[names(counts) == 'Parinari comp.'] <- 'Parinari'
  names(counts)[names(counts) == 'Pemphis comp.'] <- 'Pemphis'
  names(counts)[names(counts) == 'Pisonia comp.'] <- 'Pisonia'
  names(counts)[names(counts) == 'Potamogeton comp.'] <- 'Potamogeton'
  names(counts)[names(counts) == 'Quintinia comp.'] <- 'Quintinia'
  names(counts)[names(counts) == 'Rhamnaceae undiff.'] <- 'Rhamnaceae'
  names(counts)[names(counts) == 'Rosaceae undiff.'] <- 'Rosaceae'
  names(counts)[names(counts) == 'Rubiaceae undiff.'] <- 'Rubiaceae'
  names(counts)[names(counts) == 'Rubus comp.'] <- 'Rubus'
  names(counts)[names(counts) == 'Rutaceae undiff.'] <- 'Rutaceae'
  names(counts)[names(counts) == 'Scaevola comp.'] <- 'Scaevola'
  names(counts)[names(counts) == 'Schefflera comp.'] <- 'Schefflera'
  names(counts)[names(counts) == 'Urticaceae (type 1)'] <- 'Urticaceae'
  names(counts)[names(counts) == 'Urticaceae (type 2)'] <- 'Urticaceae'
  names(counts)[names(counts) == 'Rapanea'] <- 'Myrsine' #synonym
  names(counts)[names(counts) == 'Sphaerostephanos'] <- 'Thelypteris' #synonym
  names(counts)[names(counts) == 'Cleistocalyx'] <- 'Syzygium' #synonym
  
  #Add age information
  counts$Cal_yrs_BP <-Cal_yrs_BP
  
  #aggregate all taxa names
  tag_mixed<-t(rowsum(t(counts), group = colnames(counts), na.rm = T))
  tag_mixed<-as.data.frame(tag_mixed)
}

#8. Yacata (Fiji)####
if(1){
  counts<-read.csv2(paste("yaca_pollen.csv",sep=""))
  counts<-counts[-c(1)]
  names(counts)[1]<-"Cal_yrs_BP"
  Cal_yrs_BP<- counts$Cal_yrs_BP
  
  meta_data <- c("Cal_yrs_BP")
  counts = counts[,!(names(counts) %in% meta_data)]
  
  #delete uncertain taxa
  drop <- c("Trema.Moraceae.Celtis", "gemmate.monolete", "Treeferns", "Triletes.Other", "Monolete.Other", "Monolete.Psilate", "Damaged.Crumpled","Indeterminate.3C3P", "Indeterminate.Other", "Type.4", "Type.6")
  counts = counts[,!(names(counts) %in% drop)]
  
  names(counts)[names(counts) == 'Rubiaceae.3c.loose.weave'] <- 'Rubiaceae'
  names(counts)[names(counts) == 'Psychotria.Type.1'] <- 'Psychotria'
  names(counts)[names(counts) == 'Psychotria.Type.2'] <- 'Psychotria'
  names(counts)[names(counts) == 'Terminalia.Type'] <- 'Combretaceae'
  names(counts)[names(counts) == 'X3cp.25.30um.Araliaceae'] <- 'Araliaceae'
  names(counts)[names(counts) == 'Other.Malvaceae'] <- 'Malvaceae'
  names(counts)[names(counts) == 'Chenopodiaceae'] <- 'Amaranthaceae'#synonym
  names(counts)[names(counts) == 'Lonicera.Type'] <- 'Caprifoliaceae'
  names(counts)[names(counts) == 'Nymphoides.indica'] <- 'Nymphoides indica'
  names(counts)[names(counts) == 'Pandandus.tectorius'] <- 'Pandanus tectorius'
  names(counts)[names(counts) == 'Pandanus.Type'] <- 'Pandanaceae'
  names(counts)[names(counts) == 'Dodonea'] <- 'Dodonaea' #spelling mistake
  names(counts)[names(counts) == 'Barringtoniaceae'] <- 'Lecythidaceae'#synonym
  names(counts)[names(counts) == 'Palms'] <- 'Arecaceae' #informal synonym
  names(counts)[names(counts) == 'Macaranga.Mallotus'] <- 'Euphorbiaceae'
  names(counts)[names(counts) == 'Homalanthus.Exocaria'] <- 'Euphorbiaceae'
  
  #Add age information
  counts$Cal_yrs_BP <-Cal_yrs_BP
  
  #aggregate all taxa names
  yac_mixed<-t(rowsum(t(counts), group = colnames(counts), na.rm = T))
  yac_mixed<-as.data.frame(yac_mixed)
}

#9. Finemui Swamp (Tonga)####
if(1){
  counts<-read.csv("fine_pollen.csv")
  names(counts)[1]<-"Cal_yrs_BP"
  counts<-counts[!is.na(counts$Cal_yrs_BP),]
  Cal_yrs_BP<- counts$Cal_yrs_BP
  meta_data <- c("Cal_yrs_BP")
  counts = counts[,!(names(counts) %in% meta_data)]
  
  #delete uncertain taxa
  drop <- c("Filices..Monolete.Psilete.Folded", "Filices..Monolete.Psilete.Large", "Filices..Monolete.Psilete.Small", "Meliaceae..Sapotaceae", "Urticaceae..Moraceae")
  counts = counts[,!(names(counts) %in% drop)]
  
  #rename some of the columns
  names(counts)[names(counts) == 'Blyxa.comp.'] <- 'Blyxa'
  names(counts)[names(counts) == 'Chenopodiaceae'] <- 'Amaranthaceae' #synonym
  names(counts)[names(counts) == 'Cocos.nucifera'] <- 'Cocos nucifera'
  names(counts)[names(counts) == 'Compositae..Liguliflorae.'] <- 'Cichorioideae' #synonym sub-familiy not on POWO
  names(counts)[names(counts) == 'Compositae..Tubuliflorae.'] <- 'Asteroideae' #synonym sub-familiy not on POWO
  names(counts)[names(counts) == 'Cyathula.comp.'] <- 'Cyathula'
  names(counts)[names(counts) == 'Elaeocarpus.comp.'] <- 'Elaeocarpus'
  names(counts)[names(counts) == 'Euphorbia.sim.'] <- 'Euphorbia'
  names(counts)[names(counts) == 'Excoecaria.sim.'] <- 'Excoecaria'
  names(counts)[names(counts) == 'Filices..Monolete.Asplenium'] <- 'Asplenium'
  names(counts)[names(counts) == 'Filices..Monolete.Lepisorus..sim.'] <- 'Lepisorus'
  names(counts)[names(counts) == 'Filices..Trilete.Hymenophyllaceae.Comp.'] <- 'Hymenophyllaceae'
  names(counts)[names(counts) == 'Freycinetia.comp.'] <- 'Freycinetia'
  names(counts)[names(counts) == 'Gramineae'] <- 'Poaceae' #synonym
  names(counts)[names(counts) == 'Ipomea..cultivated'] <- 'Ipomoea'
  names(counts)[names(counts) == 'Leguminosae'] <- 'Fabaceae' #synonym
  names(counts)[names(counts) == 'Leucas.comp.'] <- 'Leucas'
  names(counts)[names(counts) == 'Lycopodium..reticulate'] <- 'Lycopodium'
  names(counts)[names(counts) == 'Potamogeton.comp.'] <- 'Potamogeton'
  names(counts)[names(counts) == 'Premna.comp.'] <- 'Premna'
  names(counts)[names(counts) == 'Tournefortia.comp.'] <- 'Tournefortia'
  names(counts)[names(counts) == 'Typha.latifolia'] <- 'Typha latifolia'
  
  #Add age information
  counts$Cal_yrs_BP <-Cal_yrs_BP
  
  #aggregate all taxa names
  fin_mixed<-t(rowsum(t(counts), group = colnames(counts), na.rm = T))
  fin_mixed<-as.data.frame(fin_mixed)
}

#10. Lotofoa Swamp (Tonga)####
if(1){
  counts<-read.csv("loto_pollen.csv")
  names(counts)[1]<-"Cal_yrs_BP"
  counts<-counts[!is.na(counts$Cal_yrs_BP),]
  Cal_yrs_BP<- counts$Cal_yrs_BP
  meta_data <- c("Cal_yrs_BP")
  counts = counts[,!(names(counts) %in% meta_data)]
  
  #delete uncertain taxa
  drop <- c("Filices..Monolete.Psilete.Folded", "Filices..Monolete.Psilete.Large", "Filices..Monolete.Psilete.Small", "Urticaceae..Moraceae")
  counts = counts[,!(names(counts) %in% drop)]
  
  #rename some of the columns
  names(counts)[names(counts) == 'Cocos.nucifera'] <- 'Cocos nucifera'
  names(counts)[names(counts) == 'Compositae..Tubuliflorae.'] <- 'Asteroideae' #synonym sub-familiy not on POWO
  names(counts)[names(counts) == 'Elaeocarpus.comp.'] <- 'Elaeocarpus'
  names(counts)[names(counts) == 'Euphorbia.sim.'] <- 'Euphorbia'
  names(counts)[names(counts) == 'Excoecaria.sim.'] <- 'Excoecaria'
  names(counts)[names(counts) == 'Filices..Monolete.Stenochlaena.palust.'] <- 'Stenochlaena palustris'
  names(counts)[names(counts) == 'Gramineae'] <- 'Poaceae' #synonym
  names(counts)[names(counts) == 'Ipomea..wild'] <- 'Ipomoea'
  names(counts)[names(counts) == 'Leguminosae'] <- 'Fabaceae' #synonym
  names(counts)[names(counts) == 'Maesa.sim.'] <- 'Maesa'
  names(counts)[names(counts) == 'Mallotus.comp.'] <- 'Mallotus'
  names(counts)[names(counts) == 'Momordica.comp.'] <- 'Momordica'
  names(counts)[names(counts) == 'Morinda.comp.'] <- 'Morinda'
  names(counts)[names(counts) == 'Nymphoides.comp.'] <- 'Nymphoides'
  names(counts)[names(counts) == 'Plantago.comp.'] <- 'Plantago'
  names(counts)[names(counts) == 'Premna.comp.'] <- 'Premna'
  names(counts)[names(counts) == 'Schefflera.sim.'] <- 'Schefflera'
  names(counts)[names(counts) == 'Syzygium.comp.'] <- 'Syzygium'
  names(counts)[names(counts) == 'Triumfetta.sp.'] <- 'Triumfetta'
  
  #Add age information
  counts$Cal_yrs_BP <-Cal_yrs_BP
  
  #aggregate all taxa names
  lot_mixed<-t(rowsum(t(counts), group = colnames(counts), na.rm = T))
  lot_mixed<-as.data.frame(lot_mixed)
}

#11. Ngofe Marsh (Tonga)####
if(1){
  counts<-read.csv("ngof_pollen.csv")
  names(counts)[1]<-"Cal_yrs_BP"
  Cal_yrs_BP<- counts$Cal_yrs_BP
  meta_data <- c("Cal_yrs_BP")
  counts = counts[,!(names(counts) %in% meta_data)]
  
  #delete cf. taxa
  counts = counts[,!grepl("^cf.",names(counts))]
  
  #delete uncertain taxa
  drop <- c("Combretaceae.Melastromataceae", "Melastromataceae.Sapotaceae", "Monolete.echinate", "Monoletes.psilate", "Monolete.spores", "Moraceae.Urticaceae", "Triletes.smooth", "Trilete.spores")
  counts = counts[,!(names(counts) %in% drop)]
  
  #Add age information
  counts$Cal_yrs_BP <-Cal_yrs_BP
  
  #aggregate all taxa names
  ngo_mixed<-t(rowsum(t(counts), group = colnames(counts), na.rm = T))
  ngo_mixed<-as.data.frame(ngo_mixed)
}

#12. Avai'o'vuna Swamp (Tonga)####
if(1){
  counts<-read.csv("avai_pollen.csv")
  names(counts)[1]<-"Cal_yrs_BP"
  Cal_yrs_BP<- counts$Cal_yrs_BP
  meta_data <- c("Cal_yrs_BP")
  counts = counts[,!(names(counts) %in% meta_data)]
  
  #delete uncertain taxa
  drop <- c("Indeterminant", "Monolete.spores", "Trilete.spores", "Unknown")
  counts = counts[,!(names(counts) %in% drop)]
  
  #rename some of the columns
  names(counts)[names(counts) == 'Ambrosia.type'] <- 'Asteraceae'
  names(counts)[names(counts) == 'Chenopodiaceae.Amaranthus'] <- 'Amaranthus'
  names(counts)[names(counts) == 'Freyeinetia'] <- 'Freycinetia'
  names(counts)[names(counts) == 'Omalanthus'] <- 'Homalanthus' #synonym
  names(counts)[names(counts) == 'Ipomoea.cf..batatas'] <- 'Ipomoea'
  names(counts)[names(counts) == 'Lorantheceae'] <- 'Loranthaceae'
  names(counts)[names(counts) == 'Paplionaceae'] <- 'Fabaceae' #synonym
  names(counts)[names(counts) == 'Poaceae..60m'] <- 'Poaceae'
  names(counts)[names(counts) == 'Poaceae.2.pores'] <- 'Poaceae'
  names(counts)[names(counts) == 'Polgonum'] <- 'Polygonum'
  
  #delete cf. taxa
  counts = counts[,!grepl("^cf.",names(counts))]
  
  counts$Asteraceae_sum<-rowSums(counts[c("Asteraceae", "Asteraceae.1")])
  counts$Ipomoea_sum<-rowSums(counts[c("Ipomoea", "Ipomoea.1")])
  counts$Poaceae_sum<-rowSums(counts[c("Poaceae", "Poaceae.1", "Poaceae.2")])
  
  drop <- c("Asteraceae", "Asteraceae.1","Ipomoea", "Ipomoea.1", "Poaceae", "Poaceae.1", "Poaceae.2")
  counts = counts[,!(names(counts) %in% drop)]
  
  names(counts)[names(counts) == 'Asteraceae_sum'] <- 'Asteraceae'
  names(counts)[names(counts) == 'Ipomoea_sum'] <- 'Ipomoea'
  names(counts)[names(counts) == 'Poaceae_sum'] <- 'Poaceae'
  
  #Add age information
  counts$Cal_yrs_BP <-Cal_yrs_BP
  
  #aggregate all taxa names
  ava_mixed<-t(rowsum(t(counts), group = colnames(counts), na.rm = T))
  ava_mixed<-as.data.frame(ava_mixed)
}

#13. Lake Lanoto'o (Samoa)####
if(1){
  counts<-read.csv("lano_pollen.csv")
  names(counts)[1]<-"Cal_yrs_BP"
  Cal_yrs_BP<- counts$Cal_yrs_BP
  meta_data <- c("Cal_yrs_BP")
  counts = counts[,!(names(counts) %in% meta_data)]
  
  #delete cf. taxa
  counts = counts[,!grepl("^cf.",names(counts))]
  
  #delete uncertain taxa
  drop <- c("Monolete.spore.undif.", "Moraceae.Urticaceae.undif.", "Sum.unknown", "Trilete.spore.undif.", "Euphorbiaceae.type")
  counts = counts[,!(names(counts) %in% drop)]
  
  #rename some of the columns
  names(counts)[names(counts) == 'Altingia.sp.'] <- 'Araucaria' #synonym
  names(counts)[names(counts) == 'Alyxia.sp.'] <- 'Alyxia'
  names(counts)[names(counts) == 'Antidesma.sp.'] <- 'Antidesma'
  names(counts)[names(counts) == 'Apiaceae.undif.'] <- 'Apiaceae'
  names(counts)[names(counts) == 'Apocynaceae.undif.'] <- 'Apocynaceae'
  names(counts)[names(counts) == 'Araceae.undif.'] <- 'Araceae'
  names(counts)[names(counts) == 'Araliaceae.undif.'] <- 'Araliaceae'
  names(counts)[names(counts) == 'Ascarina.lanceolata'] <- 'Ascarina lucida' #synonym
  names(counts)[names(counts) == 'Asteraceae.undif.'] <- 'Asteraceae'
  names(counts)[names(counts) == 'Bombacaceae.undif.'] <- 'Malvaceae'#synonym
  names(counts)[names(counts) == 'Celtis.sp.'] <- 'Celtis'
  names(counts)[names(counts) == 'Cyathaceae.undif.'] <- 'Cyatheaceae'
  names(counts)[names(counts) == 'Cyperaceae.undif.'] <- 'Cyperaceae'
  names(counts)[names(counts) == 'Dysoxylum.sp.'] <- 'Dysoxylum'
  names(counts)[names(counts) == 'Elaeoecarpus.sp.'] <- 'Elaeocarpus'
  names(counts)[names(counts) == 'Euphorbiaceae.undif.'] <- 'Euphorbiaceae'
  names(counts)[names(counts) == 'Fabaceae.undif.'] <- 'Fabaceae'
  names(counts)[names(counts) == 'Gunneraceae.undif.'] <- 'Gunneraceae'
  names(counts)[names(counts) == 'Loranthus.insularum'] <- 'Decaisnina forsteriana'#synonym
  names(counts)[names(counts) == 'Melastomataceae.undif.'] <- 'Melastomataceae'
  names(counts)[names(counts) == 'Myrsina.sp.'] <- 'Myrsine' #spelling mistake
  names(counts)[names(counts) == 'Myrtaceae.undif.'] <- 'Myrtaceae'
  names(counts)[names(counts) == 'Pandanus.sp.'] <- 'Pandanus'
  names(counts)[names(counts) == 'Poaceae.undif.'] <- 'Poaceae'
  names(counts)[names(counts) == 'Rubiaceae.undif.'] <- 'Rubiaceae'
  names(counts)[names(counts) == 'Rumex.sp.'] <- 'Rumex'
  names(counts)[names(counts) == 'Sapindaceae.undif.'] <- 'Sapindaceae'
  names(counts)[names(counts) == 'Sapotaceae.undif.'] <- 'Sapotaceae'
  names(counts)[names(counts) == 'Selaginella.sp.'] <- 'Selaginella'
  names(counts)[names(counts) == 'Solanaceae.undif.'] <- 'Solanaceae'
  names(counts)[names(counts) == 'Solanum.type'] <- 'Solanaceae'
  names(counts)[names(counts) == 'Tiliaceae.undif.'] <- 'Malvaceae' #synonym
  names(counts)[names(counts) == 'Trema.sp.'] <- 'Trema'
  names(counts)[names(counts) == 'Weinmannia.sp.'] <- 'Weinmannia'
  
  #Add age information
  counts$Cal_yrs_BP <-Cal_yrs_BP
  
  #aggregate all taxa names
  lan_mixed<-t(rowsum(t(counts), group = colnames(counts), na.rm = T))
  lan_mixed<-as.data.frame(lan_mixed)
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
  
  names(counts)[1]<-"Cal_yrs_BP"
  Cal_yrs_BP<- counts$Cal_yrs_BP
  meta_data <- c("Cal_yrs_BP")
  counts = counts[,!(names(counts) %in% meta_data)]
  
  #delete uncertain taxa
  drop <- c("Bryophyta/Marchantiophyta", "Dryopteridaceae/Polypodiaceae", "Foraminifera", "Spermatophyta undiff.", "Unknown (monolete, psilate)", "Unknown (trilete, psilate)", "Urticaceae/Moraceae")
  counts = counts[,!(names(counts) %in% drop)]
  
  #rename some of the columns
  names(counts)[names(counts) == 'Arecaceae incertae sedis'] <- 'Arecaceae'
  names(counts)[names(counts) == 'Euphorbiaceae undiff.'] <- 'Euphorbiaceae'
  names(counts)[names(counts) == 'Freycinetia sp.'] <- 'Freycinetia'
  names(counts)[names(counts) == 'Homalanthus cf. H. stokesii'] <- 'Homalanthus'
  names(counts)[names(counts) == 'Malvaceae undiff.'] <- 'Malvaceae'
  names(counts)[names(counts) == 'Myoporum rapense'] <- 'Myoporum rapense'
  names(counts)[names(counts) == 'Poaceae (<40 µm)'] <- 'Poaceae'
  names(counts)[names(counts) == 'Poaceae (>40 µm)'] <- 'Poaceae'
  names(counts)[names(counts) == 'Sonchus cf. S. asper'] <- 'Sonchus'
  
  #delete cf. taxa
  counts = counts[,!grepl("^cf.",names(counts))]
  
  #Add age information
  counts$Cal_yrs_BP <-Cal_yrs_BP
  
  #aggregate all taxa names
  tuk_mixed<-t(rowsum(t(counts), group = colnames(counts), na.rm = T))
  tuk_mixed<-as.data.frame(tuk_mixed)
}

#15. Rano Aroi (Chile)####
if(1){
  counts<-read.csv("aroi_pollen.csv")
  names(counts)[1]<-"Cal_yrs_BP"
  Cal_yrs_BP<- counts$Cal_yrs_BP
  meta_data <- c("Cal_yrs_BP")
  counts = counts[,!(names(counts) %in% meta_data)]
  
  #delete uncertain taxa
  drop <- c("Filices..monolete.psilate", "Filices..monolete.verrucate..oreolate", "Filices..trilete.psilate", "Filices..trilete.verrucate", "Unknown")
  counts = counts[,!(names(counts) %in% drop)]
  
  #rename some of the columns
  names(counts)[names(counts) == 'Acalypha.comp.'] <- 'Acalypha'
  names(counts)[names(counts) == 'Compositae.Tubuliflorae'] <- 'Asteroideae' #sub-familiy not on POWO
  names(counts)[names(counts) == 'Cyperaceae.Cyperus.comp.'] <- 'Cyperus'
  names(counts)[names(counts) == 'Cyperaceae.Scirpus.comp.'] <- 'Scirpus'
  names(counts)[names(counts) == 'Euphorbia.comp..Seprns.sim.'] <- 'Euphorbia'
  names(counts)[names(counts) == 'Gramineae'] <- 'Poaceae' #synonym
  names(counts)[names(counts) == 'Lycopodium..foveolate'] <- 'Lycopodium'
  names(counts)[names(counts) == 'Macaranga.comp.'] <- 'Macaranga'
  names(counts)[names(counts) == 'Myrtaceae.undiff.'] <- 'Myrtaceae'
  names(counts)[names(counts) == 'Palmae'] <- 'Arecaceae'#synonym
  names(counts)[names(counts) == 'Plantago.id.lanceolata.comp.'] <- 'Plantago lanceolata'
  names(counts)[names(counts) == 'Typha.angustifolia.sim.'] <- 'Typha angustifolia'
  names(counts)[names(counts) == 'Umbelliferae.comp...Apium.sim.'] <- 'Apium'
  
  #Add age information
  counts$Cal_yrs_BP <-Cal_yrs_BP
  
  #aggregate all taxa names
  aro_mixed<-t(rowsum(t(counts), group = colnames(counts), na.rm = T))
  aro_mixed<-as.data.frame(aro_mixed)
}

#Combine records####
#add site names
lou_mixed$Site<-"St. Louis Lac" #1
plu_mixed$Site<-"Plum Swamp" #2
ano_mixed$Site<-"Anouwe Swamp" #3
wai_mixed$Site<-"Waitetoke"#4
vol_mixed$Site<-"Volivoli" #5
bon_mixed$Site<-"Bonatoa Bog"#6
tag_mixed$Site<-"Lake Tagimaucia"#7
yac_mixed$Site<-"Yacata" #8
fin_mixed$Site<-"Finemui Swamp" #9
lot_mixed$Site<-"Lotofoa Swamp" #10
ngo_mixed$Site<-"Ngofe Marsh" #11
ava_mixed$Site<-"Avai'o'vuna Swamp" #12
lan_mixed$Site<-"Lake Lanoto'o" #13
tuk_mixed$Site<-"Tukou Marsh"#14
aro_mixed$Site<-"Rano Aroi" #15

#merge together
mixed<-merge(lou_mixed, plu_mixed, all=TRUE)
mixed<-merge(mixed, ano_mixed, all=TRUE)
mixed<-merge(mixed, wai_mixed, all=TRUE)
mixed<-merge(mixed, vol_mixed, all=TRUE)
mixed<-merge(mixed, bon_mixed, all=TRUE)
mixed<-merge(mixed, tag_mixed, all=TRUE)
mixed<-merge(mixed, yac_mixed, all=TRUE)
mixed<-merge(mixed, fin_mixed, all=TRUE)
mixed<-merge(mixed, lot_mixed, all=TRUE)
mixed<-merge(mixed, ngo_mixed, all=TRUE)
mixed<-merge(mixed, ava_mixed, all=TRUE)
mixed<-merge(mixed, lan_mixed, all=TRUE)
mixed<-merge(mixed, tuk_mixed, all=TRUE)
mixed<-merge(mixed, aro_mixed, all=TRUE)

#remove dates which are >5000
mixed<-arrange(mixed, Cal_yrs_BP)
mixed<-mixed[mixed$Cal_yrs_BP < 5000, ]
mixed[is.na(mixed)] <- 0
#remove rows which contain all 0s
mixed = mixed[rowSums(mixed[2,4:366])>0,]
#write.csv(mixed, "mixed_ungrouped.csv")
