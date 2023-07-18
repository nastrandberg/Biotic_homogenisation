#Validation of taxa names and identification of rank using Taxize####

#remotes::install_github("ropensci/taxize")
library(taxize)

mixed<-read.csv("mixed_ungrouped.csv", check.names = FALSE)
mixed<-mixed[-c(1)]
mixed = mixed[rowSums(mixed[2,4:366])>0,]

drop <- c("X", "Cal_yrs_BP", "Site")
mixed = mixed[,!(names(mixed) %in% drop)]

library(dplyr)
mixed[] <- lapply(mixed, function(x) {
  if(is.factor(x)) as.numeric(as.character(x)) else x
})
sapply(mixed, class)

mixed<-Filter(function(mixed) sum(abs(mixed), na.rm = TRUE) > 0, mixed)

taxa<-colnames(mixed)
taxa<-sort(taxa)

#taxize::use_entrez()
#type in your own key below between the "" symbols
Sys.setenv(ENTREZ_KEY = "")

rank<-tax_rank(taxa[1:100], db= "ncbi")
rank<-as.data.frame(rank)
rank2<-tax_rank(taxa[101:200], db= "ncbi")
rank2<-as.data.frame(rank2)
rank3<-tax_rank(taxa[201:300], db= "ncbi")
rank3<-as.data.frame(rank3)
rank4<-tax_rank(taxa[301:346], db= "ncbi")
rank4<-as.data.frame(rank4)

rank_all<-cbind(rank, rank2, rank3, rank4)

#write.csv(rank_all, "rank.csv")
#go to the excel sheet and replace all NA values with correct rank
rank<-read.csv("rank.csv")
#rank<-rank[-c(1)]
library(tidyverse)
rank<-rank %>% 
  map_df(as_tibble)

#summarize the rank data
library(purrr)
rank_summary <- as.data.frame(map_if(rank, is.character, as.factor))
summary(rank_summary)
