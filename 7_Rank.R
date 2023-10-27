#Obtain rank using taxize####

#remotes::install_github("ropensci/taxize")

#Standardisation 1####
mixed<-read.csv("Outputs/Stand1_unbinned.csv", check.names = FALSE)
mixed<-mixed[-c(1)]
mixed = mixed[rowSums(mixed[5:398])>0,]

drop <- c("Cal_yrs_BP", "site", "min", "max")
mixed = mixed[,!(names(mixed) %in% drop)]

mixed[] <- lapply(mixed, function(x) {
  if(is.factor(x)) as.numeric(as.character(x)) else x
})
sapply(mixed, class)

mixed<-Filter(function(mixed) sum(abs(mixed), na.rm = TRUE) > 0, mixed)

taxa<-colnames(mixed)
taxa<-sort(taxa)

#taxize::use_entrez()
#type in your own key below between the "" symbols
Sys.setenv(ENTREZ_KEY = "ced229983ed7ac5c1f391518ff60d6ea4508")

rank<-tax_rank(taxa[1:100], db= "ncbi")
rank<-as.data.frame(rank)
rank2<-tax_rank(taxa[101:200], db= "ncbi")
rank2<-as.data.frame(rank2)
rank3<-tax_rank(taxa[201:300], db= "ncbi")
rank3<-as.data.frame(rank3)
rank4<-tax_rank(taxa[301:383], db= "ncbi")
rank4<-as.data.frame(rank4)

rank_all<-cbind(rank, rank2, rank3, rank4)

#write.csv(rank_all, "Outputs/Stand1_rank.csv")
#go to the excel sheet and replace all NA values with correct rank
rank<-read.csv("Outputs/Stand1_rank.csv", check.names = FALSE)
rank<-rank[-c(1)]
rank<-rank %>% 
  map_df(as_tibble)

#summarise the rank data
rank_summary <- as.data.frame(map_if(rank, is.character, as.factor))
summary(rank_summary)

#Standardisation 2####
library(dplyr)
library(purrr)
library(taxize)
library(tidyverse)

mixed<-read.csv("Outputs/Stand2_unbinned.csv", check.names = FALSE)
mixed<-mixed[-c(1)]
mixed = mixed[rowSums(mixed[5:374])>0,]

drop <- c("Cal_yrs_BP", "site", "min", "max")
mixed = mixed[,!(names(mixed) %in% drop)]

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
rank4<-tax_rank(taxa[301:361], db= "ncbi")
rank4<-as.data.frame(rank4)

rank_all<-cbind(rank, rank2, rank3, rank4)

#write.csv(rank_all, "Outputs/Stand2_rank.csv")
#go to the excel sheet and replace all NA values with correct rank
rank<-read.csv("Outputs/Stand2_rank.csv", check.names = FALSE)
rank<-rank[-c(1)]
rank<-rank %>% 
  map_df(as_tibble)

#summarise the rank data
rank_summary <- as.data.frame(map_if(rank, is.character, as.factor))
summary(rank_summary)