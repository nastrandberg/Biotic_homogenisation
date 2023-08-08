#here we produce the boxplots to show the similarity
#of sites where neither, one, or both sites were
#settled by people during any particular interval

library(dplyr)
library(tidyverse)
dat<-read.csv("Outputs/max_dat.csv")

dat<-dat[-c(1)]

dat$group <- cut(dat$times, breaks = c(-Inf, 650, 3150, Inf), 
                 labels = c("both", "tbc", "neither"))

tbc<- dat[dat$group == "tbc", ]
both<- dat[dat$group == "both", ]
neither<- dat[dat$group == "neither", ]

tbc<-tbc[-c(4)]

tbc$group <- case_when(
  grepl("^(?:aro|tuk|wai) (aro|tuk|wai)$",
        tbc$name, ignore.case=TRUE) ~ "neither",
  grepl("\\b(?:ano|tuk|wai)\\b", tbc$name, ignore.case=TRUE) ~ "one",
  TRUE ~ "both")  

dat<-rbind(both,neither,tbc)

levels(dat$group)
dat$group <- factor(dat$group , levels=c("neither", "one", "both"))
boxplot(sims ~ group, data = dat, col="lightblue", xlab = "Proportion of pair settled by humans",
        ylab = "Pairwise Bray-Curtis Similarity slope coefficients")

df<-dat %>%
  group_by(name,group) %>%
  dplyr::summarize(n())

df<-df %>%
  group_by(name) %>%
  dplyr::summarize(n())
