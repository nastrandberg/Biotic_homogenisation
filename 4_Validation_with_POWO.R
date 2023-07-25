#Unify taxa according to Plants of the World Online (POWO)####

library(taxize)
library(tidyverse)

taxa <- read.csv("Outputs/harmonisation.csv", check.names = FALSE)
taxa<-taxa[!grepl("remove", taxa$max_change),]
taxa<-taxa$max_change
taxa<-as.data.frame(taxa)

#POWO does not contain subfamilies or hornworts so the following have been removed to stop error messages from appearing
taxa<-taxa[!grepl("Anthoceros", taxa$taxa),]
taxa<-as.data.frame(taxa)

#remove duplicate names
taxa<-taxa$taxa[!duplicated(taxa$taxa)]
taxa<-as.data.frame(taxa)

#names_before <- as.data.frame(colnames(mix[,2:ncol(mix)]))
names_before<-taxa
colnames(names_before) <- "taxa"
names_before$accepted <- NA

# Loop does not always run through, but can be restarted where it stopped
for(i in 1:nrow(names_before)){
  temp <- pow_lookup(get_pow(names_before[i,1], rank_filter = "genus")[1])$meta$classification
 
   temp <- pow_search(sci_com = as.character(names_before[i,1]))$data
   temp <- temp[which(temp$accepted == TRUE),]

   if(names_before[i,1] %in% temp$name){
     names_before$accepted[i] <- "TRUE"
   }else{
     names_before$accepted[i] <- "FALSE"
   }
}

#names_before[nrow(names_before) + 1,] = c("Anthoceros","genus")

#write.csv(names_before, "Outputs/taxonomy_check.csv")
