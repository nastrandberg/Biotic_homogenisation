### unify taxa according to Plants of the World Online (POWO)

library(taxize)

mix <- read.csv("mixed_ungrouped.csv", check.names = FALSE)
mix<-mix[-c(1)]
#POWO does not contain subfamilies or hornworts so these have been removed to stop error messages from appearing
#and for some reason there is a problem with some species names despite being accepted on POWO
drop <- c("Cal_yrs_BP", "Site", "Epacrideae", "Cichorioideae", "Asteroideae", "Anthoceros", "Bryosporis problematicus")
mix = mix[,!(names(mix) %in% drop)]

names_before <- as.data.frame(colnames(mix[,2:ncol(mix)]))
colnames(names_before) <- "taxa"
names_before$accepted <- NA

# Loop does not always run through, but can be restarted where it stopped.

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

names_before[nrow(names_before) + 1,] = c("Epacrideae","subfamily")
names_before[nrow(names_before) + 1,] = c("Cichorioideae","subfamily")
names_before[nrow(names_before) + 1,] = c("Asteroideae","subfamily")
names_before[nrow(names_before) + 1,] = c("Anthoceros","genus")
names_before[nrow(names_before) + 1,] = c("Bryosporis problematicus","species")

#write.csv(names_before, "taxonomy_check.csv")
