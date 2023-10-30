#Calculate mean pollen abundance before and after human colonisation
library(dplyr)

#Stand1####

data<-read.csv("Outputs/Stand1_binned.csv", check.names = FALSE)
data<-data[-c(1)]

#columns 55, 158, 353 are metadata
data<-data[c(55, 158, 353, 1:54, 56:157, 159:352, 354:397)]

names(data)[2]<-"interval"
data$interval<-gsub("]","",as.character(data$interval))
data$interval<-gsub("\\(|\\)","",as.character(data$interval))

split<-strsplit(data$interval, split = ",")
matrix<-matrix(unlist(split),ncol=2,byrow=T)
matrix<-as.data.frame(matrix)
matrix$V1 <- as.numeric(matrix$V1)
matrix$V2 <- as.numeric(matrix$V2)
matrix$mean_interval_group<-rowMeans(matrix[1:2], na.rm=TRUE)

drop <- c("interval")
data = data[,!(names(data) %in% drop)]
data$mean_interval_age<-matrix$mean_interval_group
data<-data[c(397, 1:396)]

#drop columns when the column sum= 0
meta<-data[1:3]
data<-data[4:397]
data<-Filter(function(data) sum(abs(data), na.rm = TRUE) > 0, data)
data<-cbind(meta,data)

late<-filter(data, Site %in% c('wai', 'tuk', 'aro'))
early<-filter(data, !(Site %in% c('wai', 'tuk', 'aro')))

late_pre<-filter(late, Cal_yrs_BP >700)
late_post<-filter(late, Cal_yrs_BP <=700)

early_pre<-filter(early, Cal_yrs_BP >3000)
early_post<-filter(early, Cal_yrs_BP <=3000)

pre<-rbind(late_pre,early_pre)
post<-rbind(late_post,early_post)

pre<-pre[-c(1:3)]
post<-post[-c(1:3)]

pre_mean <- summarize_all(pre, mean)
max(pre_mean)

post_mean <- summarize_all(post, mean)
max(post_mean)

pre_mean$col<-"pre human colonisation"
post_mean$col<-"post human colonisation"

all<-rbind(pre_mean, post_mean)
#write.csv(all, "Outputs/Stand1_mean_abundance.csv")

#Stand2####
data<-read.csv("Outputs/Stand2_binned.csv", check.names = FALSE)
data<-data[-c(1)]

#columns 51, 153, 336 are metadata
data<-data[c(51, 153, 336, 1:50, 52:152, 154:335, 337:373)]

names(data)[2]<-"interval"
data$interval<-gsub("]","",as.character(data$interval))
data$interval<-gsub("\\(|\\)","",as.character(data$interval))

split<-strsplit(data$interval, split = ",")
matrix<-matrix(unlist(split),ncol=2,byrow=T)
matrix<-as.data.frame(matrix)
matrix$V1 <- as.numeric(matrix$V1)
matrix$V2 <- as.numeric(matrix$V2)
matrix$mean_interval_group<-rowMeans(matrix[1:2], na.rm=TRUE)

drop <- c("interval")
data = data[,!(names(data) %in% drop)]
data$mean_interval_age<-matrix$mean_interval_group
data<-data[c(373, 1:372)]

#drop columns when the column sum= 0
meta<-data[1:3]
data<-data[4:372]
data<-Filter(function(data) sum(abs(data), na.rm = TRUE) > 0, data)
data<-cbind(meta,data)

late<-filter(data, Site %in% c('wai', 'tuk', 'aro'))
early<-filter(data, !(Site %in% c('wai', 'tuk', 'aro')))

late_pre<-filter(late, Cal_yrs_BP >700)
late_post<-filter(late, Cal_yrs_BP <=700)

early_pre<-filter(early, Cal_yrs_BP >3000)
early_post<-filter(early, Cal_yrs_BP <=3000)

pre<-rbind(late_pre,early_pre)
post<-rbind(late_post,early_post)

pre<-pre[-c(1:3)]
post<-post[-c(1:3)]

pre_mean <- summarize_all(pre, mean)
max(pre_mean)

post_mean <- summarize_all(post, mean)
max(post_mean)

pre_mean$col<-"pre human colonisation"
post_mean$col<-"post human colonisation"

all<-rbind(pre_mean, post_mean)
#write.csv(all, "Outputs/Stand2_mean_abundance.csv")

