stand1<-read.csv("Outputs/Stand1_homogen.csv", check.names = F)
stand2<-read.csv("Outputs/Stand2_homogen.csv", check.names = F)

stand1$type<-"1"
stand2$type<-"2"

data<-rbind(stand1, stand2)

library(MASS) 
library(reshape2) 
library(reshape) 

names(data)[1]<-"site"
data <- melt(data, id = c("site","type"))
data<-data[-c(1)]

data$variable <- factor(data$variable , levels=c("Avai’o’vuna Swamp",
                                                 "Finemui Swamp",
                                                 "Lotofoa Swamp",
                                                 "Ngofe Marsh",
                                                 "Yacata",
                                                 "St. Louis Lac",
                                                 "Rano Aroi",
                                                 "Plum Swamp",
                                                 "Tukou Marsh",
                                                 "Waitetoke",
                                                 "Anouwe Swamp",
                                                 "Lake Lanoto'o",
                                                 "Lake Tagimaucia",
                                                 "Volivoli",
                                                 "Bonatoa Bog"))


boxplot(value ~type + variable, data = data, las=3,
        col = c("#27408B","#4876ff"),
        pch= c(21, 24),
        outcol=c(("royalblue4"),("royalblue1")))

        abline(h=0, col= "orange", lwd=1, lty=5)
