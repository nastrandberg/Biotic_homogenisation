stand1<-read.csv("Outputs/Stand1_homogen.csv", check.names = F)
stand2<-read.csv("Outputs/Stand2_homogen.csv", check.names = F)

stand1<-stand1[-c(1)]
stand2<-stand2[-c(1)]

stand1[stand1 >0] <- "dif1"
stand1[stand1 <0] <- "hom1"

stand2[stand2 >0] <- "dif2"
stand2[stand2 <0] <- "hom2"

data<-rbind(stand1, stand2)

lou<-data$`St. Louis Lac`
lou<-as.data.frame(lou)
lou<-table(lou)
lou<-as.data.frame(lou)
pie(lou$Freq, col=c("gold3","gold", "royalblue4", "royalblue1"), labels = NA)
#labels = c("dif1", "diff2", "hom1", "hom2")

pie<-data$`Rano Aroi`
pie<-as.data.frame(pie)
pie<-table(pie)
pie<-as.data.frame(pie)
pie(pie$Freq, col=c("gold3","gold","royalblue4", "royalblue1"), labels = NA)
#labels = c("dif1", "diff2", "hom1", "hom2"
