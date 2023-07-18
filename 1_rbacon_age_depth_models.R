#This script is to make the age-depth models####
#install.packages("rbacon")
library(rbacon)

Bacon(core = "St_Louis_Lac")
Bacon(core = "Plum_Swamp")
Bacon(core = "Volivoli")
#add.dates(mn=3610,sdev=110,depth=50, cc=3, )
Bacon(core = "Yacata")
Bacon(core = "Finemui_dates",slump=c(191,200,256,293))
Bacon("Lotofoa_dates", slump=c(127,177))
Bacon(core = "Ngofe_dates")
Bacon(core = "Avaiovuna_dates")
Bacon(core = "Rano_Aroi")
