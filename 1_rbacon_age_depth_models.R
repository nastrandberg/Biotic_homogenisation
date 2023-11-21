#Generate the age-depth models####
#install.packages("rbacon")
library(rbacon)

Bacon(core = "St_Louis_Lac",  title="St. Louis Lac")
Bacon(core = "Plum_Swamp", title="Plum Swamp")
Bacon(core = "Volivoli", title="Volivoli")
add.dates(mn=3610,sdev=110,depth=50, cc=3, )
Bacon(core = "Yacata", title="Yacata")
Bacon(core = "Finemui_dates",slump=c(191,200,256,293), title="Finemui Swamp")
Bacon("Lotofoa_dates", slump=c(127,177), title="Lotofoa Swamp")
Bacon(core = "Ngofe_dates", title="Ngofe Marsh")
Bacon(core = "Avaiovuna_dates", title="Avai'o'vuna Swamp")
Bacon(core = "Rano_Aroi", title="Rano Aroi")
