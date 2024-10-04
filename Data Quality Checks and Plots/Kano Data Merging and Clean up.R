
# Install and load necessary packages
install.packages("haven")
install.packages("dplyr")
library(haven)
library(dplyr)
# Load required libraries
library(sf)
library(ggplot2)
library(ggrepel)

# Function to define custom ggplot theme
map_theme <- function() {
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        rect = element_blank(),
        plot.background = element_rect(fill = "white", colour = NA),
        plot.title = element_text(hjust = 0.5),
        legend.title.align = 0.5,
        legend.title = element_text(size = 18, colour = 'black'),
        legend.text = element_text(size = 18, colour = 'black'),
        legend.key.height = unit(0.65, "cm"))
}

  # Extract variable names and labels
  variable_names <- names(index)
  variable_labels <- sapply(index, attr, which = "label")
  
  # Create a data dictionary
  data_dictionary <- data.frame(
    Variable = variable_names,
    Label = variable_labels,
    stringsAsFactors = FALSE
  )
  
  # Print the data dictionary
  print(data_dictionary)
  
  # Optionally, save the data dictio
  
}

write.csv(data_dictionary, "data_dictionary.csv", row.names = FALSE)















# Read the .dta file
file_path <- "/Users/macbookpro/Urban Malaria Proj Dropbox/urban_malaria/data/nigeria/kano_ibadan_epi/new_field_data/Kano Wet Season Data Sept. 2024/KN Wet season women survey_edited_290924.dta"  # Replace with the path to your .dta file
datawomenkn <- read_dta(file_path)
datasummarywomenkn <- datawomenkn%>%
  group_by(sn)%>%
summarise(count=n())


# Read the .dta file
file_pathhh <- "/Users/macbookpro/Urban Malaria Proj Dropbox/urban_malaria/data/nigeria/kano_ibadan_epi/new_field_data/Kano Wet Season Data Sept. 2024/KN Wet season household data_edited_290924.dta"  # Replace with the path to your .dta file
datahhkn <- read_dta(file_pathhh)
datasummaryhhkn <- datahhkn%>%
  group_by(sn)%>%
  summarise(count=n())


# Read the .dta file
file_pathmen <- "/Users/macbookpro/Urban Malaria Proj Dropbox/urban_malaria/data/nigeria/kano_ibadan_epi/new_field_data/Kano Wet Season Data Sept. 2024/KN Wet season men survey_edited 290924.dta"  # Replace with the path to your .dta file
datamenkn <- read_dta(file_pathmen)
datasummarymenkn <- datamenkn%>%
  group_by(sn)%>%
  summarise(count=n())


# Read the .dta file
file_pathrdt <- "/Users/macbookpro/Urban Malaria Proj Dropbox/urban_malaria/data/nigeria/kano_ibadan_epi/new_field_data/Kano Wet Season Data Sept. 2024/KN wet season hhold  RDT results_290924.dta"  # Replace with the path to your .dta file
datardtkn <- read_dta(file_pathrdt)
datasummaryrdtkn <- datardtkn%>%
  group_by(sn)%>%
  summarise(count=n())

childrenrdt <- datardtkn %>%
  filter(hl5 <= 10)

positive_children <- childrenrdt %>%
  filter(q302 == 1)



# Read the .dta file
file_pathwm <- "/Users/macbookpro/Urban Malaria Proj Dropbox/urban_malaria/data/nigeria/kano_ibadan_epi/new_field_data/Kano Wet Season Data Sept. 2024/KN Wet season women malaria hx_280924.dta"  # Replace with the path to your .dta file
datawmmalkn <- read_dta(file_pathwm)
datasummarywmmalkn <- datawmmalkn%>%
  group_by(sn)%>%
  summarise(count=n())

# Read the .dta file
file_pathmob <- "/Users/macbookpro/Urban Malaria Proj Dropbox/urban_malaria/data/nigeria/kano_ibadan_epi/new_field_data/Kano Wet Season Data Sept. 2024/KN Wet season women mobility patterns_280924.dta"  # Replace with the path to your .dta file
datamobkn <- read_dta(file_pathmob)
datasummarymobkn <- datamobkn%>%
  group_by(sn)%>%
  summarise(count=n())


datahhkn$Wardn[datahhkn$ward == 1] <- "Zango"
datahhkn$Wardn[datahhkn$ward == 2] <- "Dorayi"
datahhkn$Wardn[datahhkn$ward == 3] <- "Giginyu"
datahhkn$Wardn[datahhkn$ward == 4] <- "Fagge"
datahhkn$Wardn[datahhkn$ward == 5] <- "Gobirawa"



datahhkn$ea_2 <- recode(datahhkn$ea_2 , `1385`=1358)
datahhkn$ea_2 <- recode(datahhkn$ea_2 , `20332`=332)
datahhkn$ea_2 <- recode(datahhkn$ea_2 , `1319`=1390)
datahhkn$ea_2 <- recode(datahhkn$ea_2 , `1492`=1392)
datahhkn$ea_2 <- recode(datahhkn$ea_2 , `11476`=1476)
datahhkn$ea_2 <- recode(datahhkn$ea_2 , `1475`=1476)
datahhkn$ea_2 <- recode(datahhkn$ea_2 , `11482`=1482)

datahhkn$ea_2 <- recode(datahhkn$ea_2 ,`1367`=1376)
datahhkn$ea_2 <- recode(datahhkn$ea_2 ,`1168`=2168)
datahhkn$ea_2 <- recode(datahhkn$ea_2 ,`2112`=2122)
datahhkn$ea_2 <- recode(datahhkn$ea_2 ,`2145`=2154)
datahhkn$ea_2 <- recode(datahhkn$ea_2 ,`1244`=1422)
datahhkn$ea_2 <- recode(datahhkn$ea_2 ,`224`=1422)
datahhkn$ea_2 <- recode(datahhkn$ea_2 ,`225`=1422)
datahhkn$ea_2 <- recode(datahhkn$ea_2 ,`226`=1422)
datahhkn$ea_2 <- recode(datahhkn$ea_2 ,`1`=1422)
datahhkn$ea_2 <- recode(datahhkn$ea_2 ,`129`=1422)
datahhkn$ea_2 <- recode(datahhkn$ea_2 ,`366`=336)
datahhkn$ea_2 <- recode(datahhkn$ea_2 ,`1446`=1456)
datahhkn$ea_2 <- recode(datahhkn$ea_2 ,`1570`=1456)
datahhkn$ea_2 <- recode(datahhkn$ea_2 ,`2409`=2484)
datahhkn$ea_2 <- recode(datahhkn$ea_2 ,`2492213`=2484)
datahhkn$ea_2 <- recode(datahhkn$ea_2 ,`381`=318)
datahhkn$ea_2 <- recode(datahhkn$ea_2 ,`105`=2332)
datahhkn$ea_2 <- recode(datahhkn$ea_2 ,`1475`=1476)
datahhkn$ea_2 <- recode(datahhkn$ea_2 ,`21482`=1482)
datahhkn$ea_2 <- recode(datahhkn$ea_2 ,`2402121`=2402)
datahhkn$ea_2 <- recode(datahhkn$ea_2 ,`2402220`=2402)
datahhkn$ea_2 <- recode(datahhkn$ea_2 ,`2402227`=2402)
datahhkn$ea_2 <- recode(datahhkn$ea_2 ,`102`=2402)
datahhkn$ea_2 <- recode(datahhkn$ea_2 ,`2409`=2402)
datahhkn$ea_2 <- recode(datahhkn$ea_2 ,`440`=2440)
datahhkn$ea_2 <- recode(datahhkn$ea_2 ,`385`=358)
datahhkn$ea_2 <- recode(datahhkn$ea_2 ,`2022`=2220)
datahhkn$ea_2 <- recode(datahhkn$ea_2 ,`1427`=1426)
datahhkn$ea_2 <- recode(datahhkn$ea_2 ,`366`=336)
datahhkn$ea_2 <- recode(datahhkn$ea_2 ,`1346`=1436)
datahhkn$ea_2 <- recode(datahhkn$ea_2 ,`1430`=1438)
datahhkn$ea_2 <- recode(datahhkn$ea_2 ,`1467`=1466)
datahhkn$ea_2 <- recode(datahhkn$ea_2 ,`1486`=1468)
datahhkn$ea_2 <- recode(datahhkn$ea_2 ,`336`=316)
datahhkn$ea_2 <- recode(datahhkn$ea_2 ,`314`=316)
datahhkn$ea_2 <- recode(datahhkn$ea_2 ,`2152`=2154)

datahhkn$ea_1 <- toupper(datahhkn$ea_1)

# Use recode for all the replacements
datahhkn$ea_name_new <- recode(datahhkn$ea_1,
                                 "ZAGO" = "ZANGO",
                                 "ZANGOGURGAMA" = "ZANGO",
                                 "Zongo" = "ZANGO",
                                 "ZONGO" = "ZANGO",
                                 "YANGANDU" = "YANGANDA",
                                 "YANLEWA" = "YANALAWA",
                                 "BABADAWALAYOUT" = "BADAWALAYOUT",
                                 "BADAWA" = "BADAWALAYOUT",
                               "BADAWA LAYOUT" = "BADAWALAYOUT",
                               "FILIN" = "FILINIDI",
                               "FILINGIDI" = "FILINIDI",
                               "FILINIDI" = "FILINIDI",
                                 "BABANGORI" = "BABANGWARI",
                                 "GIDANBABANGWARI" = "BABANGWARI",
                               "GIDANBABANGORI" = "BABANGWARI",
                               
                                 "CHIKAL" = "CHIKALA",
                                 "CHIKALAROAD" = "CHIKALA",
                                 "CHIKALAA" = "CHIKALA",
                                 "CHIKALAAROAD" = "CHIKALA",
                               "CHIKALA356" = "CHIKALA",
                               
                                 "DORAYIYAMADAWA" = "YAMADAWA",
                                
                                 "FORESTER" = "FORESTRY",
                                 "FORRESTER" = "FORESTRY",
                                 "FORRESTREE" = "FORESTRY",
                                 "FORSTRY" = "FORESTRY",
                                 "AFORESTRY" = "FORESTRY",
                                 "GDUKAWAL" = "GDUKAWA",
                               "G.DUKAWA" = "GDUKAWA",
                               "G_DUKAWA" = "GDUKAWA",
                               
                                 "GIGINYUNC" = "GIGINYUC",
                                 "GOBARAWA" = "GOBIRAWA",
                               "DOBIRAWA" = "GOBIRAWA",
                               
                                 "GOBERAWA" = "GOBIRAWA",
                                 "GOBIRAWAKURNA" = "KURNAA",
                               "G_KURNA" = "KURNAA",
                               "G_KURNAA_LAYINAMMASCO" = "KURNAA",
                               "G_KURNAA" = "KURNAA",
                               "G_KURNAASABE" = "KURNAA",
                               
                      
                               
                               
                               
                                 "HAJHAUWA" = "HAUWAMAISAKA",
                                 "HAJHAUWAMAISAKAROAD" = "HAUWAMAISAKA",
                                 "HAJIYAHAUWAMAI" = "HAUWAMAISAKA",
                                 "HAJIYAHAUWAMAISAKA" = "HAUWAMAISAKA",
                                 "HAJIYAHAUWAMAISAKAROAD" = "HAUWAMAISAKA",
                                 "HAJIYAHAUWAMESAKA" = "HAUWAMAISAKA",
                                 "HAJIYAHAUWAMESAQA" = "HAUWAMAISAKA",
                                 "HAJIYAHAUWMESAKA" = "HAUWAMAISAKA",
                                 "HAUWAMAISAKAMAISAKAROAD" = "HAUWAMAISAKA",
                                 "HAUWAMAISAKASAKA" = "HAUWAMAISAKA",
                                 "HAUWAMAISAKASAKAROAD" = "HAUWAMAISAKA",
                                 "HAUWAMAISAKAROAD" = "HAUWAMAISAKA",
                                 "LAYINHAUWAMAISAKA" = "HAUWAMAISAKA",
                                 "LAYINHAJIYAHAUWAMESAKA" = "HAUWAMAISAKA" ,
                                 "JIGAWAJA" = "JAENJIGAWA",
                                 "JIGAWAJAEN" = "JAENJIGAWA",
                                 "JAENJIGAWAEN" = "JAENJIGAWA",
                               "JA_ENJIGAWA" = "JAENJIGAWA",
                               "JIGAWAJA_EN" = "JAENJIGAWA",
                               
                            
                               
                               "KOFARMATA" = "KOFARMATADYEPITS",
                               
                                 "KAFARMATA" = "KOFARMATADYEPITS",
                                 "KAFARMATADYEPITS" = "KOFARMATADYEPITS",
                              
                                 "KASUWAMATA" = "KASUWARMATA",
                                 "KAWOCIKI" = "KAWO",
                                 "KAWOCIKIMAIM" = "KAWO",
                                 "KAWOCIKIN" = "KAWO",
                                 "KAWOCIKINGARI" = "KAWO",
                                 "KAWONCIKI" = "KAWO",
                                 "KAWONMAIGARI" = "KAWO",
                                 "KAWONMEGARI" = "KAWO",
                                 "KAWOMAIGARIMAIM" = "KAWO",
                                 "KAWOMAIGARIN" = "KAWO",
                                 "KAWOMAIGARINGARI" = "KAWO",
                                 "KAWOMAIGARIGARI" = "KAWO",
                               "KAWO CIKI" = "KAWO",
                               "KAWO KUDU" = "KAWO",
                               "KAWO CIKIN" = "KAWO",
                               "KAWO MAIGARI" = "KAWO",
                               "KAWOAREWA" = "KAWO",
                               "KAWOKUDU" = "KAWO",
                               "KAWOMAIGARI" = "KAWO",
                               
                              
                       
                               
                               
                                 "LAYINALHAJIALI" = "LAYINALHAJIALINOCASE",
                               "LAYINALHAJINOCASE318" = "LAYINALHAJIALINOCASE",
          
                                 "ALHAJIALI" = "LAYINALHAJIALINOCASE",
                                 "LAYINALHAJINOCASE" = "LAYINALHAJIALINOCASE",
                                 "LAYINLAYINALHAJIALINOCASENOCASE" = "LAYINALHAJIALINOCASE",
                                 "LAYINLAYINALHAJIALINOCASENOCASENOCASE" = "LAYINALHAJIALINOCASE",
                               "ALJALI ALI" = "LAYINALHAJIALINOCASE",
                               
                                 "LAYINDANBINAI" = "LAYINDABINAI",
                                 "LOKONMAKEA" = "LOKONMAKERA",
                                 "MAILIKKAFA" = "MAILIKAFA",
                                 "MLIKAFA" = "MAILIKAFA",
                               "M_LIKKAFA" = "MAILIKAFA",
                               
                               
                                 "MURTALAMUHAMMADSOCIALISTHOSPITAL" = "MURTALAMUHAMMADSPECIALHOSPITAL",
                               
                                 "MURTALAMUHAMMADSPECIALISTHOSPITAL" = "MURTALAMUHAMMADSPECIALHOSPITAL",
                                 "MURTALAMUHAMMADHOSPITAL" = "MURTALAMUHAMMADSPECIALHOSPITAL",
                                 "MURTALAMHOSPITAL" = "MURTALAMUHAMMADSPECIALHOSPITAL",
                                 "MURTALAMUHAMMAD" = "MURTALAMUHAMMADSPECIALHOSPITAL",
                                 "MURTALA" = "MURTALAMUHAMMADSPECIALHOSPITAL",
                               "MURTALAMUHAMMADSOCIALISTHOSPITAL1" = "MURTALAMUHAMMADSPECIALHOSPITAL",
                               "MURTALAMUHAMMADSOCIALISTHOSPITAL2" = "MURTALAMUHAMMADSPECIALHOSPITAL",
                               "MURTALAMUHAMMADSPECIALISTHOSPITAL2" = "MURTALAMUHAMMADSPECIALHOSPITAL",
                               
                                 "NASSARAWAG" = "NASSARAWAGRA",
                                 "NASSARAWA" = "NASSARAWAGRA",
                                 "NASSARAWAGRAGRA" = "NASSARAWAGRA",
                                 "NASSARAWAGRAGRARA" = "NASSARAWAGRA",
                               "NASSARAWA GRA" = "NASSARAWAGRA",
                               "NASSARAWAG.R.A" = "NASSARAWAGRA",
                               
                          
                               
                               
                                 "SALLARBABBA" = "SALLARI",
                     
                               
                               "MAKERA_SHAGOTARA" = "SHAGOTARA",
                               "MAKERA" = "SHAGOTARA",
                                 "SHIGOTARA" = "SHAGOTARA",
                                 "SHAGO" = "SHAGOTARA",
                                 "SHAGOTARATARA" = "SHAGOTARA",
                               "SHAGOTARA_LAYINMAKERA" = "SHAGOTARA",
                               "G_SHAGOTARA" = "SHAGOTARA",
                               "G_SHAGOTARA_MAKERA" = "SHAGOTARA",
                               "LAYINMAKERA_SHAGOTARA" = "SHAGOTARA",
                               "LAYINMEUNGUWA" = "SHAGOTARA",
                               "LAYINMAKERASHAGOTARA" = "SHAGOTARA",
                       
                               
                               
  
                                 "TRIUMPHPUBLISHED" = "TRIUMPHPUBLISHINGCOMPANY",
                                 "TRIUMPHPUBLISHING" = "TRIUMPHPUBLISHINGCOMPANY",
                                 "TRIUMPPUBLISHING" = "TRIUMPHPUBLISHINGCOMPANY",
                                 "TRUMPH" = "TRIUMPHPUBLISHINGCOMPANY",
                                 "TRIUMPHPUBLISH" = "TRIUMPHPUBLISHINGCOMPANY",
                                 "TRIUMPH" = "TRIUMPHPUBLISHINGCOMPANY",
                               "TRIUMPH PUB" = "TRIUMPHPUBLISHINGCOMPANY",
                                 "TRIUMPHPUBLISHINGCOMPANYPUBLISHINGCOMPANY" = "TRIUMPHPUBLISHINGCOMPANY",
                                 "TUDUNBUJUWA" = "TUDUNBOJUWA",
                               "T_BOJUWA" = "TUDUNBOJUWA",
                               "T_BUJUWA" = "TUDUNBOJUWA",
                               "G_BOJUWA" = "TUDUNBOJUWA",
                               
                               
                                 "UNGUWAWABAI" = "UNGUWARWAMBAI",
                                 "YAMAWADA" = "YAMADAWA",
                                 "YANALEWA" = "YANALAWA",
                               
                               
                               "1450" =	"ZANGO",
                               "1452" =	"ZANGO",
                               "1456"	= "KOFARMATADYEPITS",
                               "1466" =	"KOFARMATADYEPITS",
                               "1476" =	"MURTALAMUHAMMADSPECIALHOSPITAL",
                               "MMSH" =	"MURTALAMUHAMMADSPECIALHOSPITAL",
                               "2402" =	"SHAGOTARA",
                               "2432" =	"GOBIRAWA",
                               "2492" =	"GOBIRAWA",
                               "G_C_GARI" =	"GOBIRAWA",
                               "G_YAMMA" =	"GOBIRAWA",
                               "G_GOBIRAWA"=	"GOBIRAWA",
                               "GOBIRAWAA"=	"GOBIRAWA",
                               "HOTORO"="NASSARAWAGRA",
                               "MALUFAI"="FILINIDI",
                               "YANALAWA"="FILINIDI",
                               "GIDANTURAWA"="FILINIDI",
                               "BALATIREDA"="FILINIDI",
                               "DORAYI"="DORAYIBABBA",
                               "SHAWUCI"="MMSH",
                               "KAMADA"="MMSH",
                             "G_TSAMIYARZUBAU"="GOBIRAWA",
                               "GINGIYU B"="GIGINYUB",
                               "GIGINYUB"="GIGINYUB",
                              
                               "LAYINDABINAI"="KASUWARMATA",
                               "LAYIN DAB"="KASUWARMATA",
                               "LAYIN DAN UJU"="KASUWARMATA",
                               "LAYIN ME UNGUWA"="KASUWARMATA",
                               "KARAMA"="DORAYIKARAMA"
                             
                              
                               
                               
)
datahhkn$ea_name_new[datahhkn$ea_name_new == ""] <- "KASUWARMATA"
datahhkn$ea_name_new[datahhkn$ea_2 == 332] <- "WAPA"
datahhkn$ea_name_new[datahhkn$ea_2 == 322] <- "WAPA"
datahhkn$ea_name_new[datahhkn$ea_2 == 316] <- "KASUWARMATA"
datahhkn$ea_name_new[datahhkn$ea_2 == 2220] <- "TUDUNBOJUWA"
datahhkn$ea_name_new[datahhkn$ea_2 == 2058] <- "GOBIRAWA"
datahhkn$ea_name_new[datahhkn$ea_2 == 2154] <- "GOBIRAWA"
datahhkn$ea_name_new[datahhkn$ea_2 == 2168] <- "GOBIRAWA"
datahhkn$ea_name_new[datahhkn$ea_2 == 2188] <- "GOBIRAWA"


datahhkn$ea_name_new[datahhkn$ea_2 == 2298] <- "GOBIRAWA"



datahhkn$ea_name_new[datahhkn$ea_2 == 2440] <- "SHAGOTARA"
datahhkn$ea_name_new[datahhkn$ea_2 == 2460] <- "GOBIRAWA"
datahhkn$ea_name_new[datahhkn$ea_2 == 3858] <- "GOBIRAWA"

datahhkn$ea_name_new[datahhkn$ea_2 == 2188] <- "KASUWARMATA"
datahhkn$ea_name_new[datahhkn$ea_2 == 2484] <- "KASUWARMATA"
datahhkn$ea_name_new[datahhkn$ea_2 == 2492] <- "KASUWARMATA"
datahhkn$ea_name_new[datahhkn$ea_2 == 2252] <- "GOBIRAWA"

datahhkn$ea_name_new[datahhkn$ea_2 == 1448] <- "FILINIDI"
datahhkn$ea_name_new[datahhkn$ea_2 == 1450] <- "FILINIDI"
datahhkn$ea_name_new[datahhkn$ea_2 == 1458] <- "FILINIDI"
datahhkn$ea_name_new[datahhkn$ea_2 == 1466] <- "ZANGO"
datahhkn$ea_name_new[datahhkn$ea_2 == 1452] <- "ZANGO"
datahhkn$ea_name_new[datahhkn$ea_2 == 1456] <- "KOFARMATADYEPITS"


datahhkn$ea_name_new[datahhkn$ea_2 == 1452] <- "ZANGO"
datahhkn$ea_name_new[datahhkn$ea_2 == 1466] <- "ZANGO"
datahhkn$ea_name_new[datahhkn$ea_2 == 1468] <- "ZANGO"
datahhkn$ea_name_new[datahhkn$ea_2 == 1470] <- "ZANGO"
datahhkn$ea_name_new[datahhkn$ea_2 == 1472] <- "ZANGO"
datahhkn$ea_name_new[datahhkn$ea_2 == 1474] <- "ZANGO"
datahhkn$ea_name_new[datahhkn$ea_2 == 1478] <- "ZANGO"
datahhkn$ea_name_new[datahhkn$ea_2 == 1480] <- "ZANGO"
datahhkn$ea_name_new[datahhkn$ea_2 == 1484] <- "ZANGO"
datahhkn$ea_name_new[datahhkn$ea_2 == 1358] <- "YAMADAWA"
datahhkn$ea_name_new[datahhkn$ea_2 == 1362] <- "YAMADAWA"
datahhkn$ea_name_new[datahhkn$ea_2 == 1384] <- "YAMADAWA"
datahhkn$ea_name_new[datahhkn$ea_2 == 1396] <- "YAMADAWA"
datahhkn$ea_name_new[datahhkn$ea_2 == 1408] <- "YAMADAWA"
datahhkn$ea_name_new[datahhkn$ea_2 == 1410] <- "YAMADAWA"
datahhkn$ea_name_new[datahhkn$ea_2 == 1412] <- "YAMADAWA"
datahhkn$ea_name_new[datahhkn$ea_2 == 46] <- "BADAWALAYOUT"
datahhkn$ea_name_new[datahhkn$ea_2 == 47] <- "BADAWALAYOUT"
datahhkn$ea_name_new[datahhkn$ea_2 == 48] <- "BADAWALAYOUT"
datahhkn$ea_name_new[datahhkn$ea_2 == 49] <- "BADAWALAYOUT"
datahhkn$ea_name_new[datahhkn$ea_2 == 50] <- "BADAWALAYOUT"
datahhkn$ea_name_new[datahhkn$ea_2 == 51] <- "BADAWALAYOUT"
datahhkn$ea_name_new[datahhkn$ea_2 == 52] <- "BADAWALAYOUT"
datahhkn$ea_name_new[datahhkn$ea_2 == 53] <- "BADAWALAYOUT"
datahhkn$ea_name_new[datahhkn$ea_2 == 54] <- "BADAWALAYOUT"
datahhkn$ea_name_new[datahhkn$ea_2 == 55] <- "BADAWALAYOUT"
datahhkn$ea_name_new[datahhkn$ea_2 == 2244] <- "BURHANA"
datahhkn$ea_name_new[datahhkn$ea_2 == 356] <- "CHIKALAROAD"
datahhkn$ea_name_new[datahhkn$ea_2 == 1366] <- "DORAYIBABBA"
datahhkn$ea_name_new[datahhkn$ea_2 == 1376] <- "DORAYIBABBA"
datahhkn$ea_name_new[datahhkn$ea_2 == 1378] <- "DORAYIBABBA"
datahhkn$ea_name_new[datahhkn$ea_2 == 1380] <- "DORAYIBABBA"
datahhkn$ea_name_new[datahhkn$ea_2 == 1382] <- "DORAYIBABBA"
datahhkn$ea_name_new[datahhkn$ea_2 == 1386] <- "DORAYIBABBA"
datahhkn$ea_name_new[datahhkn$ea_2 == 1398] <- "DORAYIKARAMA"
datahhkn$ea_name_new[datahhkn$ea_2 == 1400] <- "DORAYIKARAMA"
datahhkn$ea_name_new[datahhkn$ea_2 == 1402] <- "DORAYIKARAMA"
datahhkn$ea_name_new[datahhkn$ea_2 == 1404] <- "DORAYIKARAMA"
datahhkn$ea_name_new[datahhkn$ea_2 == 2166] <- "FILINDURUMI"
datahhkn$ea_name_new[datahhkn$ea_2 == 1448] <- "FILINIDI"
datahhkn$ea_name_new[datahhkn$ea_2 == 1450] <- "FILINIDI"
datahhkn$ea_name_new[datahhkn$ea_2 == 1458] <- "FILINIDI"
datahhkn$ea_name_new[datahhkn$ea_2 == 1390] <- "FORESTRY"
datahhkn$ea_name_new[datahhkn$ea_2 == 1392] <- "FORESTRY"
datahhkn$ea_name_new[datahhkn$ea_2 == 1394] <- "FORESTRY"
datahhkn$ea_name_new[datahhkn$ea_2 == 1420] <- "FORESTRY"
datahhkn$ea_name_new[datahhkn$ea_2 == 2058] <- "G/CIKINGARI"
datahhkn$ea_name_new[datahhkn$ea_2 == 2168] <- "G/CIKINGARI"
datahhkn$ea_name_new[datahhkn$ea_2 == 2216] <- "G/CIKINGARI"
datahhkn$ea_name_new[datahhkn$ea_2 == 2220] <- "G/CIKINGARI"
datahhkn$ea_name_new[datahhkn$ea_2 == 2222] <- "G/CIKINGARI"
datahhkn$ea_name_new[datahhkn$ea_2 == 2252] <- "G/CIKINGARI"
datahhkn$ea_name_new[datahhkn$ea_2 == 2324] <- "G/DUKAWA"
datahhkn$ea_name_new[datahhkn$ea_2 == 2332] <- "G/DUKAWA"
datahhkn$ea_name_new[datahhkn$ea_2 == 2188] <- "G/KURNA"
datahhkn$ea_name_new[datahhkn$ea_2 == 2492] <- "G/KURNAA"
datahhkn$ea_name_new[datahhkn$ea_2 == 2402] <- "G/SHAGOTARA"
datahhkn$ea_name_new[datahhkn$ea_2 == 2424] <- "G/SHAGOTARA"
datahhkn$ea_name_new[datahhkn$ea_2 == 2432] <- "G/SHAGOTARA"
datahhkn$ea_name_new[datahhkn$ea_2 == 2440] <- "G/SHAGOTARA"
datahhkn$ea_name_new[datahhkn$ea_2 == 2152] <- "G/UNGUWARHASSAN"
datahhkn$ea_name_new[datahhkn$ea_2 == 2276] <- "G/YAMMA"
datahhkn$ea_name_new[datahhkn$ea_2 == 2102] <- "GOBIRAWA"
datahhkn$ea_name_new[datahhkn$ea_2 == 2116] <- "GOBIRAWA"
datahhkn$ea_name_new[datahhkn$ea_2 == 2122] <- "GOBIRAWA"
datahhkn$ea_name_new[datahhkn$ea_2 == 2154] <- "GOBIRAWA"
datahhkn$ea_name_new[datahhkn$ea_2 == 2032] <- "GOBIRAWAA"
datahhkn$ea_name_new[datahhkn$ea_2 == 2374] <- "GOBIRAWAA"
datahhkn$ea_name_new[datahhkn$ea_2 == 2382] <- "GOBIRAWAA"
datahhkn$ea_name_new[datahhkn$ea_2 == 2460] <- "GOBIRAWAA"
datahhkn$ea_name_new[datahhkn$ea_2 == 2180] <- "GOBIRAWAB"
datahhkn$ea_name_new[datahhkn$ea_2 == 2198] <- "GOBIRAWAB"
datahhkn$ea_name_new[datahhkn$ea_2 == 2282] <- "GOBIRAWAB"
datahhkn$ea_name_new[datahhkn$ea_2 == 2298] <- "GOBIRAWAB"
datahhkn$ea_name_new[datahhkn$ea_2 == 346] <- "HAJHAUWAMAISAKAROAD"
datahhkn$ea_name_new[datahhkn$ea_2 == 29] <- "HOTORO"
datahhkn$ea_name_new[datahhkn$ea_2 == 1422] <- "JAENJIGAWA"
datahhkn$ea_name_new[datahhkn$ea_2 == 316] <- "KASUWARMATA"
datahhkn$ea_name_new[datahhkn$ea_2 == 336] <- "KASUWARMATA"
datahhkn$ea_name_new[datahhkn$ea_2 == 340] <- "KASUWARMATA"
datahhkn$ea_name_new[datahhkn$ea_2 == 1456] <- "KOFARMATADYEPITS"
datahhkn$ea_name_new[datahhkn$ea_2 == 2484] <- "KURNAA"
datahhkn$ea_name_new[datahhkn$ea_2 == 1476] <- "MURTALAMUHAMMADSPECIALISTHOSPITAL1"
datahhkn$ea_name_new[datahhkn$ea_2 == 1482] <- "MURTALAMUHAMMADSPECIALISTHOSPITAL2"
datahhkn$ea_name_new[datahhkn$ea_2 == 348] <- "TRIUMPHPUBLISHINGCOMPANY"
datahhkn$ea_name_new[datahhkn$ea_2 == 358] <- "TRIUMPHPUBLISHINGCOMPANY"
datahhkn$ea_name_new[datahhkn$ea_2 == 1416] <- "UNGUWARJAKADA"
datahhkn$ea_name_new[datahhkn$ea_2 == 1418] <- "UNGUWARJAKADA"
datahhkn$ea_name_new[datahhkn$ea_2 == 1426] <- "UNGUWARJAKADA"
datahhkn$ea_name_new[datahhkn$ea_2 == 1432] <- "UNGUWARJAKADA"
datahhkn$ea_name_new[datahhkn$ea_2 == 1436] <- "UNGUWARWAMBAI"
datahhkn$ea_name_new[datahhkn$ea_2 == 1438] <- "UNGUWARWAMBAI"


datahhkn$ea_name_new[datahhkn$ea_2 == 318] <- "LAYINALHAJIALINOCASE"
uniqueeas <- data.frame(unique(datahhkn$ea_name_new) )
uniqueno <- data.frame(unique(datahhkn$ea_2) )
datahhkn$ea_cluster <- paste(datahhkn$ea_name_new,"/",datahhkn$ea_2)

datahhkn$ea_cluster <- gsub(" ","", datahhkn$ea_cluster)



uniqueeaclust <- data.frame(unique(datahhkn$ea_cluster) )


write_dta(datahhkn,"/Users/macbookpro/Urban Malaria Proj Dropbox/urban_malaria/data/nigeria/kano_ibadan_epi/new_field_data/Kano Wet Season Data Sept. 2024/Kano_data_with_EA_names_cleaned_to_125.dta")

write_csv(datahhkn,"/Users/macbookpro/Urban Malaria Proj Dropbox/urban_malaria/data/nigeria/kano_ibadan_epi/new_field_data/Kano Wet Season Data Sept. 2024/Kano_data_with_EA_names_cleaned_to_125.csv")

library(openxlsx)

file_pathlist <- "/Users/macbookpro/Urban Malaria Proj Dropbox/urban_malaria/data/nigeria/kano_ibadan_epi/new_field_data/Kano Wet Season Data Sept. 2024/New EA Clusters found in Data.xlsx"  # Replace with the path to your .dta file
invalidclustnum <- read.xlsx(file_pathlist)

clustnumberinv <- left_join(invalidclustnum, datahhkn, by = c("Cluster_No"="ea_2"))

write_csv(clustnumberinv,"/Users/macbookpro/Urban Malaria Proj Dropbox/urban_malaria/data/nigeria/kano_ibadan_epi/new_field_data/Kano Wet Season Data Sept. 2024/New_cluster_numbers_in_hh_data.csv")

missinghhkn <- anti_join(datahhkn,datawomenkn, by ="sn") 





missingmenkn <- anti_join(datahhkn,datamenkn, by ="sn") 

#missingmenknn <- missingmenkn %>%
 # filter(!hl4 == 2)

missingrdtkn <- anti_join(datahhkn,datardtkn, by ="sn") 

missingwmmalkn <- anti_join(datahhkn,datawmmalkn, by ="sn") 

missingmobkn <- anti_join(datahhkn,datamobkn, by ="sn") 

write_csv(missinghhkn,"/Users/macbookpro/Urban Malaria Proj Dropbox/urban_malaria/data/nigeria/kano_ibadan_epi/new_field_data/Kano Wet Season Data Sept. 2024/missing_women_records_overall_from_hh_data.csv")
write_csv(missingmenkn,"/Users/macbookpro/Urban Malaria Proj Dropbox/urban_malaria/data/nigeria/kano_ibadan_epi/new_field_data/Kano Wet Season Data Sept. 2024/missing_men_records_overall_from_hh_data.csv")

#write_csv(missingmenknn,"/Users/macbookpro/Urban Malaria Proj Dropbox/urban_malaria/data/nigeria/kano_ibadan_epi/new_field_data/Kano Wet Season Data Sept. 2024/missing_men_records_hl4_is_male_from_hh_data.csv")

write_csv(missingrdtkn,"/Users/macbookpro/Urban Malaria Proj Dropbox/urban_malaria/data/nigeria/kano_ibadan_epi/new_field_data/Kano Wet Season Data Sept. 2024/missing_rdt_records_from_hh_data.csv")

write_csv(missingmobkn,"/Users/macbookpro/Urban Malaria Proj Dropbox/urban_malaria/data/nigeria/kano_ibadan_epi/new_field_data/Kano Wet Season Data Sept. 2024/missing_mobility_records_from_hh_data.csv")

write_csv(missingwmmalkn,"/Users/macbookpro/Urban Malaria Proj Dropbox/urban_malaria/data/nigeria/kano_ibadan_epi/new_field_data/Kano Wet Season Data Sept. 2024/missing_women malaria_records_from_hh_data.csv")




data$sn <- as.numeric(data$sn)

wide_data <- pivot_wider(data, names_from = sn, values_from = Value)

wide_data <- pivot_wider(
  datardt,
  names_from = 4,
  values_from = c(2:19)  # Spread both Value1 and Value2 columns
)%>%
  select(-2:-25)

write_dta(wide_data,"data_dictionary.dta")


write_csv(wide_data,"wide_rdt_data.csv")

write_csv(missinghhib,"missing_women_records_from_hh_data.csv")


# Read the .dta file
file_pathrdtw <- "/Users/macbookpro/Downloads/wide_data_rdt.dta"  # Replace with the path to your .dta file
datardtw <- read_dta(file_pathrdtw)

# Extract variable names and labels
variable_names <- names(wide_data)
variable_labels <- sapply(wide_data, attr, which = "label")

# Create a data dictionary
data_dictionary <- data.frame(
  Variable <- variable_names,
  Label <- variable_labels,
  stringsAsFactors = FALSE
)



# Create a data dictionary
data_dictionary <- data.frame(variable_names)

label <- data.frame(variable_labels)

# Print the data dictionary
print(data_dictionary)

# Optionally, save the data dictionary to a CSV file
write.csv(data_dictionary, "data_dictionary.csv", row.names = FALSE)






# Read the .dta file
file_path <- "/Users/macbookpro/Downloads/KN Wet season women malaria hx.dta"  # Replace with the path to your .dta file
data <- read_dta(file_path)

# Extract variable names and labels
variable_names <- names(datahh)
variable_labels <- sapply(datahh, attr, which = "label")

# Create a data dictionary
data_dictionary <- data.frame(
  Variable = variable_names,
  Label = variable_labels,
  stringsAsFactors = FALSE
)

# Print the data dictionary
print(data_dictionary)

# Optionally, save the data dictionary to a CSV file
write.csv(data_dictionary, "maldata_dictionary.csv", row.names = FALSE)




# Extract variable names
variable_names <- names(wide_data)

# Extract labels, replacing NULL with an empty string
variable_labels <- sapply(wide_data, function(x) attr(x, "label"))
variable_labels[is.null(variable_labels)] <- ""  # Handle NULL by replacing with empty string

# Alternatively, to avoid the error, ensure all elements are of length 1
variable_labels <- sapply(wide_data, function(x) {
  lbl <- attr(x, "label")
  if (is.null(lbl)) "" else lbl
})

# Create a data dictionary
data_dictionary <- data.frame(
  Variable = variable_names,
  Label = variable_labels,
  stringsAsFactors = FALSE
)

# View the data dictionary
print(data_dictionary)

write.csv(data_dictionary, "rdt_data_dictionary.csv", row.names = FALSE)






#data dictionary for missing hh data

# Extract variable names
variable_names <- names(datahh)

# Extract labels, replacing NULL with an empty string
variable_labels <- sapply(datahh, function(x) attr(x, "label"))
variable_labels[is.null(variable_labels)] <- ""  # Handle NULL by replacing with empty string

# Alternatively, to avoid the error, ensure all elements are of length 1
variable_labels <- sapply(datahh, function(x) {
  lbl <- attr(x, "label")
  if (is.null(lbl)) "" else lbl
})

# Create a data dictionary
data_dictionary <- data.frame(
  Variable = variable_names,
  #Label = variable_labels,
  stringsAsFactors = FALSE
)

# Create a data dictionary
data_dictionary_ <- data.frame(
  #Variable = variable_names,
  Label = variable_labels,
  stringsAsFactors = FALSE
)

# View the data dictionary
print(variable_labels)

write.csv(variable_labels, "data_dictionary_missing_hh.csv", row.names = FALSE)



labelling_missing_data <- merged_data %>%
  mutate(across(123:324, as.character)) %>%  # Convert all to character
  rowwise() %>%  # Operate row by row
  mutate(
    across(
      123:324, 
      ~ ifelse(all(is.na(c_across(123:324))), "No Women Data Available", .)
    )
  ) %>%
  ungroup()

as.character(merged_data[, 123:326])
na_rows <- apply(merged_data[, 123:326], 1, function(row) all(is.na(row)))

# Replace NAs with "No Women Data Available" in the identified rows
merged_data[na_rows, 123:326] <- "No Women Data Available"

library(dplyr)

# Rename columns in each dataframe with a prefix based on the dataframe name
datahh <- datahh %>% rename_with(~ paste0("hh_data_", .), -sn)
data <- data %>% rename_with(~ paste0("women_data_", .), -sn)
datamen <- datamen %>% rename_with(~ paste0("men_data_", .), -sn)
datardtw <- datardtw %>% rename_with(~ paste0("rdt_wide_data_", .), -sn)

# Perform the joins
merged_data <- datahh %>%
  left_join(data, by = "sn") %>%
  left_join(datamen, by = "sn") %>%
  left_join(datardtw, by = "sn")

na_rows <- apply(merged_data[, 123:324], 1, function(row) all(is.na(row)))

# Replace NAs with "Missing Data" in the identified rows
merged_data[na_rows, 123:126] <- "Missing Data in Women Questionnaire"


na_rows_men <- apply(merged_data[, 325:629], 1, function(row) all(is.na(row)))

# Replace NAs with "Missing Data" in the identified rows
merged_data[na_rows_men, 325:328] <- "Missing Data in Men Questionnaire"

na_rows_rdt <- apply(merged_data[, 630:821], 1, function(row) all(is.na(row)))

# Replace NAs with "Missing Data" in the identified rows
merged_data[na_rows_rdt, 642:653] <- "Missing Data in RDT Questionnaire"

write.csv(merged_data, "Merged_data_women_men_rdt_Ibadan.csv", row.names = FALSE)



# Extract variable names and labels
variable_names <- names(merged_data)
variable_labels <- sapply(merged_data, attr, which = "label")



# Create a data dictionary
data_dictionary <- data.frame(
 Variable = variable_names,
  Label = variable_labels,
  stringsAsFactors = FALSE
)




# Print the data dictionary
print(variable_labels)

# Optionally, save the data dictionary to a CSV file
write.csv(variable_labels, "maldata_dictionary.csv", row.names = FALSE)





# Temporarily change the number of rows displayed
options(max.print = 300)

# Print the data frame
print(variable_labels)

# Optionally, reset to default
options(max.print = 1000)  # You can set this to a value that suits your usual needs




datardtmismatch <- datardt %>%
  filter(!redcap_repeat_instance == hl1)

write.csv(datardtmismatch, "RDT_SN_LN_MISMATCH.csv", row.names = FALSE)



 selecteddffemale<- merged_data %>%
  select(1,146,150,158,159,316,318,642:653,666:677,690:701,750:761, 774:785, 798:809)

 selecteddfmale <- merged_data %>%
  select(1, 348,352,360,361,621,623, 642:653,666:677,690:701,750:761,  774:785, 798:809)



dbs_code_rdt <- datardt %>%
  tidyr::separate(newdbs_code , into = c("newsn", "ln"), sep = "/") 


femaledata <- selecteddffemale %>%
  tidyr::separate(women_data_q704 , into = c("newsn", "ln"), sep = "/") %>%
  tidyr::separate(rdt_wide_data_newdbs_code_2 , into = c("newsnrdt", "lnrdt"), sep = "/") 

male_data <- selecteddfmale  %>%
  tidyr::separate(men_data_q704 , into = c("newsn", "ln"), sep = "/") %>%
  tidyr::separate(rdt_wide_data_newdbs_code_1 , into = c("newsnrdt", "lnrdt"), sep = "/") 



not_matching_f_dbsln_ln <- femaledata%>%
  filter(!lnrdt == women_data_ln)%>%
  select(1:8,10,22,34,47,71)

not_matching_f_ln <- femaledata%>%
  filter(!ln == women_data_ln)


not_matching_f_rdt <- femaledata%>%
  filter(!women_data_q702 == rdt_wide_data_q302_2)%>%
  select(1:8,10,22,34,47,71)

female_age_mismatch <- not_matching_f_rdt %>%
  filter(!women_data_q201b == rdt_wide_data_hl6_2)


female_age_match <- not_matching_f_rdt %>%
  filter(women_data_q201b == rdt_wide_data_hl6_2)


notmatchingfemales <- not_matching_f_dbsln_ln %>%
select(,-10:-20 )%>%
  select(, -12:-22, -24:-34 ) %>%
  select(, -14:-24 ) 


not_matching_m_dbsln_ln <- male_data%>%
  filter(!rdt_wide_data_q300i_1 == ln)%>%
  select(,-10:-20 )%>%
  select(, -11:-21, -23:-33 ) %>%
  select(, -13:-23 ) 
  


  
  not_matching_m__dbsln_ln <- male_data%>%
  filter(!men_data_ln == ln)%>%
  select(,-10:-20 )%>%
  select(, -11:-21, -23:-33 ) %>%
  select(, -13:-23 ) 



  
  not_matching_m_rdt <- male_data%>%
    filter(!men_data_q702 == rdt_wide_data_q302_1)%>%
    select(,-10:-20 )%>%
    select(, -11:-21, -23:-33 ) %>%
    select(, -13:-23 )

  
  dobmismatch_rdt <- not_matching_m_rdt %>%
    filter(!men_data_q201b == rdt_wide_data_hl6_1)
  
  dobmatch_rdt <- not_matching_m_rdt %>%
    filter(men_data_q201b == rdt_wide_data_hl6_1)
  
  
  not_matching_m_dbsln_ln <- male_data%>%
    filter(!rdt_wide_data_q300i_1 == ln)%>%
    select(,-10:-20 )%>%
    select(, -11:-21, -23:-33 ) %>%
    select(, -13:-23 ) 
  
  
  
  
  not_matching_m__dbsln_ln <- male_data%>%
    filter(!men_data_ln == ln)%>%
    select(,-10:-20 )%>%
    select(, -11:-21, -23:-33 ) %>%
    select(, -13:-23 ) 
  
  
  
  
  not_matching_m_rdt <- male_data%>%
    filter(!men_data_q702 == rdt_wide_data_q302_1)%>%
    select(,-10:-20 )%>%
    select(, -11:-21, -23:-33 ) %>%
    select(, -13:-23 )
  
  
  dobmismatch_rdt <- not_matching_m_rdt %>%
    filter(!men_data_q201b == rdt_wide_data_hl6_1)
  
  
  plot(merged_data$men_data_q201b)
  
  plot(merged_data$women_data_q201b)
  
  library(openxlsx)
  
  # List of dataframes with sheet names
  df_list <- list("Not Matching LN- Females" = not_matching_f_dbsln_ln, "Not Matching RDTs -Female" = not_matching_f_rdt, "Date of Birth Mismatch - Female" = female_age_mismatch, "Date of Birth Match - Female" = female_age_match, "Not Matching LN- Males" = not_matching_m_dbsln_ln, "Not Matching RDTs -Male" = not_matching_m_rdt, "Date of Birth Mismatch - Male" = dobmismatch_rdt, "Date of Birth Match - Male" = dobmatch_rdt)
  
  # Create a workbook
  wb <- createWorkbook()
  
  # Add dataframes to workbook sheets
  for (sheet_name in names(df_list)) {
    addWorksheet(wb, sheet_name)
    writeData(wb, sheet_name, df_list[[sheet_name]])
  }
  
  # Save the workbook
  saveWorkbook(wb, "Ibadan QA_sheets.xlsx", overwrite = TRUE)
  
  
  

view(not_matching_m_rdt)




# Check for missing values across all columns---
missing_valuesrdt <- data.frame(colSums(is.na(datardtkn)))
View(missing_valuesrdt)



# Check for missing values across all columns---
missing_values_w <- data.frame(colSums(is.na(datawomenkn)))
View(missing_values_w)


# Check for missing values across all columns---
missing_values_m <- data.frame(colSums(is.na(datamenkn)))
View(missing_values_m)

# Check for missing values across all columns---
missing_values_mob <- data.frame(colSums(is.na(datamobkn)))
View(missing_values_mob)

# Check for missing values across all columns---
missing_values_wmmal <- data.frame(colSums(is.na(datawmmalkn)))
View(missing_values_mob)


write_csv(missing_valuesrdt,"/Users/macbookpro/Urban Malaria Proj Dropbox/urban_malaria/data/nigeria/kano_ibadan_epi/new_field_data/Kano Wet Season Data Sept. 2024/Summary of missingness_rdt_records.csv")

write_csv(missing_values_m,"/Users/macbookpro/Urban Malaria Proj Dropbox/urban_malaria/data/nigeria/kano_ibadan_epi/new_field_data/Kano Wet Season Data Sept. 2024/Summary of missingness_men_records.csv")

write_csv(missing_values_w,"/Users/macbookpro/Urban Malaria Proj Dropbox/urban_malaria/data/nigeria/kano_ibadan_epi/new_field_data/Kano Wet Season Data Sept. 2024/Summary of missingness_women_records.csv")

write_csv(missing_values_mob,"/Users/macbookpro/Urban Malaria Proj Dropbox/urban_malaria/data/nigeria/kano_ibadan_epi/new_field_data/Kano Wet Season Data Sept. 2024/Summary of missingness_women_mobility_records.csv")

write_csv(missing_values_wmmal,"/Users/macbookpro/Urban Malaria Proj Dropbox/urban_malaria/data/nigeria/kano_ibadan_epi/new_field_data/Kano Wet Season Data Sept. 2024/Summary of missingness_women_malaria_records.csv")

write_csv(invalid,"/Users/macbookpro/Urban Malaria Proj Dropbox/urban_malaria/data/nigeria/kano_ibadan_epi/new_field_data/Kano Wet Season Data Sept. 2024/GPS Records outside of Approved_records.csv")




proportion_missing_values <- missing_values / nrow(df1)
View(proportion_missing_values)






library(randomForest)

# Fit Random Forest model
rf_model <- randomForest(malaria ~ ., data = merged_data, importance = TRUE)

# Variable importance
importance(rf_model)



#define the map theme function
map_theme <- function(){
  theme(axis.text.x = ggplot2::element_blank(),
        axis.text.y = ggplot2::element_blank(),
        axis.ticks = ggplot2::element_blank(),
        rect = ggplot2::element_blank(),
        plot.background = ggplot2::element_rect(fill = "white", colour = NA),
        plot.title = element_text(hjust = 0.5),
        legend.title.align=0.5,
        legend.title=element_text(size=10, colour = 'black'),
        legend.text =element_text(size = 10, colour = 'black'),
        legend.key.height = unit(0.65, "cm"))
}

######gps checks  



shape <- st_read(dsn = "/Users/macbookpro/Downloads/Kano_metro_ward_sixLGAs/", layer = "Kano_metro_ward_sixLGAs")
View(shape)

dff <- datahhkn%>%
  filter(!is.na(bi7_long) & !is.na(bi7_lat))
#remove other wards from the shape file
shp <- shape %>%
  filter(WardName %in% c("Giginyu", "Dorayi","Fagge D2", "Gobirawa", "Zango"))


giginyu <- shp %>%
  filter(WardName=="Giginyu")

fagge <- shp %>%
  filter(WardName=="Fagge D2")

dorayi <- shp %>%
  filter(WardName=="Dorayi")

zango <- shp %>%
  filter(WardName=="Zango")

gobirawa <- shp %>%
  filter(WardName=="Gobirawa")


#define the coordinates
gps <- sf::st_as_sf(dff, coords=c("bi7_long", "bi7_lat"), crs=4326)
# Perform spatial intersection
st_crs(gps) <- 4326
st_crs(shp) <- 4326
intersects_a1 <- st_intersection(gps, shp)



#plot
vld <- ggplot(shp)+
  geom_point(data = intersects_a1,  aes(geometry = geometry, color=settle_type), size=0.5, stat= "sf_coordinates")+
  scale_color_manual(values = c(formal = "#00A08A", informal = "#F2A6A2" , slum = "#923159"))+
  # geom_sf_text(data=all,aes(label = WardName , geometry=geometry) ) +
  #geom_sf_text(data=intersects_a1, aes(label = ea_names , geometry=geometry),size=1 ) +
  geom_sf(fill = NA) +
  guides(size = FALSE)+
  map_theme()+
  theme_manuscript()+
  ylab("")+
  xlab("")+
  labs(title = "Cross Sectional Survey Geo-points of Households Surveyed",
       subtitle = "GPS Position of HHs surveyed in Kano by Settlement type",
       caption = "Data source : Cross Sectional Survey, Kano"
  )+
  coord_sf()


invalid <- anti_join(datahhkn, intersects_a1, by = "sn")
gpsi <- sf::st_as_sf(invalid, coords=c("bi7_long", "bi7_lat"), crs=4326)
# Perform spatial intersection
st_crs(gpsi) <- 4326
st_crs(shp) <- 4326
intersects_invalid <- st_intersection(gpsi, shape)


giginyu <- shp %>%
  filter(WardName=="Giginyu")

fagge <- shp %>%
  filter(WardName=="Fagge D2")

dorayi <- shp %>%
  filter(WardName=="Dorayi")

zango <- shp %>%
  filter(WardName=="Zango")

gobirawa <- shp %>%
  filter(WardName=="Gobirawa")



giginyui <- intersects_invalid %>%
  filter(Wardn=="Giginyu")

faggei <- intersects_invalid %>%
  filter(Wardn=="Fagge")

dorayii <- intersects_invalid %>%
  filter(Wardn=="Dorayi")

zangoi <- intersects_invalid %>%
  filter(Wardn=="Zango")

gobirawai <- intersects_invalid %>%
  filter(Wardn=="Gobirawa")



#plot
inv <- ggplot(shp)+
  geom_point(data = intersects_invalid,  aes(geometry = geometry, color=settle_type), size=0.5, stat= "sf_coordinates")+
  scale_color_manual(values = c(formal = "#00A08A", informal = "#F2A6A2" , slum = "#923159"))+
   #geom_sf_text(data=intersects_a1, aes(label = ea_names , geometry=geometry),size=1 ) +
  geom_sf(fill = NA) +
  geom_sf_text(data=shape,aes(label = WardName , geometry=geometry), size=2 ) +
  guides(size = FALSE)+
  map_theme()+
  theme_manuscript()+
  ylab("")+
  xlab("")+
  labs(title = "Geo points of Households interviewed outside of sampled wards",
       subtitle = "GPS Position of HHs surveyed in Kano by distance",
       caption = "Data source : Cross Sectional Survey, Kano"
  )+
  coord_sf()



distancefagge <- st_distance(faggei, fagge)

distancefaggedf <- data.frame(distancefagge)

Merged <- cbind(faggei,distancefaggedf)

Merged$distancefaggedf <- gsub("\\[m\\]", "", Merged$distancefagge)

# Convert the column to numeric, if necessary
Merged$distancefagge <- as.numeric(Merged$distancefagge)

#Merged$distancefagge <- Merged$distancefagge/1000

Merged$distancefagge <- ceiling(Merged$distancefagge * 10) / 10
write_csv(Merged,"/Users/macbookpro/Urban Malaria Proj Dropbox/urban_malaria/data/nigeria/kano_ibadan_epi/new_field_data/Kano Wet Season Data Sept. 2024/Distance between Invalid HHs in Fagge and Fagge.csv")


library(ggrepel)
Merged <- Merged %>%
  mutate(X = st_coordinates(geometry)[,1],
         Y = st_coordinates(geometry)[,2])
#plot
fge <- ggplot(shape)+
  geom_sf(data= shape, aes(fill = WardName), colour="lightblue", alpha=0.2) +
  scale_fill_manual(values = c("Fagge D2" = "skyblue"))+
  geom_point(data = Merged,  aes(geometry = geometry, color=distancefagge, size=distancefagge),alpha=0.9, stat= "sf_coordinates")+
    scale_color_gradientn(colors = c("#00A08A", "#ffdd00", "red4"), 
                       values = c(0, 0.5, 1)) +
  #geom_sf_text(data=intersects_a1, aes(label = ea_names , geometry=geometry),size=1 ) +
  geom_text_repel(data = Merged, aes(x=X, y=Y, label = paste(sn), geometry = geometry), size = 2, max.overlaps = 20) +
  guides(size = FALSE)+
  map_theme()+
  theme_manuscript()+
  ylab("")+
  xlab("")+
  labs(title = "Geo points of Households interviewed outside of sampled wards",
       subtitle = "GPS Position of HHs surveyed in Kano by distance",
       caption = "Data source : Cross Sectional Survey, Kano"
  )+
  coord_sf()






distancedorayi <- st_distance(dorayii, dorayi)

distancedorayidf <- data.frame(distancedorayi)

Mergeddorayi <- cbind(dorayii,distancedorayidf)

Mergeddorayi$distancedorayidf <- gsub("\\[m\\]", "", Mergeddorayi$distancedorayi)

# Convert the column to numeric, if necessary
Mergeddorayi$distancedorayi <- as.numeric(Mergeddorayi$distancedorayi)

#Mergeddorayi$distancedorayi <- Mergeddorayi$distancedorayi/1000

Mergeddorayi$distancedorayi <- ceiling(Mergeddorayi$distancedorayi * 10) / 10
write_csv(Mergeddorayi,"/Users/macbookpro/Urban Malaria Proj Dropbox/urban_malaria/data/nigeria/kano_ibadan_epi/new_field_data/Kano Wet Season Data Sept. 2024/Distance between Invalid HHs in Dorayi and Dorayi.csv")


library(ggrepel)
Mergeddorayi <- Mergeddorayi %>%
  mutate(X = st_coordinates(geometry)[,1],
         Y = st_coordinates(geometry)[,2])
#plot
dry <- ggplot(shape)+
  geom_sf(data= shape, aes(fill = WardName), colour="lightblue", alpha=0.2) +
  scale_fill_manual(values = c("Dorayi" = "skyblue"))+
  geom_point(data = Mergeddorayi,  aes(geometry = geometry, color=distancedorayi, size=distancedorayi),alpha=0.9, stat= "sf_coordinates")+
  scale_color_gradientn(colors = c("#00A08A", "#ffdd00", "red4"), 
                        values = c(0, 0.5, 1)) +
  #geom_sf_text(data=intersects_a1, aes(label = ea_names , geometry=geometry),size=1 ) +
  geom_text_repel(data = Mergeddorayi, aes(x=X, y=Y, label = paste(sn), geometry = geometry), size = 2, max.overlaps = 20) +
  guides(size = FALSE)+
  map_theme()+
  theme_manuscript()+
  ylab("")+
  xlab("")+
  labs(title = "Geo points of Households interviewed outside of sampled wards",
       subtitle = "GPS Position of HHs surveyed in Kano by distance",
       caption = "Data source : Cross Sectional Survey, Kano"
  )+
  coord_sf()



distancegiginyu <- st_distance(giginyui, giginyu)

distancegiginyudf <- data.frame(distancegiginyu)

Mergedgiginyu <- cbind(giginyui,distancegiginyudf)

Mergedgiginyu$distancegiginyudf <- gsub("\\[m\\]", "", Mergedgiginyu$distancegiginyu)

# Convert the column to numeric, if necessary
Mergedgiginyu$distancegiginyu <- as.numeric(Mergedgiginyu$distancegiginyu)

#Mergedgiginyu$distancegiginyu <- Mergedgiginyu$distancegiginyu/1000

Mergedgiginyu$distancegiginyu <- ceiling(Mergedgiginyu$distancegiginyu * 10) / 10
write_csv(Mergedgiginyu,"/Users/macbookpro/Urban Malaria Proj Dropbox/urban_malaria/data/nigeria/kano_ibadan_epi/new_field_data/Kano Wet Season Data Sept. 2024/Distance between Invalid HHs in Giginyu and Giginyu.csv")


library(ggrepel)
Mergedgiginyu <- Mergedgiginyu %>%
  mutate(X = st_coordinates(geometry)[,1],
         Y = st_coordinates(geometry)[,2])
#plot
ggy <- ggplot(shape)+
  geom_sf(data= shape, aes(fill = WardName), colour="lightblue", alpha=0.2) +
  scale_fill_manual(values = c("Giginyu" = "skyblue"))+
  geom_point(data = Mergedgiginyu,  aes(geometry = geometry, color=distancegiginyu, size=distancegiginyu),alpha=0.9, stat= "sf_coordinates")+
  scale_color_gradientn(colors = c("#00A08A", "#ffdd00", "red4"), 
                        values = c(0, 0.5, 1)) +
  #geom_sf_text(data=intersects_a1, aes(label = ea_names , geometry=geometry),size=1 ) +
  geom_text_repel(data = Mergedgiginyu, aes(x=X, y=Y, label = paste(sn), geometry = geometry), size = 2, max.overlaps = 20) +
  guides(size = FALSE)+
  map_theme()+
  theme_manuscript()+
  ylab("")+
  xlab("")+
  labs(title = "Geo points of Households interviewed outside of sampled wards",
       subtitle = "GPS Position of HHs surveyed in Kano by distance",
       caption = "Data source : Cross Sectional Survey, Kano"
  )+
  coord_sf()


distancegobirawa <- st_distance(gobirawai, gobirawa)

distancegobirawadf <- data.frame(distancegobirawa)

Mergedgobirawa <- cbind(gobirawai,distancegobirawadf)

Mergedgobirawa$distancegobirawadf <- gsub("\\[m\\]", "", Mergedgobirawa$distancegobirawa)

# Convert the column to numeric, if necessary
Mergedgobirawa$distancegobirawa <- as.numeric(Mergedgobirawa$distancegobirawa)

#Mergedgobirawa$distancegobirawa <- Mergedgobirawa$distancegobirawa/1000

Mergedgobirawa$distancegobirawa <- ceiling(Mergedgobirawa$distancegobirawa * 10) / 10
write_csv(Mergedgobirawa,"/Users/macbookpro/Urban Malaria Proj Dropbox/urban_malaria/data/nigeria/kano_ibadan_epi/new_field_data/Kano Wet Season Data Sept. 2024/Distance between Invalid HHs in Gobirawa and Gobirawa.csv")


library(ggrepel)
Mergedgobirawa <- Mergedgobirawa %>%
  mutate(X = st_coordinates(geometry)[,1],
         Y = st_coordinates(geometry)[,2])
#plot
gbr <- ggplot(shape)+
  geom_sf(data= shape, aes(fill = WardName), colour="lightblue", alpha=0.2) +
  scale_fill_manual(values = c("Gobirawa" = "skyblue"))+
  geom_point(data = Mergedgobirawa,  aes(geometry = geometry, color=distancegobirawa, size=distancegobirawa),alpha=0.9, stat= "sf_coordinates")+
  scale_color_gradientn(colors = c("#00A08A", "#ffdd00", "red4"), 
                        values = c(0, 0.5, 1)) +
  #geom_sf_text(data=intersects_a1, aes(label = ea_names , geometry=geometry),size=1 ) +
  geom_text_repel(data = Mergedgobirawa, aes(x=X, y=Y, label = paste(sn), geometry = geometry), size = 2, max.overlaps = 20) +
  guides(size = FALSE)+
  map_theme()+
  theme_manuscript()+
  ylab("")+
  xlab("")+
  labs(title = "Geo points of Households interviewed outside of sampled wards",
       subtitle = "GPS Position of HHs surveyed in Kano by distance",
       caption = "Data source : Cross Sectional Survey, Kano"
  )+
  coord_sf()


distancezango <- st_distance(zangoi, zango)

distancezangodf <- data.frame(distancezango)

Mergedzango <- cbind(zangoi,distancezangodf)

Mergedzango$distancezangodf <- gsub("\\[m\\]", "", Mergedzango$distancezango)

# Convert the column to numeric, if necessary
Mergedzango$distancezango <- as.numeric(Mergedzango$distancezango)

#Mergedzango$distancezango <- Mergedzango$distancezango/1000

Mergedzango$distancezango <- ceiling(Mergedzango$distancezango * 10) / 10
write_csv(Mergedzango,"/Users/macbookpro/Urban Malaria Proj Dropbox/urban_malaria/data/nigeria/kano_ibadan_epi/new_field_data/Kano Wet Season Data Sept. 2024/Distance between Invalid HHs in Zango and Zango.csv")


library(ggrepel)
Mergedzango <- Mergedzango %>%
  mutate(X = st_coordinates(geometry)[,1],
         Y = st_coordinates(geometry)[,2])
#plot
zng <- ggplot(shape)+
  geom_sf(data= shape, aes(fill = WardName), colour="lightblue", alpha=0.2) +
  scale_fill_manual(values = c("Zango" = "skyblue"))+
  geom_point(data = Mergedzango,  aes(geometry = geometry, color=distancezango, size=distancezango),alpha=0.9, stat= "sf_coordinates")+
  scale_color_gradientn(colors = c("#00A08A", "#ffdd00", "red4"), 
                        values = c(0, 0.5, 1)) +
  #geom_sf_text(data=intersects_a1, aes(label = ea_names , geometry=geometry),size=1 ) +
  geom_text_repel(data = Mergedzango, aes(x=X, y=Y, label = paste(sn), geometry = geometry), size = 2, max.overlaps = 20) +
  guides(size = FALSE)+
  map_theme()+
  theme_manuscript()+
  ylab("")+
  xlab("")+
  labs(title = "Geo points of Households interviewed outside of sampled wards",
       subtitle = "GPS Position of HHs surveyed in Kano by distance",
       caption = "Data source : Cross Sectional Survey, Kano"
  )+
  coord_sf()

ggsave("/Users/macbookpro/Urban Malaria Proj Dropbox/urban_malaria/data/nigeria/kano_ibadan_epi/new_field_data/Kano Wet Season Data Sept. 2024/Valid_GPS_Points.png", plot = vld, width = 20, height = 15, units = "cm", dpi = 300)

ggsave("/Users/macbookpro/Urban Malaria Proj Dropbox/urban_malaria/data/nigeria/kano_ibadan_epi/new_field_data/Kano Wet Season Data Sept. 2024/Invalid_gps.png", plot = inv, width = 20, height = 15, units = "cm", dpi = 300)

ggsave("/Users/macbookpro/Urban Malaria Proj Dropbox/urban_malaria/data/nigeria/kano_ibadan_epi/new_field_data/Kano Wet Season Data Sept. 2024/Zango_gps.png", plot = zng, width = 20, height = 15, units = "cm", dpi = 300)

ggsave("/Users/macbookpro/Urban Malaria Proj Dropbox/urban_malaria/data/nigeria/kano_ibadan_epi/new_field_data/Kano Wet Season Data Sept. 2024/fagge_gps.png", plot = fge, width = 20, height = 15, units = "cm", dpi = 300)

ggsave("/Users/macbookpro/Urban Malaria Proj Dropbox/urban_malaria/data/nigeria/kano_ibadan_epi/new_field_data/Kano Wet Season Data Sept. 2024/dorayi_gps.png", plot = dry, width = 20, height = 15, units = "cm", dpi = 300)

ggsave("/Users/macbookpro/Urban Malaria Proj Dropbox/urban_malaria/data/nigeria/kano_ibadan_epi/new_field_data/Kano Wet Season Data Sept. 2024/Giginyu_gps.png", plot = ggy, width = 20, height = 15, units = "cm", dpi = 300)

ggsave("/Users/macbookpro/Urban Malaria Proj Dropbox/urban_malaria/data/nigeria/kano_ibadan_epi/new_field_data/Kano Wet Season Data Sept. 2024/gobirawa_gps.png", plot = gbr, width = 20, height = 15, units = "cm", dpi = 300)



