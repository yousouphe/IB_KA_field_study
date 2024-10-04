library(haven)
library(tidyverse)
library(readxl)
library(dplyr)
library(ggplot2)
library(patchwork)
library(sf)
library(ggplot2)
library(ggmap)
library(plotly)




kanohhdata<-read_dta('/Users/macbookpro/Urban Malaria Proj Dropbox/urban_malaria/data/nigeria/kano_ibadan_epi/new_field_data/last_upload_Akinyemi/Kano Wet Household questionnaire/KN wet season household members RDT results.dta')

kanohhdata$enum_clust <- gsub(",", "", kanohhdata$bi5)
kanohhdata$enum_clust <- toupper(kanohhdata$enum_clust)
kanohhdata$enum_clust_1 <- gsub(" ", "", kanohhdata$enum_clust)
kanohhdata$enum_clust_3 <- gsub("/", "", kanohhdata$enum_clust)

kanohhdata$enum_clust_4 <- gsub("\\.", "", kanohhdata$enum_clust_3)

kanohhdata$enum_clust_5 <- gsub(" ", "", kanohhdata$enum_clust_4)

kanohhdata$enum_clust_6 <- gsub("_N", "", kanohhdata$enum_clust_5)
# 1. Extract the text part
kanohhdata$ea_name <- str_extract(kanohhdata$enum_clust_6, "[A-Za-z]+")

# 2. Extract the numeric part
kanohhdata$cluster_no <- str_extract(kanohhdata$enum_clust_6, "[0-9]+")






kanohhdata$cluster_no <- gsub("^0+", "", kanohhdata$cluster_no)



# Use recode for all the replacements
kanohhdata$ea_name_new <- recode(kanohhdata$ea_name,
                             "ZAGO" = "ZANGO",
                             "ZANGOGURGAMA" = "ZANGO",
                             "Zongo" = "ZANGO",
                             "ZONGO" = "ZANGO",
                             "YANGANDU" = "YANGANDA",
                             "YANLEWA" = "YANALAWA",
                             "BABADAWALAYOUT" = "BADAWALAYOUT",
                             "BADAWA" = "BADAWALAYOUT",
                             "BABANGORI" = "BABANGWARI",
                             "GIDANBABANGWARI" = "BABANGWARI",
                             "CHIKAL" = "CHIKALA",
                             "CHIKALAROAD" = "CHIKALA",
                             "CHIKALAA" = "CHIKALA",
                             "CHIKALAAROAD" = "CHIKALA",
                             "DORAYIYAMADAWA" = "YAMADAWA",
                             "FILINGIDI" = "FILINIDI",
                             "FORESTER" = "FORESTRY",
                             "FORRESTER" = "FORESTRY",
                             "FORRESTREE" = "FORESTRY",
                             "FORSTRY" = "FORESTRY",
                             "AFORESTRY" = "FORESTRY",
                             "GDUKAWAL" = "GDUKAWA",
                             "GIGINYUNC" = "GIGINYUC",
                             "GOBARAWA" = "GOBIRAWA",
                             "GOBERAWA" = "GOBIRAWA",
                             "GOBIRAWAKURNA" = "KURNAA",
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
                             "KAFARMATA" = "KOFARMATA",
                             "KAFARMATADYEPITS" = "KOFARMATA",
                             "KOFARMATADYEPITS" = "KOFARMATA",
                             "KASUWAMATA" = "KASUWARMATA",
                             "KAWOCIKI" = "KAWOMAIGARI",
                             "KAWOCIKIMAIM" = "KAWOMAIGARI",
                             "KAWOCIKIN" = "KAWOMAIGARI",
                             "KAWOCIKINGARI" = "KAWOMAIGARI",
                             "KAWONCIKI" = "KAWOMAIGARI",
                             "KAWONMAIGARI" = "KAWOMAIGARI",
                             "KAWONMEGARI" = "KAWOMAIGARI",
                             "KAWOMAIGARIMAIM" = "KAWOMAIGARI",
                             "KAWOMAIGARIN" = "KAWOMAIGARI",
                             "KAWOMAIGARINGARI" = "KAWOMAIGARI",
                             "KAWOMAIGARIGARI" = "KAWOMAIGARI",
                             "LAYINALHAJIALI" = "LAYINALHAJIALINOCASE",
                             "ALHAJIALI" = "LAYINALHAJIALINOCASE",
                             "LAYINALHAJINOCASE" = "LAYINALHAJIALINOCASE",
                             "LAYINLAYINALHAJIALINOCASENOCASE" = "LAYINALHAJIALINOCASE",
                             "LAYINLAYINALHAJIALINOCASENOCASENOCASE" = "LAYINALHAJIALINOCASE",
                             "LAYINDANBINAI" = "LAYINDABINAI",
                             "LOKONMAKEA" = "LOKONMAKERA",
                             "MAILIKKAFA" = "MAILIKAFA",
                             "MLIKAFA" = "MAILIKAFA",
                             "MURTALAMUHAMMADSOCIALISTHOSPITAL" = "MMSH",
                             "MURTALAMUHAMMADSPECIALHOSPITAL" = "MMSH",
                             "MURTALAMUHAMMADSPECIALISTHOSPITAL" = "MMSH",
                             "MURTALAMUHAMMADHOSPITAL" = "MMSH",
                             "MURTALAMHOSPITAL" = "MMSH",
                             "MURTALAMUHAMMAD" = "MMSH",
                             "MURTALA" = "MMSH",
                             "NASSARAWAG" = "NASSARAWAGRA",
                             "NASSARAWA" = "NASSARAWAGRA",
                             "NASSARAWAGRAGRA" = "NASSARAWAGRA",
                             "NASSARAWAGRAGRARA" = "NASSARAWAGRA",
                             "SALLARBABBA" = "SALLARI",
                             "SHIGOTARA" = "SHAGOTARA",
                             "SHAGO" = "SHAGOTARA",
                             "SHAGOTARATARA" = "SHAGOTARA",
                             "TRIUMPHPUBLISHED" = "TRIUMPHPUBLISHINGCOMPANY",
                             "TRIUMPHPUBLISHING" = "TRIUMPHPUBLISHINGCOMPANY",
                             "TRIUMPPUBLISHING" = "TRIUMPHPUBLISHINGCOMPANY",
                             "TRUMPH" = "TRIUMPHPUBLISHINGCOMPANY",
                             "TRIUMPHPUBLISH" = "TRIUMPHPUBLISHINGCOMPANY",
                             "TRIUMPH" = "TRIUMPHPUBLISHINGCOMPANY",
                             "TRIUMPHPUBLISHINGCOMPANYPUBLISHINGCOMPANY" = "TRIUMPHPUBLISHINGCOMPANY",
                             "TUDUNBUJUWA" = "TUDUNBOJUWA",
                             "UNGUWAWABAI" = "UNGUWARWAMBAI",
                             "YAMAWADA" = "YAMADAWA",
                             "YANALEWA" = "YANALAWA"
)



kanohhdata <- kanohhdata %>%
  arrange(ea_name_new, cluster_no)
dt <- data.frame(unique(kanohhdata$ea_name_new))
kanohhdata$ea_cluster <- paste(kanohhdata$ea_name_new, kanohhdata$cluster_no, sep = "_")



kanohhdata$ea_cluster <- recode(kanohhdata$ea_cluster , "BADAWALAYOUT_59"="BADAWALAYOUT_48")
kanohhdata$cluster_no <- recode(kanohhdata$cluster_no , "59"="48")

kanohhdata$ea_cluster <- recode(kanohhdata$ea_cluster , "BADAWALAYOUT_"="BADAWALAYOUT_47")

kanohhdata$ea_cluster <- recode(kanohhdata$ea_cluster , "BADAWALAYOUT_6158"="BADAWALAYOUT_47")

kanohhdata$ea_cluster <- recode(kanohhdata$ea_cluster , "BADAWALAYOUT_37"="BADAWALAYOUT_47")

kanohhdata$ea_cluster <- recode(kanohhdata$ea_cluster , "BABANGWARI_NA"="BABANGWARI_354")
#kanohhdata$cluster_no <- recode(kanohhdata$cluster_no , "NA"="354")


kanohhdata$ea_cluster[kanohhdata$bi6 == 511 & kanohhdata$ward == 3] <- "BADAWALAYOUT_47"
kanohhdata$ea_cluster[kanohhdata$bi6 == 512 & kanohhdata$ward == 3] <- "BADAWALAYOUT_49"



dteaclust <- kanohhdata %>%
  group_by(ea_cluster)%>%
  summarise(
    total=n()
  )


# 2. Extract the numeric part
kanohhdata$cluster_no_ <- str_extract(kanohhdata$ea_cluster, "[0-9]+")
kanohhdata$cluster_no_ <- as.double(kanohhdata$cluster_no_)

kanohhdata$cluster_no_[kanohhdata$bi6 == 104 & kanohhdata$bi5 == "YAMADAWA"] <- 1396



kanohhdata$cluster_no_ <- recode(kanohhdata$cluster_no_ , `1385`=1358)
kanohhdata$cluster_no_ <- recode(kanohhdata$cluster_no_ , `20332`=332)
kanohhdata$cluster_no_ <- recode(kanohhdata$cluster_no_ , `1319`=1390)
kanohhdata$cluster_no_ <- recode(kanohhdata$cluster_no_ , `1492`=1392)
kanohhdata$cluster_no_ <- recode(kanohhdata$cluster_no_ , `11476`=1476)
kanohhdata$cluster_no_ <- recode(kanohhdata$cluster_no_ , `1475`=1476)
kanohhdata$cluster_no_ <- recode(kanohhdata$cluster_no_ , `11482`=1482)

kanohhdata$cluster_no_ <- recode(kanohhdata$cluster_no_ ,`1367`=1376)
kanohhdata$cluster_no_ <- recode(kanohhdata$cluster_no_ ,`1168`=2168)
kanohhdata$cluster_no_ <- recode(kanohhdata$cluster_no_ ,`2112`=2122)
kanohhdata$cluster_no_ <- recode(kanohhdata$cluster_no_ ,`2145`=2154)
kanohhdata$cluster_no_ <- recode(kanohhdata$cluster_no_ ,`1244`=1422)
kanohhdata$cluster_no_ <- recode(kanohhdata$cluster_no_ ,`224`=1422)
kanohhdata$cluster_no_ <- recode(kanohhdata$cluster_no_ ,`225`=1422)
kanohhdata$cluster_no_ <- recode(kanohhdata$cluster_no_ ,`226`=1422)
kanohhdata$cluster_no_ <- recode(kanohhdata$cluster_no_ ,`1`=1422)
kanohhdata$cluster_no_ <- recode(kanohhdata$cluster_no_ ,`129`=1422)
kanohhdata$cluster_no_ <- recode(kanohhdata$cluster_no_ ,`366`=336)
kanohhdata$cluster_no_ <- recode(kanohhdata$cluster_no_ ,`1446`=1456)
kanohhdata$cluster_no_ <- recode(kanohhdata$cluster_no_ ,`1570`=1456)
kanohhdata$cluster_no_ <- recode(kanohhdata$cluster_no_ ,`2409`=2484)
kanohhdata$cluster_no_ <- recode(kanohhdata$cluster_no_ ,`2492213`=2484)
kanohhdata$cluster_no_ <- recode(kanohhdata$cluster_no_ ,`381`=318)
kanohhdata$cluster_no_ <- recode(kanohhdata$cluster_no_ ,`105`=2332)
kanohhdata$cluster_no_ <- recode(kanohhdata$cluster_no_ ,`1475`=1476)
kanohhdata$cluster_no_ <- recode(kanohhdata$cluster_no_ ,`21482`=1482)
kanohhdata$cluster_no_ <- recode(kanohhdata$cluster_no_ ,`2402121`=2402)
kanohhdata$cluster_no_ <- recode(kanohhdata$cluster_no_ ,`2402220`=2402)
kanohhdata$cluster_no_ <- recode(kanohhdata$cluster_no_ ,`2402227`=2402)
kanohhdata$cluster_no_ <- recode(kanohhdata$cluster_no_ ,`102`=2402)
kanohhdata$cluster_no_ <- recode(kanohhdata$cluster_no_ ,`2409`=2402)
kanohhdata$cluster_no_ <- recode(kanohhdata$cluster_no_ ,`440`=2440)
kanohhdata$cluster_no_ <- recode(kanohhdata$cluster_no_ ,`385`=358)
kanohhdata$cluster_no_ <- recode(kanohhdata$cluster_no_ ,`2022`=2220)
kanohhdata$cluster_no_ <- recode(kanohhdata$cluster_no_ ,`1427`=1426)
kanohhdata$cluster_no_ <- recode(kanohhdata$cluster_no_ ,`366`=336)
kanohhdata$cluster_no_ <- recode(kanohhdata$cluster_no_ ,`1346`=1436)
kanohhdata$cluster_no_ <- recode(kanohhdata$cluster_no_ ,`1430`=1438)
kanohhdata$cluster_no_ <- recode(kanohhdata$cluster_no_ ,`1467`=1466)
kanohhdata$cluster_no_ <- recode(kanohhdata$cluster_no_ ,`1486`=1468)


sampled<-read_xlsx('/Users/macbookpro/Urban Malaria Proj Dropbox/urban_malaria/data/nigeria/kano_ibadan_epi/Sampled HHs/KN Sampled HHs_2024.xlsx')



samp <- sampled %>%
  select(3:7)%>%
  distinct()
samp$easerialnumber <- recode(samp$easerialnumber , `-356`=356)

#samp$easerialnumber <- as.double(samp$easerialnumber)

newdata <- left_join(kanohhdata,samp, by = c("cluster_no_" = "easerialnumber"))


dteaclustnew <- newdata %>%
  group_by(cluster_no_, enumerationarea)%>%
  summarise(
    total=n()
  )

missings <- newdata %>%
filter(is.na(enumerationarea))%>%
filter(!is.na(cluster_no_))

write.csv(missings,"Missings.csv")

ibrdt<-read_dta('/Users/macbookpro/Downloads/IB wet household qnaire/IB Wet season household members RDT.dta')%>%
  filter(!is.na(q302), !(q302 ==3) )

knrdt<-read_dta('/Users/macbookpro/Downloads/Kano Wet Household questionnaire/KN wet season household members RDT results.dta')%>%
  filter(!is.na(q302), !(q302 ==3) )

knrdt$q302 <- ifelse(knrdt$q302 == 2, 0, knrdt$q302)

ibrdt$q302 <- ifelse(ibrdt$q302 == 2, 0, ibrdt$q302)

overall_prevalencekn <- knrdt %>%
  summarise(prevalence = mean(q302)) %>%
  pull(prevalence)

overall_prevalenceib <- ibrdt %>%
  summarise(prevalence = mean(q302)) %>%
  pull(prevalence)


ward_prevalencekn <- knrdt %>%
  group_by(ward) %>%
  summarise(prevalence = mean(q302),
            total_cases = sum(q302),
            population = n())

print(ward_prevalencekn)


ibhh<-read_dta('/Users/macbookpro/Downloads/IB wet household qnaire/IB Wet season household data_edited.dta')

bginfoib <- datahh %>%
  select(1,4,6,10,11:17,117:120)

ib_mergedrdt_hh <- left_join(ibrdt,bginfoib, by='sn')


ward_prevalenceib <- ib_mergedrdt_hh %>%
  group_by(hh_data_bi2) %>%
  summarise(prevalence = mean(q302),
            total_cases = sum(q302),
            population = n())


print(ward_prevalenceib)

cat("Kano Overall Malaria Estimate: Mean - ",overall_prevalencekn)

cat("Ibadan Overall Malaria Estimate: Mean - ",overall_prevalenceib)




# Plot for Prevalence by City
ggplot(ward_prevalenceib, aes(x = hh_data_bi2, y = prevalence)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  theme_minimal() +
  labs(title = "Malaria Prevalence by City", y = "Prevalence", x = "City")



ward_prevalencekns <- knrdt %>%
  group_by(ward,bi3, bi7_long, bi7_lat) %>%
  summarise(prevalence = mean(q302),
            total_cases = sum(q302),
            population = n())
ward_prevalencekns <- ward_prevalencekns %>%
  filter(is.double(bi7_long), is.double(bi7_lat))

malaria_data_sf <- st_as_sf(ward_prevalencekns, coords = c("bi7_long", "bi7_lat"), crs = 4326)





#Starting again

ibadanhhweighted <- read_dta('/Users/macbookpro/Urban Malaria Proj Dropbox/urban_malaria/data/nigeria/kano_ibadan_epi/new_field_data/240922_Ibadan_latest_data/wetseason_household_members_with_weights.dta')

resultibdweighted <- ibadanhhweighted %>%
  filter(!is.na(q302) )

install.packages("survey")
library(survey)
library(dplyr)
library(tidyverse)

survey_design <- svydesign(
  id = ~sn,  # Household identifier (assuming `sn` is unique for households)
  strata = ~ward + enumaration_area + settlement_type_new,  # Strata variables
  weights = ~overall_hh_weight,  # Weight variable
  data = resultibdweighted
)

resultibdweighted<- resultibdweighted%>%
  filter(!q302 == 3) 

resultibdweighted$q302[resultibdweighted$q302 == 2] <- 0

malaria_prevalence <- svymean(~q302, survey_design)
print(malaria_prevalence)


# Use svyby to calculate prevalence at EA by settlement type
malaria_prevalence_by_ea_settlement <- svyby(
  ~q302, 
  ~enumaration_area + settlement_type_new,   # Group by ward and settlement type
  survey_design, 
  svymean
)

# Print the result
print(malaria_prevalence_by_ea_settlement)





# Step 1: Calculate malaria prevalence at EA by settlement type
malaria_prevalence_by_ea_settlement <- svyby(
  ~q302, 
  ~enumaration_area + settlement_type_new,   # Group by enumeration area and settlement type
  survey_design, 
  svymean
)

# Step 2: Min-max normalization (scale from 0 to 1)
# Calculate the min and max prevalence
min_prevalence <- min(malaria_prevalence_by_ea_settlement$q302)
max_prevalence <- max(malaria_prevalence_by_ea_settlement$q302)

# Apply min-max normalization
malaria_prevalence_by_ea_settlement$normalized_prevalence <- 
  (malaria_prevalence_by_ea_settlement$q302 - min_prevalence) / (max_prevalence - min_prevalence)

# Step 3: Arrange the results in ascending order based on normalized prevalence
malaria_prevalence_by_ea_settlement_sorted <- malaria_prevalence_by_ea_settlement %>%
  arrange(normalized_prevalence)

# Print the normalized and sorted prevalence result
print(malaria_prevalence_by_ea_settlement_sorted)






# Use svyby to calculate prevalence at ward level by settlement type
malaria_prevalence_by_ward_settlement <- svyby(
  ~q302, 
  ~ward + settlement_type_new,   # Group by ward and settlement type
  survey_design, 
  svymean
)

# Print the result
print(malaria_prevalence_by_ward_settlement)
