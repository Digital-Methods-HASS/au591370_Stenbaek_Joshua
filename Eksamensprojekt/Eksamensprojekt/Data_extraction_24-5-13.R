
#Install and load the required package if not already installed
install.packages('tidyverse')
library(tidyverse)


#Displaying the census data from Aarhus County 1801 and 1860
Aarhus_1801 <- read.csv("data/census_1801_cleaned_up.csv")
View(Aarhus_1801)
Aarhus_1860 <- read.csv("data/census_1860_cleaned_up.csv")
View(Aarhus_1860)


#Removing the first rows to leave only the most important data
Aarhus_1801 <- Aarhus_1801[,-c(3:10)]
Aarhus_1860 <- Aarhus_1860[,-c(4:15)]


#Filtering the relevant terms from the censuses into categories
#The 1801 census
Aarhus_1801_dag_ind <- filter(Aarhus_1801, erhverv == "daglejer" | erhverv == "indsidder") %>% 
  count(erhverv)
Aarhus_1801_fattig <- filter(Aarhus_1801, erhverv == "almisselem") %>% 
  count(erhverv)
Aarhus_1801_van <- filter(Aarhus_1801, erhverv == "vanvittig" | erhverv == "vanfør") %>% 
  count(erhverv)

#The 1860 census
Aarhus_1860_dag_ind <- filter(Aarhus_1860, stilling == "daglejer" | stilling == "indsidder") %>% 
  count(stilling)
Aarhus_1860_fattig <- filter(Aarhus_1860, stilling == "almisselem" | stilling == "fattiglem" | stilling == "understøttes af andre") %>% 
  count(stilling)


#Filtering the relevant categories by location for the 1801 census - split by gender
#Filtering the total number of inhabitants in 1801 - split by gender
Aarhus_1801_mand <- filter(Aarhus_1801, koen == "mand") %>% 
  count(koen)
Aarhus_1801_kvinde <- filter(Aarhus_1801, koen == "kvinde") %>% 
  count(koen)

#Filtering the data about men into categories
Aarhus_1801_mand_dag_ind <- filter(Aarhus_1801, koen == "mand" & erhverv == "daglejer" | koen == "mand" & erhverv == "indsidder") %>% 
  count(erhverv)
Aarhus_1801_mand_fattig <- filter(Aarhus_1801, koen == "mand" & erhverv == "almisselem") %>% 
  count(erhverv)
Aarhus_1801_mand_van <- filter(Aarhus_1801, koen == "mand" & erhverv == "vanvittig" | koen == "mand" & erhverv == "vanfør") %>% 
  count(erhverv)

#Filtering the data about women into categories
Aarhus_1801_kvinde_dag_ind <- filter(Aarhus_1801, koen == "kvinde" & erhverv == "daglejer" | koen == "kvinde" & erhverv == "indsidder") %>% 
  count(erhverv)
Aarhus_1801_kvinde_fattig <- filter(Aarhus_1801, koen == "kvinde" & erhverv == "almisselem") %>% 
  count(erhverv)
Aarhus_1801_kvinde_van <- filter(Aarhus_1801, koen == "kvinde" & erhverv == "vanvittig" | koen == "kvinde" & erhverv == "vanfør") %>% 
  count(erhverv)

#Filtering the relevant categories by location for the 1860 census - split by gender
#Filtering the total number of inhabitants in 1860 - split by gender
Aarhus_1860_mand <- filter(Aarhus_1860, koen == "mand") %>% 
  count(koen)
Aarhus_1860_kvinde <- filter(Aarhus_1860, koen == "kvinde") %>% 
  count(koen)

#Filtering the data about men into categories
Aarhus_1860_mand_dag_ind <- filter(Aarhus_1860, koen == "mand" & stilling == "daglejer" | koen == "mand" & stilling == "indsidder") %>% 
  count(stilling)
Aarhus_1860_mand_fattig <- filter(Aarhus_1860, koen == "mand" & stilling == "almisselem" | koen == "mand" & stilling == "fattiglem" | koen == "mand" & stilling == "understøttes af andre") %>% 
  count(stilling)

#Filtering the data about women into categories
Aarhus_1860_kvinde_dag_ind <- filter(Aarhus_1860, koen == "kvinde" & stilling == "daglejer" | koen == "kvinde" & stilling == "indsidder") %>% 
  count(stilling)
Aarhus_1860_kvinde_fattig <- filter(Aarhus_1860, koen == "kvinde" & stilling == "almisselem" | koen == "kvinde" & stilling == "fattiglem" | koen == "kvinde" & stilling == "understøttes af andre") %>% 
  count(stilling)


#Filtering the relevant terms by location for the 1801 census - split by location
#Filtering the total number of inhabitants for the censuses - split by location
Aarhus_1801_by <- filter(Aarhus_1801, sogn == "Århus Købstad") %>% 
  count(sogn)
Aarhus_1801_land <- filter(Aarhus_1801, sogn != "Århus Købstad") %>% 
  select(sogn) %>% 
  summary(sogn)
Aarhus_1860_by <- filter(Aarhus_1860, sogn == "Århus Købstad") %>% 
  count(sogn)
Aarhus_1860_land <- filter(Aarhus_1860, sogn != "Århus Købstad") %>% 
  select(sogn) %>% 
  summary(sogn)

#Filtering the data for city dwellers
Aarhus_1801_by_dag_ind <- filter(Aarhus_1801, sogn == "Århus Købstad" & erhverv == "daglejer" | sogn == "Århus Købstad" & erhverv == "indsidder") %>% 
  count(erhverv)
Aarhus_1801_by_fattig <- filter(Aarhus_1801, sogn == "Århus Købstad" & erhverv == "almisselem") %>% 
  count(erhverv)
Aarhus_1801_by_van <- filter(Aarhus_1801, sogn == "Århus Købstad" & erhverv == "vanvittig" | sogn == "Århus Købstad" & erhverv == "vanfør") %>% 
  count(erhverv)

#Filtering the data for the countryside
Aarhus_1801_land_dag_ind <- filter(Aarhus_1801, sogn != "Århus Købstad" & erhverv == "daglejer" | sogn != "Århus Købstad" & erhverv == "indsidder") %>% 
  count(erhverv)
Aarhus_1801_land_fattig <- filter(Aarhus_1801, sogn != "Århus Købstad" & erhverv == "almisselem") %>% 
  count(erhverv)
Aarhus_1801_land_van <- filter(Aarhus_1801, sogn != "Århus Købstad" & erhverv == "vanvittig" | sogn != "Århus Købstad" & erhverv == "vanfør") %>% 
  count(erhverv)

#Filtering the relevant terms by location for the 1860 census - split by location
#Filtering the data for city dwellers
Aarhus_1860_by_dag_ind <- filter(Aarhus_1860, sogn == "Århus Købstad" & stilling == "daglejer" | sogn == "Århus Købstad" & stilling == "indsidder") %>% 
  count(stilling)
Aarhus_1860_by_fattig <- filter(Aarhus_1860, sogn == "Århus Købstad" & stilling == "almisselem" | sogn == "Århus Købstad" & stilling == "fattiglem" | sogn == "Århus Købstad" & stilling == "understøttes af andre") %>% 
  count(stilling)

#Filtering the data for the countryside
Aarhus_1860_land_dag_ind <- filter(Aarhus_1860, sogn != "Århus Købstad" & stilling == "daglejer" | sogn != "Århus Købstad" & stilling == "indsidder") %>% 
  count(stilling)
Aarhus_1860_land_fattig <- filter(Aarhus_1860, sogn != "Århus Købstad" & stilling == "almisselem" | sogn != "Århus Købstad" & stilling == "fattiglem" | sogn != "Århus Købstad" & stilling == "understøttes af andre") %>% 
  count(stilling)


#Filtering the relevant terms into categories for the 1801 census - split by age
#Filtering the total number of inhabitants for the 1801 census - split by age
Aarhus_1801_barn <- filter(Aarhus_1801, alder <= 15) %>% 
  select(sogn, alder) %>% 
  summary(alder)
Aarhus_1801_voksen <- filter(Aarhus_1801, alder > 15 & alder < 60) %>% 
  select(sogn, alder) %>% 
  summary(alder)
Aarhus_1801_gammel <- filter(Aarhus_1801, alder >= 60) %>% 
  select(sogn, alder) %>% 
  summary(alder)



#Filtering the total number of inhabitants for the 1860 census - split by age
Aarhus_1860_barn <- filter(Aarhus_1860, alder <= 15) %>% 
  select(sogn, alder) %>% 
  summary(alder)
Aarhus_1860_voksen <- filter(Aarhus_1860, alder > 15 & alder < 60) %>% 
  select(sogn, alder) %>% 
  summary(alder)
Aarhus_1860_gammel <- filter(Aarhus_1860, alder >= 60) %>% 
  select(sogn, alder) %>% 
  summary(alder)

#Filtering the data into categories for the 1801 census - split by age
#Filtering the data for children
Aarhus_1801_barn_dag_ind <- filter(Aarhus_1801, alder <= 15 & erhverv == "daglejer" | alder <= 15 & erhverv == "indsidder") %>% 
  count(erhverv)
Aarhus_1801_barn_fattig <- filter(Aarhus_1801, alder <= 15 & erhverv == "almisselem") %>% 
  count(erhverv)
Aarhus_1801_barn_van <- filter(Aarhus_1801, alder <= 15 & erhverv == "vanvittig" | alder <= 15 & erhverv == "vanfør") %>% 
  count(erhverv)

#Filtering the data for adults
Aarhus_1801_voksen_dag_ind <- filter(Aarhus_1801, alder > 15 & alder < 60 & erhverv == "daglejer" | alder > 15 & alder < 60 & erhverv == "indsidder") %>% 
  count(erhverv)
Aarhus_1801_voksen_fattig <- filter(Aarhus_1801, alder > 15 & alder < 60 & erhverv == "almisselem") %>% 
  count(erhverv)
Aarhus_1801_voksen_van <- filter(Aarhus_1801, alder > 15 & alder < 60 & erhverv == "vanvittig" | alder > 15 & alder < 60 & erhverv == "vanfør") %>% 
  count(erhverv)

#Filtering the data for the elderly
Aarhus_1801_gammel_dag_ind <- filter(Aarhus_1801, alder >= 60 & erhverv == "daglejer" | alder >= 60 & erhverv == "indsidder") %>% 
  count(erhverv)
Aarhus_1801_gammel_fattig <- filter(Aarhus_1801, alder >= 60 & erhverv == "almisselem") %>% 
  count(erhverv)
Aarhus_1801_gammel_van <- filter(Aarhus_1801, alder >= 60 & erhverv == "vanvittig" | alder >= 60 & erhverv == "vanfør") %>% 
  count(erhverv)

#Filtering the relevant terms into categories for the 1860 census - split by age
#Filtering the data for children
Aarhus_1860_barn_dag_ind <- filter(Aarhus_1860, alder <= 15 & stilling == "daglejer" | alder <= 15 & stilling == "indsidder") %>% 
  count(stilling)
Aarhus_1860_barn_fattig <- filter(Aarhus_1860, alder <= 15 & stilling == "almisselem" | alder <= 15 & stilling == "fattiglem" | alder <= 15 & stilling == "understøttes af andre") %>% 
  count(stilling)

#Filtering the data for adults
Aarhus_1860_voksen_dag_ind <- filter(Aarhus_1860, alder > 15 & alder < 60 & stilling == "daglejer" | alder > 15 & alder < 60 & stilling == "indsidder") %>% 
  count(stilling)
Aarhus_1860_voksen_fattig <- filter(Aarhus_1860, alder > 15 & alder < 60 & stilling == "almisselem" | alder > 15 & alder < 60 & stilling == "fattiglem" | alder > 15 & alder < 60 & stilling == "understøttes af andre") %>% 
  count(stilling)

#Filtering the data for the elderly
Aarhus_1860_gammel_dag_ind <- filter(Aarhus_1860, alder >= 60 & stilling == "daglejer" | alder >= 60 & stilling == "indsidder") %>% 
  count(stilling)
Aarhus_1860_gammel_fattig <- filter(Aarhus_1860, alder >= 60 & stilling == "almisselem" | alder >= 60 & stilling == "fattiglem" | alder >= 60 & stilling == "understøttes af andre") %>% 
  count(stilling)


#Displaying the different relevant values
#Displaying the total inhabitants by category for the two censuses
Aarhus_1801_dag_ind
Aarhus_1801_fattig
Aarhus_1801_van

Aarhus_1860_dag_ind
Aarhus_1860_fattig

#Displaying the total inhabitants of each census - split by gender
Aarhus_1801_mand
Aarhus_1801_kvinde

Aarhus_1860_mand
Aarhus_1860_kvinde

#Displaying the number of categorized inhabitants of the 1801 census - split by gender
Aarhus_1801_mand_dag_ind
Aarhus_1801_kvinde_dag_ind

Aarhus_1801_mand_fattig
Aarhus_1801_kvinde_fattig

Aarhus_1801_mand_van
Aarhus_1801_kvinde_van

#Displaying the number of categorized inhabitants of the 1860 census - split by gender
Aarhus_1860_mand_dag_ind
Aarhus_1860_kvinde_dag_ind

Aarhus_1860_mand_fattig
Aarhus_1860_kvinde_fattig

#Displaying the total inhabitants of each census - split by location
Aarhus_1801_by
Aarhus_1801_land

Aarhus_1860_by
Aarhus_1860_land

#Displaying the number of categorized inhabitants of the 1801 census - split by location
Aarhus_1801_by_dag_ind
Aarhus_1801_land_dag_ind

Aarhus_1801_by_fattig
Aarhus_1801_land_fattig

Aarhus_1801_by_van
Aarhus_1801_land_van

#Displaying the number of categorized inhabitants of the 1860 census - split by location
Aarhus_1860_by_dag_ind
Aarhus_1860_land_dag_ind

Aarhus_1860_by_fattig
Aarhus_1860_land_fattig

#Displaying the total inhabitants of each census - split by age
Aarhus_1801_barn
Aarhus_1801_voksen
Aarhus_1801_gammel

Aarhus_1860_barn
Aarhus_1860_voksen
Aarhus_1860_gammel

#Displaying the number of categorized inhabitants of the 1801 census - split by age
Aarhus_1801_barn_dag_ind
Aarhus_1801_voksen_dag_ind
Aarhus_1801_gammel_dag_ind

Aarhus_1801_barn_fattig
Aarhus_1801_voksen_fattig
Aarhus_1801_gammel_fattig

Aarhus_1801_barn_van
Aarhus_1801_voksen_van
Aarhus_1801_gammel_van

#Displaying the number of categorized inhabitants of the 1860 census - split by age
Aarhus_1860_barn_dag_ind
Aarhus_1860_voksen_dag_ind
Aarhus_1860_gammel_dag_ind

Aarhus_1860_barn_fattig
Aarhus_1860_voksen_fattig
Aarhus_1860_gammel_fattig