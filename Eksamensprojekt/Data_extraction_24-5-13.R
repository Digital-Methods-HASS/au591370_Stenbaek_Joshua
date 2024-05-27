
#Install and load the required package if not already installed
install.packages('tidyverse')
library(tidyverse)

#Displaying the census data from Aarhus County 1801 and 1860
Aarhus_1801 <- read.csv("data/census_1801_cleaned_up.csv")
View(Aarhus_1801)
Aarhus_1860 <- read.csv("data/census_1860_cleaned_up.csv")
View(Aarhus_1860)
  
#Removing the first rows to leave only the most important data
Aarhus_1801 <- Aarhus_1801[,-c(1:10)]
Aarhus_1860 <- Aarhus_1860[,-c(1:15)]

#Total number om each gender in 1801
Aarhus_1801_mand <- filter(Aarhus_1801, koen == "mand") %>% 
  count(koen)
Aarhus_1801_kvinde <- filter(Aarhus_1801, koen == "kvinde") %>% 
  count(koen)
Aarhus_1801_mand
Aarhus_1801_kvinde

#Filtering the relevant terms by gender for the 1801 census
#First filtering the data for the men
Aarhus_1801_mand_dag_ind <- filter(Aarhus_1801, koen == "mand" & erhverv == "daglejer" | erhverv == "indsidder") %>% 
  count(erhverv)
Aarhus_1801_mand_fattig <- filter(Aarhus_1801, koen == "mand" & erhverv == "almisselem") %>% 
  count(erhverv)
Aarhus_1801_mand_van <- filter(Aarhus_1801, koen == "mand" & erhverv == "vanvittig" | erhverv == "vanfør") %>% 
  count(erhverv)

#Filtering the data for the women
Aarhus_1801_kvinde_dag_ind <- filter(Aarhus_1801, koen == "kvinde" & erhverv == "daglejer" | erhverv == "indsidder") %>% 
  count(erhverv)
Aarhus_1801_kvinde_fattig <- filter(Aarhus_1801, koen == "kvinde" & erhverv == "almisselem") %>% 
  count(erhverv)
Aarhus_1801_kvinde_van <- filter(Aarhus_1801, koen == "kvinde" & erhverv == "vanvittig" | erhverv == "vanfør") %>% 
  count(erhverv)

#Displaying the number in each category
Aarhus_1801_mand_dag_ind
Aarhus_1801_mand_fattig
Aarhus_1801_mand_van
Aarhus_1801_kvinde_dag_ind
Aarhus_1801_kvinde_fattig
Aarhus_1801_kvinde_van

#Total number om each gender in 1860
Aarhus_1860_mand <- filter(Aarhus_1860, koen == "mand") %>% 
  count(koen)
Aarhus_1860_kvinde <- filter(Aarhus_1860, koen == "kvinde") %>% 
  count(koen)
Aarhus_1860_mand
Aarhus_1860_kvinde

#Filtering the relevant terms by gender for the 1860 census
#First filtering the data for the men
Aarhus_1860_mand_dag_ind <- filter(Aarhus_1860, koen == "mand" & stilling == "daglejer" | stilling == "indsidder") %>% 
  count(stilling)
Aarhus_1860_mand_fattig <- filter(Aarhus_1860, koen == "mand" & stilling == "almisselem" | stilling == "fattiglem" | stilling == "understøttes af andre") %>% 
  count(stilling)

#Filtering the data for the women
Aarhus_1860_kvinde_dag_ind <- filter(Aarhus_1860, koen == "kvinde" & stilling == "daglejer" | stilling == "indsidder") %>% 
  count(stilling)
Aarhus_1860_kvinde_fattig <- filter(Aarhus_1860, koen == "kvinde" & stilling == "almisselem" | stilling == "fattiglem" | stilling == "understøttes af andre") %>% 
  count(stilling)

Aarhus_1860_mand_dag_ind
Aarhus_1860_mand_fattig
Aarhus_1860_kvinde_dag_ind
Aarhus_1860_kvinde_fattig