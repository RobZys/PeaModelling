library(tidyverse)
library(readxl)
library(RCurl)
library(RSQLite)
library(DBI)
source("./R/functions.R")
#constant set up 
url_gs_LN <- "https://iplant.plantandfood.co.nz/project/P442060-13/_layouts/15/WopiFrame.aspx?sourcedoc=/project/P442060-13/Research/Pea_Protein_GS_2018-19_Lincoln.xlsx&action=default"
url_gs_HB <- "https://iplant.plantandfood.co.nz/project/P442060-13/_layouts/15/WopiFrame.aspx?sourcedoc=/project/P442060-13/Research/Pea_Protein_GS_2018-19_HBay.xlsx&action=default"
url_irri <- "https://iplant.plantandfood.co.nz/project/P442060-13/_layouts/15/WopiFrame.aspx?sourcedoc=/project/P442060-13/Research/Peas2018_19_SWCirrigation_Lincoln_HB.xlsx&action=default"
url_soil <- "https://iplant.plantandfood.co.nz/project/P442060-13/_layouts/15/WopiFrame.aspx?sourcedoc=/project/P442060-13/Research/PeaProtein_Soil_Test_Results_2018-19.xlsx&action=default"
url_plantLECO_LN <- "https://iplant.plantandfood.co.nz/project/P442060-13/_layouts/15/WopiFrame.aspx?sourcedoc=/project/P442060-13/Research/Pea%20Protein%202018-19%20Plant%20LECO%20results%20LINCOLN.xlsx&action=default"
url_plantLECO_HB <- "https://iplant.plantandfood.co.nz/project/P442060-13/_layouts/15/WopiFrame.aspx?sourcedoc=/project/P442060-13/Research/Pea%20Protein%202018-19%20Plant%20LECO%20results%20HBAY.xlsx&action=default"
Source_HB = "RAW_HB"
Source_Linc = "RAW_Lincoln"




# greenseeker and sunscan -------------------------------------------------

gs_sheet = "NDVI raw data"

gs_lincoln <- scrape_xl(url_gs_LN, sheet = gs_sheet, skip = 5)
gs_HB <- scrape_xl(url_gs_HB, sheet = gs_sheet, skip = 5)
gs_LN <- gs_lincoln %>% 
  select(Date, Plot, NDVISC) %>% 
  mutate(Date = as.Date(Date),
         Plot = as.character(Plot)) %>% 
  filter(!is.na(Plot))

gs_LN %>% 
  group_by(Date, Plot) %>% 
  dplyr::summarise(NDVI_mean = mean(NDVISC, na.rm = TRUE),
            NDVI_se = sd(NDVISC, na.rm = TRUE)/sqrt(n())) %>% 
  filter(!is.na(Plot)) %>% 
  ggplot(aes(Date, NDVI_mean, group = Plot)) +
  geom_point() +
  geom_line()
# plot 10 in 20181203 has a negative scaled NDVI value, possible to be early stage of the crop
# correct to 0
# plot NA is bare soil, safe to ignore
# 2nd one always lower than the 1st measurement in sowing date one. WHY???
# a bump over 1, not good. how to explain 


gs_HB <-  gs_HB %>% 
  select(Date, Plot, NDVISC) %>% 
  mutate(Date = as.Date(Date),
         Plot = as.character(Plot)) %>% 
  filter(Plot != "bare_soil")
gs_HB %>% 
  group_by(Date, Plot) %>% 
  dplyr::summarise(NDVI_mean = mean(NDVISC, na.rm = TRUE),
                   NDVI_se = sd(NDVISC, na.rm = TRUE)/sqrt(n())) %>% 
  filter(!is.na(Plot)) %>% 
  ggplot(aes(Date, NDVI_mean, group = Plot)) +
  geom_point() +
  geom_line()
# no data avaiable for HB greenseeker or it is somewhere else? 
sunscan = "Sunscan"
sunscan_LN <- scrape_xl(url_gs_LN, sheet = sunscan, skip = 3)
sunscan_LN <- sunscan_LN %>% 
  select(Date, Plot = Plot...12,Light_Int_Corr)   %>% 
  filter(Plot != "above") %>%  # Light int is calculated values
  mutate(Date = as.Date(Date))

sunscan_LN %>% 
  group_by(Date, Plot) %>% 
  dplyr::summarise(light_mean = mean(Light_Int_Corr, na.rm = TRUE),
                   light_se = sd(Light_Int_Corr, na.rm = TRUE)/sqrt(n())) %>% 
  ggplot(aes(Date, light_mean, group = Plot,color = Plot)) +
  geom_point() +
  geom_line()

# sunscan is sampled in selected plots
sunscan = "Ceptometer"
sunscan_HB <- scrape_xl(url_gs_HB, sheet = sunscan, skip = 0)
sunscan_HB <- sunscan_HB %>% 
  select(Date, Plot = `Plot no`,Light_Int_Corr = LI) %>% 
  filter(Plot != "above") %>% 
  mutate(Date = as.Date(Date),
         Plot = as.character(Plot))

sunscan_HB %>%  # Light int is calculated values
  group_by(Date, Plot) %>% 
  dplyr::summarise(light_mean = mean(Light_Int_Corr, na.rm = TRUE),
                   light_se = sd(Light_Int_Corr, na.rm = TRUE)/sqrt(n())) %>% 
 
  ggplot(aes(Date, light_mean, group = Plot, color = as.character(Plot))) +
  geom_point() +
  geom_line()


# plot 1 and 3 have quite a bit variation 

# combine two  ------------------------------------------------------------

sunscan <- sunscan_LN %>%
  mutate(Site = "LN") %>% 
  bind_rows(sunscan_HB %>% 
              mutate(Site = "HB")) %>% 
  select(Site, everything()) %>% 
  mutate(Id = 1:n()) # add unique identifier for the table 

gs <- gs_LN %>% 
  mutate(Site = "LN") %>% # add unique identifier for each site 
  bind_rows(gs_HB %>% 
              mutate(Site = "HB")) %>% 
  select(Site, everything())%>% 
    mutate(Id = 1:n()) # add unique identifier for the table 


# soil --------------------------------------------------------------------


  

# [1] "Basic Test & BD"         "MinN_Lincoln_12-09-18"  
# [3] "MinN_Lincoln_28-01-19"   "MinN_Lincoln_04-02-2019"
# [5] "MinN_Lincoln_19-02-2019" "MinN_HBay_Feb2019"      
# [7] "Pivots"                  "TrialPlan_Lincoln"      
# [9] "TrialPlan_HB"           
# 

# creat SQL3 and build the connection  ------------------------------------
con <- dbConnect(RSQLite::SQLite(),'./Data/Pea_data.sqlite3')
dbListTables(con)

# creat table to correct the type of variable -----------------------------

colnames(gs)
dbExecute(con," CREATE TABLE `greenseeker` (
  `Site` TEXT,
  `Date` int NOT NULL,
  `Plot` int NOT NULL,
  `NDVISC` DOUBLE,
  `Id` int NOT NULL,
  PRIMARY KEY (Id)
 
);
")
colnames(sunscan)
dbExecute(con," CREATE TABLE `sunscan` (
  `Site` TEXT,
  `Date` int NOT NULL,
  `Plot` int NOT NULL,
  `Light_Int_Corr` DOUBLE,
  `Id` int NOT NULL,
  PRIMARY KEY (Id)
  
);
")
# write the data into the table  ------------------------------------------

dbWriteTable(con, name = "greenseeker", value = gs,append=TRUE)
dbWriteTable(con, name = "sunscan", value = sunscan,append=TRUE)



gs_sql <- dbReadTable(con, "greenseeker")
gs_sql %>% 
  mutate(date = as.Date(c(17835), origin = "1970-01-01"))
# danger !!! ------
# dbRemoveTable(con, "greenseeker")
# dbRemoveTable(con, "sunscan")
# dbRemoveTable(con, "biomass")

