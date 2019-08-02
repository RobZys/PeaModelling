library(readxl)
library(tidyverse)
library(lubridate)
# source("R/IplantScrape.r")


# devtools::install_github("PlantandFoodResearch/PFRTextMiner", auth_token = Sys.getenv("auth_token"),force = T, dependencies = T)
library(PFRTextMiner)
url <- "https://iplant.plantandfood.co.nz/project/P442060-13/_layouts/15/WopiFrame.aspx?sourcedoc=/project/P442060-13/Research/18_19_SAE_pea_protein_managment.xlsx&action=default"
Source_HB = "RAW_HB"
Source_Linc = "RAW_Lincoln"

# Use the function scrape_list to extract the table from iPlant as a dataframe.
df_lincoln <- scrape_xl(url = url, sheet = Source_Linc, skip = 2)
df_HB <- scrape_xl(url = url, sheet = Source_HB, skip = 2)
# url <- "https://iplant.plantandfood.co.nz/project/I180822/Research/Forms/AllItems.aspx"
# PFRTextMiner::scrape_file(url, dest = "./Data/")
# PFRTextMiner::scrape_list(url) # not working

# repair the name and glimpse the structure of the data
df_lincoln %>% 
  mutate(Index = Source_Linc) %>%
  as_tibble(.name_repair = "universal") %>% 
  glimpse()

# vars start with N are not numeric 
# need to replace the `-` by NA and change type


LN <- df_lincoln %>% 
  as_tibble(.name_repair = "universal") %>% 
  mutate(Index = Source_Linc) %>%
  mutate_at(vars(starts_with("N_")), as.double)


HB <- df_HB %>% 
  as_tibble(.name_repair = "universal") %>% 
  mutate(Index = Source_HB, 
         TFW = ifelse(is.na(TFW2), TFW, TFW2)) %>% # some missing values in both TFW and TFW2, combine both for now 
  mutate_at(vars(starts_with("N_")), as.double) %>% 
  select(- TFW2)

# TEST IF THE COLNAMES ARE THE SAME
identical(colnames(LN),colnames(HB))
setdiff(colnames(LN),colnames(HB))
setdiff(colnames(HB),colnames(LN))
# RETUREN FALSE
# REQUIRE MANUAL CHECKING
