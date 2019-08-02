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
         maturepodFW = as.numeric(maturepodFW),
         maturepodDW = as.numeric(maturepodDW)) %>% 
  mutate_at(vars(starts_with("N_")), as.double)


replace_0 <- function(x) {x = ifelse(x == 0, NA, x);x}
combined_raw <- bind_rows(LN, HB) %>% 
  mutate(Index = gsub("RAW\\_","", Index))

# %>% 
  # mutate_if(is.numeric, ~ replace_0(.))

#replace 0 values by NAs to uniform the missing values in both sites

glimpse(combined_raw) # Some variable turns to logic values becasue no actural values

# TEST IF THE COLNAMES ARE THE SAME
# identical(colnames(LN),colnames(HB))
# RETUREN FALSE
# REQUIRE MANUAL CHECKING

# investigation  ----------------------------------------------------------

#read all variable names from the spreadsheet 
variables <- scrape_xl(url = url, sheet = "VariableNames")

#lower case all names for easy comparision
variables <- variables %>% 
  mutate(Name = tolower(Name))

colnames(combined_raw) <- tolower(colnames(combined_raw))

colnames(combined_raw)[!colnames(combined_raw) %in% variables$Name]

# extra colnames from both sites after joining 
# "tfw2"  from HB, is the total fresh weight after 12 hrs in cool store, use this where it presents
# "deadfw"  from HB, is the sum of dead stem and leaf, not present in Lincoln site
# "grainfw" from HB, is the same as greengrainfw in lincoln
# "pea_num" from HB, is the same as greenpeanum in lincoln
# "deaddw"   from HB, is sum of dry stem and leaf, not present in Lincoln site         
# "graindw"            from HB, is the same as greengraindw in lincoln  
# "final_residuefw"       from HB, is the same as final_residuesubfw in lincoln 
# "pod_num"              from HB, should be the same as final_pod_num in lincoln !!! proceed with caution!!!
# "final_grainsprouted"   from HB, not present in Lincoln
# "final_grainsproutedfw" from HB, not present in Lincoln
# "final_greenpodnumber"  from HB, not present in Lincoln
# "final_greenpodfw"     from HB, not present in Lincoln
# "final_residuedw"      from HB, is the same as final_residuesubfw in lincoln
# "final_greenpoddw"      from HB, not present in Lincoln
# "final_grainsprouteddw"  from HB, not present in Lincoln
# "final_grainfw_hb2"   from HB, weighted in lincoln, use this one for final_grainfw  
# "n_content_dead"  from HB, is the sum of n_content_dead stem and leaf, not present in Lincoln site

#"final_pod.peafw" 

# looking into the colnames with final as prefix
# apdat logic from Rob's `ReadBiomassData.Rmd` file
# creat new var names to have pre-fix for easy identification
combined_clean <- combined_raw %>% 
  mutate_if(is.numeric, ~ replace_na(.,0)) %>% 
  mutate(#fresh weight
         Sub_partFW = ifelse(partfw==0 & index == "Lincoln" , final_residuesubfw + final_pod.peafw, partfw),
         Sub_partFW = ifelse(partfw==0 & index == "HB", final_residuefw + final_podfw + final_grainfw, partfw),
         
         #dry weight
         Sub_partDW = ifelse(index == "Lincoln", final_residuesubdw, final_residuedw),
         Sub_partDW = Sub_partDW + greenleafdw + greenstemdw + greenpoddw,
         
         #dead weight
         Sub_part_deadFW = ifelse(index == "Lincoln", deadstemfw + deadleaffw, deadfw),
         Sub_part_deadDW = ifelse(index == "Lincoln", deadstemdw + deadleafdw, deadfw),
         
         #Total fresh weight correction
         Total_FW = ifelse(is.na(tfw2), tfw, tfw2),
         
         #Grain 
         Sub_grainFW = ifelse(index == "Lincoln", greengrainfw, grainfw),
         Sub_grainDW = ifelse(index == "Lincoln", greengraindw, graindw),
         Sub_final_grain = ifelse(index == "Lincoln", final_grainfw, final_grainfw_hb2), 
         Sub_pea_num = ifelse(index == "Lincoln", greenpeanum, pea_num),
         
         #Pod and shell 
         Sub_pod_num = ifelse(index == "Lincoln", final_pod_num, pod_num),
         
         #N_content 
         # Sub_N_content_dead = ifelse(index == "Lincoln", n_content_dead_stem + n_content_dead_leaf, n_content_dead)  #no valid data
         )
         
       
# calculation

combined_cal <- combined_clean %>% 
  mutate_if(is.numeric, ~ replace_na(.,0)) %>% 
  mutate( #grain 
          Sub_grainDW = ifelse(maturegraindw == 0, Sub_grainDW, Sub_grainDW + maturegraindw),
          
          #pod/shell
          Sub_shellDW = greenpoddw + maturepoddw,
          
          SpoutedPC = final_grainsprouted/final_graincount, # is this necessary
          Sub_final_graindw_mean = final_100graindw/100, # mean value for the final 100 grain 
          Sub_final_graindw = Sub_final_graindw_mean * final_graincount,
          Sub_grainDW_TOTAL = Sub_final_graindw + Sub_grainDW
    ) 

var = colnames(LN)
var = enquo(var)
combined_cal %>% 
  select(1:length(colnames(LN)), starts_with("Sub_"), Total_FW, -contains("green")) %>% 
  mutate(date = as.Date(date),
         PC = )
combined_cal$Sub_partDW
combined_cal$Sub_partFW

var = paste0(colnames(LN), collapse =  ",")
var = substitute(var)


# grain


LN_2 %>% 
 
LN_2$deaddw  
LN_2$deadfw
HB_1$deaddw
HB_1$deadfw


LN_1$partfw
HB_1$partfw
part1="dead"
grep(part1,colnames(LN), value = T)
grep(part1,colnames(HB), value = T)

"deadstemFW" "deadleafFW" "deadstemDW" "deadleafDW"
"deadFW" "deadDW"
part2="grain"
grep(part2,colnames(LN_2), value = T)
grep(part2,colnames(HB_1), value = T)

# lincoln site separate plant into different compnent and weight individually with a combined total
# HB site has two dead 