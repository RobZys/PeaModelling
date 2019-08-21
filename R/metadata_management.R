library(RSQLite)
library(data.table)
library(dplyr, warn.conflicts = FALSE)

url <- "https://iplant.plantandfood.co.nz/project/P442060-13/_layouts/15/WopiFrame.aspx?sourcedoc=/project/P442060-13/Research/18_19_SAE_pea_protein_managment.xlsx&action=default"
Source_HB = "RAW_HB"
Source_Linc = "RAW_Lincoln"

# Use the function scrape_list to extract the table from iPlant as a dataframe.
df_lincoln <- scrape_xl(url = url, sheet = Source_Linc, skip = 2)
df_HB <- scrape_xl(url = url, sheet = Source_HB, skip = 2)
var_names <- scrape_xl(url, sheet = "VariableNames", skip = 0)
var_names
var_LN <- colnames(df_lincoln)
var_HB <- colnames(df_HB)

nrow(var_names)
var_combined <- data.table(var_names)[, Experiment := NULL]
var_LN <- data.table(LN = var_LN)[, Lincoln := "Lincoln"]
var_HB <- data.table(HB = var_HB)[, HawkesBay := "HB"]

var_LN <- merge(var_combined, var_LN, by.x = "Name", by.y = "LN", all = T , no.dups = T)
var_combined <- merge(var_LN, var_HB, by.x = "Name", by.y = "HB", all = T , no.dups = T )
colnames(var_combined)
setcolorder(var_combined, colnames(var_combined)[c(1,6,7,2:5)])
var_combined[, ':=' (Lincoln = ifelse(Lincoln =="Lincoln", Name, Lincoln),
                     HawkesBay = ifelse(HawkesBay == "HB", Name, HawkesBay))][]

# create a sqlite ---------------------------------------------------------

con <- dbConnect(RSQLite::SQLite(),'./Data/Pea_data.sqlite3')
setkey(var_combined, Name)
anyDuplicated(var_combined)


# write into sqlite ------------------------------------------------------------------


# dbWriteTable(con,'metadata',var_combined, overwrite = TRUE)
dbReadTable(con, 'metadata')
