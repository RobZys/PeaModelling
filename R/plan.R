# The workflow plan data frame outlines what you are going to do.
plan <- drake_plan(
  
  con = dbConnect(RSQLite::SQLite(),'./Data/Pea_data.sqlite3'),
  biomass = dbReadTable(con, "biomass"),
  greenseeker = dbReadTable(con, "greenseeker"),
  sunscan = dbReadTable(con, "sunscan"),
  soil = dbReadTable(con, "soilMinN"), 
  LN_mike = dbReadTable(con, "LN_mike_yield"), 
  df_biomass = biomass %>% 
    mutate(Date = as.Date(Date, origin = "1970-01-01")),
  df_greenseeker = greenseeker %>% 
    mutate(Date = as.Date(Date, origin = "1970-01-01")),
  df_sunscan = sunscan %>% 
    mutate(Date = as.Date(Date, origin = "1970-01-01")),
  df_soil = soil %>% 
    mutate(Date = as.Date(Date, origin = "1970-01-01")),
  LN_mike_yield = LN_mike %>% 
    mutate(Date = as.Date(Date, origin = "1970-01-01")),
  DBI::dbDisconnect(con),
  cleaning_report = rmarkdown::render(knitr_in("./R/clean_data_withNotes.Rmd"),output_format = "html_document", output_dir = "./output/", quiet = TRUE),
  report = rmarkdown::render(knitr_in("./R/20190808_EDA.Rmd"),output_format = "html_document", output_dir = "./output/", quiet = TRUE)
  )
