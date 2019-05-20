library(readxl)
library(tidyverse)

scrape_xl <- function(url = NULL, sheet = NULL, skip = NULL) {
  
  # Ensure the input url is a character vector.
  stopifnot(is.character(url))
  
  # Check that the input url has the correct form.
  if (!grepl("layout", url) && !grepl(".xl", url)) {
    stop("Url is not of the correct form")
  }
  
  # Extract the base of the url.
  base_url <- url %>%
    stringr::str_split("_layouts") %>%
    lapply(`[[`, 1) %>%
    unlist()
  
  # Extract the extension of the url and replace spaces with encoded format.
  extension <- url %>%
    stringr::str_split("=/") %>%
    lapply(`[[`, 2) %>%
    unlist() %>%
    stringr::str_split(".xlsx") %>%
    lapply(`[[`, 1) %>%
    unlist() %>%
    stringr::str_replace_all(" ", "%20")
  
  # Define the correct file format to be added to the url.
  file_format <- "_layouts/download.aspx?SourceUrl=/"
  
  # Paste the base, format and extension together.
  url_fmt <- paste0(paste(base_url, extension, sep = file_format), ".xlsx")
  
  # Check that the formatted url exists and get the user credentials.
  if (httr::http_error(httr::GET(url_fmt)) == 404L) {
    stop("Url does not exist")
  }
  username <- Sys.getenv("USER")
  if (Sys.getenv("PASSWORD") == "") {
    Sys.setenv("PASSWORD" = getPass::getPass(paste0(username,
                                                    ", enter your password")))
  }
  password <- Sys.getenv("PASSWORD")
  
  # Check that the reponse had no errors with the user credentials.
  if (httr::http_error(httr::GET(url_fmt,
                                 httr::authenticate(username,
                                                    password,
                                                    type = "ntlm")))) {
    stop("The request failed, check username and password",
         httr::http_status(res))
  }
  
  # Get the response to the Excel spreadsheet and write the data to a
  # temporary file.
  httr::GET(url_fmt,
            httr::authenticate(username, password, type = "ntlm"),
            httr::write_disk(temp_file <- tempfile(fileext = ".xlsx")))
  
  # Read the Excel data in from the temp file to a dataframe.
  df <- readxl::read_excel(temp_file, sheet = sheet, skip = skip)
  
  # Return the Excel spreadsheet data from iPlant as a dataframe.
  return(df)
}