


#' Title
#'
#' @param df 
#' @param Site 
#' @param Date 
#'
#' @return
#' @export
#'
#' @examples
#' 
#' 
select_cols_soils <- function(df, Site, Date){
  df <- df %>% 
    select(Plot, Depth, Core_length, min_N_kgN_ha = `MinN (kg N/ha)`) %>% 
    filter(!is.na(min_N_kgN_ha)) %>% 
    mutate(Site = Site,
           Date = Date)
}

#' Title
#'
#' @param x 
#'
#' @return
#' @export
#'
#' @examples
#' 
replace_0 <- function(x) {x = ifelse(x == 0, NA, x);x}



#' scrape_xl
#'
#' @author Blake List
#' @source PlantandFoodResearch/PFRTextMiner
#' @param url 
#' @param sheet 
#' @param skip 
#'
#' @return
#' @import readxl
#'         tidyverse
#'
#' @export
#'


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
  username <- Sys.getenv("USERNAME")
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



#' replace_0
#' 
#' @description replace all NAs by 0 to do addition. 
#'
#' @param x a vector has NAs
#'
#' @return a vector
#' 
#' @export


replace_0 <- function(x) {x = ifelse(x == 0, NA, x);x}

# Your custom code is a bunch of functions.
create_plot <- function(data) {
  ggplot(data, aes(x = Petal.Width, fill = Species)) +
    geom_histogram(binwidth = 0.25) +
    theme_gray(20)
}

#' calculate_it
#'
#' @param df 
#'
#' @return
#' @export
#'
#' @examples
calculate_it <- function(df){
  df_calculated = df %>% 
    mutate_if(is.numeric, ~ replace_na(.,0)) %>% 
    mutate(Date = as.Date(Date),
           PC = TFW/partFW, 
           perM = PC/HA,
           PP_m2 = PP/HA, 
           AboveGroundWt = perM * partDW,
           leafLiveWt = perM * greenleafDW,
           StemLiveWt = perM * greenstemDW,
           DeadWt = perM * deadDW,
           PodLiveWt = perM * greenpodDW,
           ShellWt = perM * (greenpodDW + maturepodDW),
           GrainWt = grainDW/HA,  # total grain weight, green + mature
           GrainLiveWt = perM * greengrainDW) %>% 
    mutate( # green area
      LeafArea = perM * Leaf_area, #cm2,
      leafLAI = LeafArea/10000, #m
      StemArea = perM * Stem_area,
      stemGAI = StemArea/10000,
      PodArea = perM * Pod_area,
      podGAI = PodArea/10000,
      GAI = leafLAI + stemGAI + podGAI) %>% 
    mutate( # N concentration
      LeafNConc = N_Content_Green_Leaf/100.0, 
      leafN_g = LeafNConc * leafLiveWt, 
      StemNConc = N_Content_Green_Stem/100.0,
      StemN_g = StemNConc *StemLiveWt,
      GrainNConc = N_Content_Green_Grain/100.0,
      GrainLiveN_g = GrainNConc*GrainLiveWt,
      GrainMatureNConc = N_Content_Mat_Grain/100.0,
      GrainMatureN_g = GrainMatureNConc * GrainWt , 
      Grain_crudeP_g = GrainMatureN_g*6.25, 
      PodNConc = N_Content_Green_Pod/100.0, 
      PodLiveN_g = PodNConc*PodLiveWt,
      PodMatureNConc = N_Content_Mat_Pod/100.0,
      PodMatureN_g = PodMatureNConc * ShellWt )
  df_calculated
}

#' sum_it
#'
#' @param var1 
#'
#' @return
#' @export
#'
#' @examples
sum_it<- function(df, var1){
  sum_mean<- df %>% 
    group_by(Site, Date, Trt_name) %>% 
    summarise_at(.vars = var1, list(~ mean(., na.rm = TRUE), se= ~ sd(., na.rm = TRUE)/sqrt(n())))
  
  saveRDS(object = sum_mean, file = here::here("Data_intermediate", paste0("sum_mean_", var1, ".rds")), compress = FALSE)
  sum_mean
}

#' plot_it
#'
#' @param frame 
#' @param var1 
#'
#' @return
#' @export
#'
#' @examples
plot_it<- function(frame=dataset_name,var1){
  
  
  sum_mean <- sum_it(df = frame,var1 = var1)
 
  it_plot <- ggplot(data=sum_mean, aes(x=Date,y=mean, colour=Trt_name, shape=Site))+
    geom_point()+
    geom_line(alpha = 0.8)+ 
    geom_errorbar(aes(ymin=mean-se,ymax=mean+se))+
    facet_grid(Site ~ .) + 
    theme_classic() +
    theme(panel.border = element_rect(fill =NA),
          text = element_text(family = "serif")) + 
    ylab(var1) + 
    scale_x_date(breaks = "2 weeks") +
    scale_y_continuous(expand = c(0,0), limits = c(0, max(pretty(frame[[var1]]))))
  
  it_plot
}
