# towards creating a line chart for each MS region
library(tidyverse)

# install Datawrappr package
#devtools::install_github("munichrocker/DatawRappr")

# use custom Datawrapper functions (based on Datawrappr, but slightly different plus some extra functions, notably dw_update_and_republish)
source("gde_texte/DW/custom_DW_functions.R")




# set DW API KEY

# siehe txt.key
#Sys.setenv(DW_API_KEY = "88c1zJgLmvaDvrsoUGTEEo04CSbmOp6Amb1RTenMeGWq4poourBtgRpenjHb92y7")

# set folder number (as string!)
folder_region_increase <- "115714"

-# load data
library (readr)
library(tidyverse)

# get data

source("dta_exploring.R")

dta_tabelle1 <- dta_anbieter 
  


# function that returns the data of one `region`
create_dta_per_gemeinde <- function(Gemeindename) {
  
out <- gde1 %>% 
  filter(Gemeinde == Gemeindename)
  
    
  
  return(out)
  
}


#HIER WEITERMACHEN !!!
  
  
# function that creates one chart (based on template `old_chart_id`) -and saves the iframe code in a table (`rpl_tbl_path`)
create_new_chart <- function(neuer_Gemeinde_Name,
                             neue_Gemeinde_Nr,
                             old_chart_id = "7EtPS" ,
                             new_folder = "115714", 
                             rpl_tbl_path) {
  
  new_dta <- create_dta_per_gemeinde(Gemeindename = neuer_Gemeinde_Name)
  
  new_chart <- DatawRappr::dw_copy_chart(copy_from = old_chart_id,
                                         api_key = Sys.getenv("DW_API_KEY"))
  
  dw_edit_chart(chart_id = new_chart$id,
                title = paste0(""), 
                intro = paste0(""),
                annotate = paste0("Kränze pro Jahr für die Gemeinde", neuer_Gemeinde_Name, "Note: 2020 wurde die Schwinger-Saison wegen Covid-19 abgesagt."),
                folderId = new_folder,
                api_key = Sys.getenv("DW_API_KEY"))
  
  dw_update_and_republish(dw_id = new_chart$id,
                          data_for_dw = new_dta)
  
  new_chart_meta <- DatawRappr::dw_retrieve_chart_metadata(chart_id = new_chart$id, 
                                                           api_key = Sys.getenv("DW_API_KEY"))
  
  new_tbl <- read_csv(rpl_tbl_path, col_types = cols(
    MS_Gemeinde_Nr = col_double(),
    MS_Gemeinde_Name = col_character(),
    iframe_full = col_character()
  )) %>% 
    add_row(MS_Gemeinde_Nr = neue_Gemeinde_Nr,
            MS_Gemeinde_Name = neuer_Gemeinde_Name,
            iframe_full = new_chart_meta$content$metadata$publish$`embed-codes`$`embed-method-responsive`)
  
  write_csv(new_tbl, rpl_tbl_path)
  
  return(new_chart)
  
}

###
### Do IT
###

# create empty table to store iframe codes in it
embeds_chart_increase_empty <- tibble(MS_Gemeinde_Nr = numeric(0),
                                      MS_Gemeinde_Name = character(0),
                                      iframe_full = character(0))
write_csv(embeds_chart_increase_empty, "embeds_chart_einb_test.csv")

# data frame with all required regions in (name and number)

gde1.3 <- read_csv("gde_texte/schwinger-detail.csv") 

ms_data <- gde1.3 %>% 
  select(nr = GDENR, name = Gemeinde) %>% 
  unique() 

#create sampleset


ms_data_test <- ms_data %>% 
  filter(nr %in% c(3360,1503,3295))

walk2(ms_data_test$nr, ms_data_test$name,
      function(x, y) create_new_chart(neuer_Gemeinde_Name = y,
                                      neue_Gemeinde_Nr = x, 
                                      rpl_tbl_path = "embeds_chart_einb_test.csv"))

#Try out for one region (Zürich)
# chart_zrh <- create_new_chart(neuer_Gemeinde_Name = "Herisau", neue_Gemeinde_Nr = 3001)
# 
# ee <- read_csv("embeds_chart_increase_V2.csv")
# chart_prtg <- create_new_chart(new_region = "Prättigau")
# 
# test <- read.csv("embeds_chart_increase_V2.csv")



############################# Zweite grafik ###################################

#import data

dta_kraenze <- read_csv("dta_schwinger_pro_gde.csv")
  


# set folder number (as string!)
folder_region_increase <- "114931"

# function that returns the data of one `region`
create_dta_per_gemeinde_2 <- function(Gemeindename) {
  
  out <- dta_kraenze %>% 
    filter(gde == Gemeindename)
  
    return(out)
  
}

# function that creates one chart (based on template `old_chart_id`) -and saves the iframe code in a table (`rpl_tbl_path`)
create_new_chart2 <- function(neuer_Gemeinde_Name,
                             neue_Gemeinde_Nr,
                             old_chart_id = "51YL6" ,
                             new_folder = "114931", 
                             rpl_tbl_path) {
  
  new_dta <- create_dta_per_gemeinde_2(Gemeindename = neuer_Gemeinde_Name)
  
  new_chart <- DatawRappr::dw_copy_chart(copy_from = old_chart_id,
                                         api_key = Sys.getenv("DW_API_KEY"))
  
  einb_gesamt <- data3_gesamt %>% filter(Gemeinde_Name == neuer_Gemeinde_Name) %>% select(gesamt)
  
  dw_edit_chart(chart_id = new_chart$id,
                title = paste0("Einbürgerungen in der Gemeinde ", neuer_Gemeinde_Name, " pro Jahr"),
                intro = paste0("Seit 2011 erhielten ", einb_gesamt, " Menschen den Schweizer Pass."),
                #annotate = paste0("Anzahl Einbürgerungen für ", neuer_Gemeinde_Name),
                folderId = new_folder,
                api_key = Sys.getenv("DW_API_KEY"))
  
  dw_update_and_republish(dw_id = new_chart$id,
                          data_for_dw = new_dta)
  
  new_chart_meta <- DatawRappr::dw_retrieve_chart_metadata(chart_id = new_chart$id, 
                                                           api_key = Sys.getenv("DW_API_KEY"))
  
  new_tbl <- read_csv(rpl_tbl_path, col_types = cols(
    MS_Gemeinde_Nr = col_double(),
    MS_Gemeinde_Name = col_character(),
    iframe_full = col_character()
  )) %>% 
    add_row(MS_Gemeinde_Nr = neue_Gemeinde_Nr,
            MS_Gemeinde_Name = neuer_Gemeinde_Name,
            iframe_full = new_chart_meta$content$metadata$publish$`embed-codes`$`embed-method-responsive`)
  
  write_csv(new_tbl, rpl_tbl_path)
  
  return(new_chart)
  
}

###
### Do IT
###

# create empty table to store iframe codes in it
embeds_chart_increase_empty <- tibble(MS_Gemeinde_Nr = numeric(0),
                                      MS_Gemeinde_Name = character(0),
                                      iframe_full = character(0))
write_csv(embeds_chart_increase_empty, "embeds_chart_einb_test2.csv")


# data frame with all required regions in (name and number)
# ms_data <- read.csv("datawrapper_gemeindetexte.csv", fileEncoding = "Latin1") %>% 
#   select(nr = Gemeinde_Nr, name = Gemeinde_Name) %>% 
#   unique() %>% 
#   head(3)

walk2(ms_data$nr, ms_data$name,
      function(x, y) create_new_chart2(neuer_Gemeinde_Name = y,
                                      neue_Gemeinde_Nr = x, 
                                      rpl_tbl_path = "embeds_chart_einb_test2.csv"))

#Try out for one region (Zürich)
chart_zrh <- create_new_chart(neuer_Gemeinde_Name = "Herisau", neue_Gemeinde_Nr = 3001)

ee <- read_csv("embeds_chart_increase_V2.csv")
chart_prtg <- create_new_chart(new_region = "Prättigau")

test <- read.csv("embeds_chart_increase_V2.csv")