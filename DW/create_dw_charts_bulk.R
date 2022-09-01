

# towards creating a line chart for each MS region
library(tidyverse)

# install Datawrappr package
#devtools::install_github("munichrocker/DatawRappr")

# use custom Datawrapper functions (based on Datawrappr, but slightly different plus some extra functions, notably dw_update_and_republish)
source("custom_DW_functions.R")

# set DW API KEY
Sys.setenv(DW_API_KEY = "DG2ZvNZDxvNLvSSwqmQq8nptiEniHmBO8JLzEZeBKWJnoPMGaQdr6cAw0vwTIcBy")

# set folder number (as string!)
folder_region_increase <- "98732"

# load data
library (readr)

urlfile="https://raw.githubusercontent.com/stefantra/create_dw_charts_bulk/main/20220119_Datenlieferung_V.xlsx%20-%20Tabelle1_EFH_long.csv?token=GHSAT0AAAAAABTC6B5CXQNL3QK5IPC4BRWOYSSXQVQ"

efh_long <- read_csv(url(urlfile))

efh_long$lower <- as.numeric(efh_long$lower) 
efh_long$upper <- as.numeric(efh_long$upper) 


# transform data so that it is in the required format
vgl_table <- efh_long %>% 
  mutate(avg = (lower+upper)/2/1000000) %>% 
  filter(Year == 2000) %>% 
  select(Year, Name, avg) %>% 
  spread(Name, avg) %>% 
  bind_rows(efh_long %>% 
              mutate(avg = (lower+upper)/2/1000000) %>% 
              filter(Year == 2021) %>% 
              select(Year, Name, avg) %>% 
              spread(Name, avg))


# function that returns the data of one `region`
create_dta_per_region <- function(dta_all_wide = vgl_table,
                                  dta_efh_long = efh_long,
                                  region) {
  
  out <- dta_all_wide %>% 
    left_join(dta_efh_long %>% 
                filter(Name == region) %>% 
                mutate(avg = (lower+upper)/2/1000000) %>% 
                transmute(Year, principal = avg), by = "Year")
  
  return(out)
  
}

# function that creates one chart (based on template `old_chart_id`) -and saves the iframe code in a table (`rpl_tbl_path`)
create_new_chart <- function(new_region,
                             new_region_nr,
                             old_chart_id = "nWFBY" ,
                             new_folder = folder_region_increase, 
                             rpl_tbl_path) {
  
  new_dta <- create_dta_per_region(region = new_region)
  
  new_chart <- DatawRappr::dw_copy_chart(copy_from = old_chart_id,
                                         api_key = Sys.getenv("DW_API_KEY"))
  
  dw_edit_chart(chart_id = new_chart$id,
                title = paste0("Der Preisanstieg zwischen 2000 und 2021"),
                intro = paste0("Wie viel teurer ein <b>Einfamilienhaus</b> in der Region ", new_region, "&nbsp;<span style='border-left:2px solid #c71e1d; transform: rotate(60deg); display:inline-block; margin:0 6px 0px;'>&nbsp;</span> geworden ist, verglichen mit den übrigen Regionen&nbsp;<span style='border-left:2px solid #c4c4c4; transform: rotate(60deg); display:inline-block; margin:0 6px 6px;'>&nbsp;</span> der Schweiz."),
                folderId = new_folder,
                api_key = Sys.getenv("DW_API_KEY"))
  
  dw_update_and_republish(dw_id = new_chart$id,
                          data_for_dw = new_dta)
  
  new_chart_meta <- DatawRappr::dw_retrieve_chart_metadata(chart_id = new_chart$id, 
                                                           api_key = Sys.getenv("DW_API_KEY"))
  
  new_tbl <- read_csv(rpl_tbl_path, col_types = cols(
    MS_Region_Nr = col_double(),
    MS_Region_Name = col_character(),
    iframe_full = col_character()
  )) %>% 
    add_row(MS_Region_Nr = new_region_nr,
            MS_Region_Name = new_region,
            iframe_full = new_chart_meta$content$metadata$publish$`embed-codes`$`embed-method-responsive`)
  
  write_csv(new_tbl, rpl_tbl_path)
  
  return(new_chart)
  
}

###
### Do IT
###

# create empty table to store iframe codes in it
embeds_chart_increase_empty <- tibble(MS_Region_Nr = numeric(0),
                                      MS_Region_Name = character(0),
                                      iframe_full = character(0))
write_csv(embeds_chart_increase_empty, "embeds_chart_increase_V2.csv")


# data frame with all required regions in (name and number)
ms_regions <- efh_long %>% 
  select(nr = MS_Region, name = Name) %>% 
  unique()

walk2(ms_regions$nr, ms_regions$name,
      function(x, y) create_new_chart(new_region = y,
                                      new_region_nr = x, 
                                      rpl_tbl_path = "embeds_chart_increase_V2.csv"))

# Try out for one region (Zürich)
# chart_zrh <- create_new_chart(new_region = "Zürich", new_region_nr = 1)
# 
# ee <- read_csv("embeds_chart_increase_V2.csv")
# chart_prtg <- create_new_chart(new_region = "Prättigau")