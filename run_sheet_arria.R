# load required functions and libraries
source("functions_arria.R") 
source("functions_newsml.R")
source("get_gde_required.R") # Hier werden die Gemeinde-Nummern 
library(httr)
library(glue)
library(unglue)
library(xml2)
library(RCurl)
library(textutils)
library(lubridate)

# this function loads "config_arria_connector.json" into a list "config_arria_connector"
load_arria_config()

# we check out config_arria_connector to ensure everything is as it should be
# especially -> ftp_destination!!!
config_arria_connector

# this function creates our export_journal where the details of the exported texts is stored
# ATTENTION: if we want to append an existing export_journal, DO NOT run init_function()
init_function()

# if we want to export our articles newspaper by newspaper, we might need to filter dta_json (see below) to a subset of Gemeinde_Nrs which we create here ("chm_gde_az"):
# AZ
# chm_gde_az <- read_csv("CHM_RSS_FEEDS.csv") %>%
#   filter(!is.na(id), !is.na(rss)) %>%
#   filter(portal %in% c("nwch:aaz2016", "nwch:bat2016")) %>%
#   pull(Gemeinde_Nr)

# load data to generate texts within Arria (with function or sourcing a script)
# in this example, we call this data "dta_json"
# occasionally, we create the dta_json right within "run_sheet_arria.R"
# dta_json <- list(all_gdes = SOME_DATASET %>% 
#                    filter(Gemeinde_Nr %in% chm_gde_az) %>%  # this 
#                    mutate(abst_won = map(Gemeinde_Nr, function(x) get_abst_won(x)),
#                           abst_lost = map(Gemeinde_Nr, function(x) get_abst_lost(x))))

# it is wise to check that all the Gemeinde_Nrs that we want to export are in the range of the portal, and they have a Gemeindeseite, this can be done like so:
# JUST CHECKING: sum(!dta_json$all_gdes$Gemeinde_Nr %in% chm_gde_az) # 0 = ALL GOOD

# HERE WE GENERATE OUR TEXTS (this costs money!)
# we can save the texts as csv (save_as_csv) which allows us to reload the texts (instead of re-geneate), should there an issue during the export process
test1 <- create_gde_dta(dta_4_arria = strompreis_json_test_daten2, save_as_csv = "test_01.csv")

# HERE WE TRANSFORM THEM INTO A LIST OBJECT
test1_objs <- arria_create_text_objects(test1)

# HERE WE EXPORT THE TEXTS (includes entry into our export_journal)
system.time(arria_export_batch(text_objs = test1_objs))

test
