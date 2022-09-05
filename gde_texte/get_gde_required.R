# file to source gde_required -> this is a vector of the BFS-Gemeinde_Nr for which there exist Gemeindeseiten

url_gde_ids <- "https://api.petitio.ch/v1/cities?limit=1000&resolve=region.province&sort=name"

gde_ids_raw <- jsonlite::fromJSON(url_gde_ids)

gde_ids <- as_tibble(gde_ids_raw$data) %>% 
  filter(!provider %in% c("so", "linth"))

petitio_id_name <- gde_ids %>%
  mutate(portal = portal$id) %>%
  mutate(main_url = map_chr(str_split(gde_ids$portal$rss2, "gemeinde/"), function(x) x[[1]])) %>% 
  select(id, name, provider, portal, main_url) 

##
previous_masterfile_gde <- read_csv("New_MASTERFILE_gde_20210112.csv")

##
# manually_matched_petitio_ids <- read_csv("BFS-NR vs CITY-ID - gde_needing_id.csv")
manually_matched_petitio_ids <- read_csv("BFS-NR vs CITY-ID - gde_needing_id_20210127.csv")

##### establish final list of relevant Gemeindeseiten:
bfs_gde_for_gde_seiten <- petitio_id_name %>% 
  left_join(previous_masterfile_gde %>% select(Gemeinde_Nr, Gemeinde_Name_dw), by = c("name" = "Gemeinde_Name_dw")) %>% 
  filter(!is.na(Gemeinde_Nr)) %>% 
  bind_rows(manually_matched_petitio_ids %>% select(1:5)) %>% 
  filter(!is.na(Gemeinde_Nr)) %>%  # this excludes Fahrweid, currently
  # THESE ARE ALL CHANGES TO GEMEINDE_NR IN 2021
  mutate(Gemeinde_Nr = ifelse(id == "1DIPV", # Altwis (neu Hitzkirch)
                              1030, 
                              Gemeinde_Nr)) %>% 
  mutate(Gemeinde_Nr = ifelse(id == "1lC0R", # Bauen (neu Seedorf)
                              1214,
                              Gemeinde_Nr)) %>%
  mutate(Gemeinde_Nr = ifelse(id == "1MFqP", # Gettnau (neu Willisau)
                              1151,
                              Gemeinde_Nr)) %>%
  mutate(Gemeinde_Nr = ifelse(id == "nwch:2102", # Welschenrohr + Gänsbrunnen
                              2430,
                              Gemeinde_Nr)) %>%
  mutate(Gemeinde_Nr = ifelse(id == "nwch:1111", # Welschenrohr + Gänsbrunnen
                              2430,
                              Gemeinde_Nr)) %>%
  mutate(Gemeinde_Nr = ifelse(id == "nwch:183", # Rohr (neu Stüsslingen)
                              2499,
                              Gemeinde_Nr))

# this needs to be blended with giant_dataset_3 ( %>% filter(Gemeinde_Nr %in% gde_required))  
gde_required <- bfs_gde_for_gde_seiten %>% 
  select(Gemeinde_Nr) %>% 
  unique() %>% 
  unlist() %>% 
  unname()



# function to get city_id by gde_nr
get_city_id_by_bfs_nr <- function(gdenr,
                                  lookup = bfs_gde_for_gde_seiten) {
  
  out <- lookup %>% 
    filter(Gemeinde_Nr == gdenr) %>% 
    select(id) %>% # Only the first department will be used, if there are several
    unlist()
  
  return(out)
  
}

# function to get department by gde_nr
get_department_by_bfs_nr <- function(gdenr,
                                     lookup = bfs_gde_for_gde_seiten,
                                     type_text = "Gemeindetexte") { # may also be "Regiofussball"
  
  if(type_text != "Gemeindetexte" &
     type_text != "Regiofussball") {
    
    stop(paste0("'type_text' must be 'Gemeindetexte' or 'Regiofussball'. It is '", type_text, " tho."))
    
  }
  
  portal_id <- lookup %>% 
    filter(Gemeinde_Nr == gdenr) %>% 
    select(portal) %>% 
    slice(1) %>% 
    unlist()
  
  if(type_text == "Gemeindetexte") {
    
    lookup_reg_dept <- tribble(~region, ~dept,
                               "luzernerzeitung", "ZCH Gemeindetexte",
                               "nwch:aaz2016", "AG Gemeindetexte",
                               "nwch:bat2016", "AG Gemeindetexte",
                               "nwch:bzb2016", "BS Gemeindetexte",
                               "nwch:gtb2016", "SO Gemeindetexte",
                               "nwch:liz2016", "ZH Gemeindetexte",
                               "nwch:ot2016", "SO Gemeindetexte",
                               "nwch:soz2016","SO Gemeindetexte",
                               "stgallertagblatt", "OCH Gemeindetexte")
    
  } else {
    
    lookup_reg_dept <- tribble(~region, ~dept,
                               "luzernerzeitung", "ZCH Regiofussball Spielberichte",
                               "nwch:aaz2016", "AG Regiofussball Spielberichte",
                               "nwch:bat2016", "AG Regiofussball Spielberichte",
                               "nwch:bzb2016", "BS Regiofussball Spielberichte",
                               "nwch:gtb2016", "SO Regiofussball Spielberichte",
                               "nwch:liz2016", "AG Regiofussball Spielberichte",
                               "nwch:ot2016", "SO Regiofussball Spielberichte",
                               "nwch:soz2016","SO Regiofussball Spielberichte",
                               "stgallertagblatt", "OCH Regiofussball Spielberichte")
    
  }
  
  
  out <- lookup_reg_dept %>%
    filter(region == portal_id) %>% 
    select(dept) %>% 
    unlist()
  
  
  # exception for two gde whose portal.id is NA (Bad Ragaz, Flums)
  if(is.na(portal_id)) {
    
    return("OCH Gemeindetexte")
    
  } else {
    
    return(out)
    
  }
  
  return(out)
  
}

