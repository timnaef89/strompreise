library(tidyverse)


dta_ag <- read_csv("dta_historic.csv") %>% 
  filter(operatorLabel %in% c("AEW Energie AG", "StWZ Energie AG"),
         category == "H3",
         product == "standard") %>% 
  unique()


