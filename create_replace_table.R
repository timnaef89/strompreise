# create replace_table.csv to insert chart iframes

sanitise_iframe <- function(i) {
  
  a <- tibble(nodq = str_replace_all(i, "\\\"", "'")) %>% 
    separate(nodq, into = c("out", "biff"), sep = "\\<script ") %>% 
    pull(out)
  
  return(a)
  
}

beeswarm_plots <- read_csv("beeswarm_datawrapper_angaben.csv") %>% 
  rename(Gemeinde_Nr = `Unnamed: 0`) %>% 
  mutate(iframe_ld = map_chr(embed_code, sanitise_iframe)) %>% 
  transmute(find = paste0("INSERT_DW_CHART_BEESWARM_", Gemeinde_Nr),
            replace = iframe_ld)

linechart_plots <- read_csv("linechart_datawrapper_angaben.csv") %>% 
  rename(Gemeinde_Nr = `BFS-Nummer`) %>% 
  mutate(iframe_ld = map_chr(embed_code, sanitise_iframe)) %>% 
  transmute(find = paste0("INSERT_DW_LINECHART_", Gemeinde_Nr),
            replace = iframe_ld)

this_is_replace_table <- bind_rows(beeswarm_plots, linechart_plots)

write_csv(this_is_replace_table, "replace_table.csv")  

