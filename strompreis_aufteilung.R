# Dieses Skript zeigt, wie sich Datawrapper-Tabellen für alle Kombinationen von Betreiber-Strompreisen generieren lassen
library(tidyverse)
source("Energiekrise_Winter_2023-main/custom_DW_functions.R")
Sys.setenv(DW_API_KEY = "") # ergänzen

# Das ist der Datensatz 2022 mit der Gemeinde-Zuordnung (bereinigt)
dta_elcom_22_plus <- read_csv("Energiekrise_Winter_2023-main/Elcom_Tarife_2022_PLUS.xlsx - cleaned.csv")

# Dummy-Daten mit Vorjahr
# ACHTUNG: Wir benötigen NUR die Gemeinden in unserem Einzugsgebiet (use gde_required aus get_gde_required.R)
dta_elcom_22_dummy_unique2 <- dta_elcom_22_plus %>% 
  select(Betreiber_Name, Gemeinde_Nr, Gemeinde_Name, H4_Netznutzung:C3_Total)



dta_elcom_long <- dta_elcom_22_dummy_unique2 %>% 
  pivot_longer(cols = H4_Netznutzung:C3_Total,
               names_to = c("typ", "Komponenten"),
               values_to = c("value"),
               names_sep = "_"
              ) %>% 
  mutate(typ = recode(typ, "H4" = "5-Zw-Whg",
                      "C3" = "Mittlerer Betrieb"),
         value = paste0(value, "&nbsp;"),
         Komponenten = recode(Komponenten, 
                              "Netznutzung" = "Netznutzung ^Rp./kWh^",
                              "Energie" = "Energie ^Rp./kWh^",
                              "Abgaben" = "Abgaben ^Rp./kWh^",
                              "Netzzuschlag" = "Netzzuschlag ^Rp./kWh^",
                              "Total" = "Total ^Rp./kWh^")) %>% 
  pivot_wider(names_from = "typ",
              values_from = "value") %>% 
  mutate(`5-Zw-Whg` = as.character(`5-Zw-Whg`),
         `Mittlerer Betrieb` = as.character(`Mittlerer Betrieb`))

# Diese Hilfsfunktion kopiert ein Muster DW-Chart (template_id), aktualisiert dieses mit Titel (title) und Daten (data), verschiebt es in einen Ordner (folder_id), publiziert es und gibt alle DW-Metadaten zurück (oder wahlweise auch nur die DW-id; return_id_only)
dw_copy_update_publish <- function(template_id,
                                   title,
                                   folder_id,
                                   data, 
                                   return_id_only = F) {
  
  # copy template chart
  new_chart <- DatawRappr::dw_copy_chart(copy_from = template_id,
                                         api_key = Sys.getenv("DW_API_KEY"))  
  
  # move in the right folder
  dw_edit_chart(chart_id = new_chart$id, 
                title = title,
                folderId = folder_id,
                api_key = Sys.getenv("DW_API_KEY"))
  
  
  # add new data and publish
  dw_update_and_republish(dw_id = new_chart$id,
                          data_for_dw = data)
  
  if(return_id_only) { return(new_chart$id) }
  
  # retrieve meta data of new chart
  new_chart_meta <- DatawRappr::dw_retrieve_chart_metadata(chart_id = new_chart$id, 
                                                           api_key = Sys.getenv("DW_API_KEY"))
  
  return(new_chart_meta)
  
}

## Funktion zur Erstellung einer Zeile mit Preisvergleich
# betreiber_one_line2 <- function(price_thisyear,
#                                price_yearbefore) {
#   
#   price_change <-  price_thisyear - price_yearbefore
#   percentage_change <- price_change  / price_yearbefore * 100
#   
#   out <- tibble(`Stromkosten ^in CHF^` = paste0(price_thisyear, "^pro Jahr^"),
#                 `+/- ^in CHF^` = paste0(round(price_change,0), " ^(", 
#                                         ifelse(percentage_change > 0, paste0("+", round(percentage_change, 1)), round(percentage_change)),"%)^"))
#   
#   return(out)
#   
# }

# Funktion zum Erstellen einer DW-Tabelle 
# data_line = eine Zeile eines Tibbles/Dataframes
create_dw_chart_betreiber <- function(data = dta_elcom_long, gde_nr, betr_name) {
  
  data = data %>% 
    filter(Gemeinde_Nr == gde_nr &
             Betreiber_Name == betr_name)
  
  print(data  %>% 
    select(Komponenten:`Mittlerer Betrieb`))
  meta_of_new_dw_table <-  dw_copy_update_publish(template_id = "rUaHG", # hardcoded!
                                                  title = data$Betreiber_Name[1], # kann auch ergänzt werden zu einem ausführlicheren Titel
                                                  folder_id = "115716",# Hardcoded!
                                                  data = data  %>% 
                                                    select(Komponenten:`Mittlerer Betrieb`),
                                                  return_id_only = F)
  
  # Embed code der neuen DW-Tabelle ausgeben
  embed_code <- meta_of_new_dw_table$content$metadata$publish$`embed-codes`$`embed-method-responsive`
  
  return(embed_code)
  
}

# Für jede Zeile in den (Dummy-)Daten wird eine Tabelle erstellt
# (das ist nichts anderes als ein fancy for-loop)
# all_embed_codes <- map_chr(3023, # Variante zum Ausprobieren (gde_nr von Speicher)
#                            # all_embed_codes <- map_chr(unique(dta_elcom_long$Gemeinde_Nr)(dta_elcom_22_dummy_unique), # Variante mit allen Betreibern
#                            function(i) create_dw_chart_betreiber(dta_elcom_long, i))

all_embed_codes <- as.data.frame(t(sapply(c(3023, 97), function(g){
  data = dta_elcom_long %>% 
    filter(Gemeinde_Nr == g)
  sapply(unique(data$Betreiber_Name), function(b){
    data.frame(dw_embed = create_dw_chart_betreiber(dta_elcom_long, g, b), Gemeinde_Nr = g)
  })
})))

all_embed_codes2 <- all_embed_codes %>% 
  rename(embed = V1,
         Gemeinde_Nr = V2) %>% 
  mutate(Gemeinde_Nr = as.numeric(unlist(Gemeinde_Nr)))

# Die Embed-Codes werden als neue Variable an den Datensatz angehängt

# Dafür benötigen wir diese Hilfsfunktion, welche das iframe von Sonderzeichen und <script> säubert
sanitise_iframe <- function(i) {
  
  a <- tibble(nodq = str_replace_all(i, "\\\"", "'")) %>% 
    separate(nodq, into = c("out", "biff"), sep = "\\<script ") %>% 
    pull(out)
  
  return(a)
  
}

dta_elcom_22_plus_with_embed <- dta_elcom_22_plus %>% 
  left_join(all_embed_codes2) %>% 
  mutate(embed = sanitise_iframe(embed))
  
  

# Wir können jetzt dta_elcom_22_dummy_unique_with_dw wieder an den Ursprungs-Datensatz joinen (hier: dta_elcom_22_plus, mit Betreiber_Name und den Totalpreisen als ID). Das machen wir, bevor wir den Datensatz mit gather-spread in die Form bringen, wie wir ihn für Arria benötigen. Denn wenn wir iframe_embed_code und dw_betreiber_chart vorher drin haben, kriegen wir u.a. "Anb_1_dw_betreiber_chart", das wir dann in Arria nutzen können. 
# Jetzt haben wir die Wahl, in Arria das Kürzel für replace_table.csv zu nutzen oder das ganze iframe direkt einzubinden.
# In Arria haben wir die Variable anzahl_anbieter: Damit können wir eine if-else-Kaskade schaffen, wie viele Anb_x_dw_betreiber_chart (1-3) wir abrufen müssen.
