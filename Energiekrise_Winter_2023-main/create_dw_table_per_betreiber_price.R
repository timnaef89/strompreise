# Dieses Skript zeigt, wie sich Datawrapper-Tabellen für alle Kombinationen von Betreiber-Strompreisen generieren lassen
library(tidyverse)
source("custom_DW_functions.R")
Sys.setenv(DW_API_KEY = "") # ergänzen

# Das ist der Datensatz 2022 mit der Gemeinde-Zuordnung (bereinigt)
dta_elcom_22_plus <- read_csv("Elcom_Tarife_2022_PLUS.xlsx - cleaned.csv")

# hier sieht man, dass es Gemeinden gibt, die unterschiedliche Abgaben haben, d.h. kein Matching möglich
dta_elcom_22_plus %>% 
  select(Gemeinde_Nr, H4_Abgaben, C3_Abgaben) %>% 
  mutate(diff_abgaben = H4_Abgaben - C3_Abgaben) %>% 
  filter(diff_abgaben != 0)

# Dummy-Daten mit Vorjahr
# ACHTUNG: Wir benötigen NUR die Gemeinden in unserem Einzugsgebiet (use gde_required aus get_gde_required.R)
dta_elcom_22_dummy_unique <- dta_elcom_22_plus %>% 
  transmute(Betreiber_Name,
            H4_Total_aktuelles_Jahr = round(H4_Total * 4500/100,0), # wir berechnen hier den Jahrespreis für H4
            # DUMMY data
            H4_Total_Vorjahr = (H4_Total + round(runif(1, -1, 1),0)) * 4500/100, 
            C3_Total_aktuelles_Jahr = round(C3_Total * 150000/100,0),
            # DUMMY data
            C3_Total_Vorjahr = (C3_Total + round(runif(1, -1, 1),0)) * 150000/100) %>%
  unique() # mit unique() haben wir jede Kombination nur EINMAL

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
betreiber_one_line <- function(price_thisyear,
                               price_yearbefore) {
  
  price_change <-  price_thisyear - price_yearbefore
  percentage_change <- price_change  / price_yearbefore * 100
  
  out <- tibble(`Stromkosten ^in CHF^` = paste0(price_thisyear, "^pro Jahr^"),
                `+/- ^in CHF^` = paste0(round(price_change,0), " ^(", 
                                        ifelse(percentage_change > 0, paste0("+", round(percentage_change, 1)), round(percentage_change)),"%)^"))
  
  return(out)
  
}

# Funktion zum Erstellen einer DW-Tabelle 
# data_line = eine Zeile eines Tibbles/Dataframes
create_dw_chart_betreiber <- function(data_line) {
  
  h4_line <- cbind(tibble(`Kategorie ^Ø-Verbrauch^` = "4-Zimmer-Whg^4500 kWh^"),
                   betreiber_one_line(price_thisyear = data_line$H4_Total_aktuelles_Jahr,
                                      price_yearbefore = data_line$H4_Total_Vorjahr))
  
  c3_line <- cbind(tibble(`Kategorie ^Ø-Verbrauch^` = "Mittlerer Betrieb^150'000 kWh^"),
                   betreiber_one_line(price_thisyear = data_line$C3_Total_aktuelles_Jahr,
                                      price_yearbefore = data_line$C3_Total_Vorjahr))
  
  dta_for_dw_table <- rbind(h4_line,
                            c3_line)
  
  meta_of_new_dw_table <-  dw_copy_update_publish(template_id = "7EtPS", # hardcoded!
                                                  title = data_line$Betreiber_Name, # kann auch ergänzt werden zu einem ausführlicheren Titel
                                                  folder_id = "115714", # Hardcoded!
                                                  data = dta_for_dw_table, 
                                                  return_id_only = F)
  
  # Embed code der neuen DW-Tabelle ausgeben
  embed_code <- meta_of_new_dw_table$content$metadata$publish$`embed-codes`$`embed-method-responsive`
  
  return(embed_code)
  
}

# Für jede Zeile in den (Dummy-)Daten wird eine Tabelle erstellt
# (das ist nichts anderes als ein fancy for-loop)
all_embed_codes <- map_chr(205:206, # Variante zum Ausprobieren
# all_embed_codes <- map_chr(1:nrow(dta_elcom_22_dummy_unique), # Variante mit allen Betreibern
                           function(i) create_dw_chart_betreiber(dta_elcom_22_dummy_unique[i,]))

# Die Embed-Codes werden als neue Variable an den Datensatz angehängt

# Dafür benötigen wir diese Hilfsfunktion, welche das iframe von Sonderzeichen und <script> säubert
sanitise_iframe <- function(i) {
  
  a <- tibble(nodq = str_replace_all(i, "\\\"", "'")) %>% 
    separate(nodq, into = c("out", "biff"), sep = "\\<script ") %>% 
    pull(out)
  
  return(a)
  
}

dta_elcom_22_dummy_unique_with_dw <- dta_elcom_22_dummy_unique %>% 
  slice(3:4) %>% 
  mutate(iframe_embed_code = sanitise_iframe(all_embed_codes)) %>% 
  mutate(dw_betreiber_chart = paste0("DW_BETREIBER_CHART", seq_along(Betreiber_Name)))

# Wir können jetzt dta_elcom_22_dummy_unique_with_dw wieder an den Ursprungs-Datensatz joinen (hier: dta_elcom_22_plus, mit Betreiber_Name und den Totalpreisen als ID). Das machen wir, bevor wir den Datensatz mit gather-spread in die Form bringen, wie wir ihn für Arria benötigen. Denn wenn wir iframe_embed_code und dw_betreiber_chart vorher drin haben, kriegen wir u.a. "Anb_1_dw_betreiber_chart", das wir dann in Arria nutzen können. 
# Jetzt haben wir die Wahl, in Arria das Kürzel für replace_table.csv zu nutzen oder das ganze iframe direkt einzubinden.
# In Arria haben wir die Variable anzahl_anbieter: Damit können wir eine if-else-Kaskade schaffen, wie viele Anb_x_dw_betreiber_chart (1-3) wir abrufen müssen.
