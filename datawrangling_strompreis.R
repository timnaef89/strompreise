#libraries

library(tidyverse)
library(xlsx)


# get data 
source("gde_texte/get_gde_required.R")

# Die Netzbetreiber haben verschiedene Stromtarife, folglich werden für den Tarifvergleich 
# sogenannte Stromverbrauchsprofile (Kategorien z. B. H1 oder C1) verwendet. 
# Alle Tarife werden in Bezug zum Tarif des Schweizer Mediankunden gesetzt und die 
# Abweichung dargestellt. Der Tarif des Mediankunden wird aus allen Tarifen der 
# entsprechenden Kategorie unter Berücksichtigung der Einwohnerzahlen ermittelt. 
# Der Tarif des Mediankunden ist der Wert, welcher in der Mitte des gelben Intervalls liegt. 
# Würde also jeder Einwohner das gewählte Verbrauchsprofil aufweisen, hätte die eine 
# Hälfte der Einwohner einen günstigeren, die andere Hälfte einen teureren Tarif zu bezahlen. 
# Die Rohdaten der Strompreis-Übersicht stammen direkt von den Netzbetreibern und 
# werden von der ElCom nicht geprüft. Die Verantwortung für die Qualität 
# der Daten liegt bei den Netzbetreibern.


# variablen

# Das csv-File enthält die Rohdaten für die Standard- und günstigsten Tarife aller 
# Preiskategorien sämtlicher Netzbetreiber. Zur Berechnung der Durchschnittswerte
# vgl. auch www.elcom.admin.ch >  Themen > Strompreise > EDES – 
# Neues ElCom Dateneinliefersystem > Wegleitung zur Tarifdeklaration.  
# Es werden folgende Komponenten ausgewiesen:

# period	        ->  Tarifjahr (es wird standardmässig das csv des Jahres angezeigt, das Sie in der Auswahl gewählt haben)
# operator	      ->  Netzbetreiber ID
# operatorLabel	  ->  Netzbetreiber
# category	      ->  Tarifkategorie
# product	        ->  Produkt
# aidfee	        ->  Förderabgaben (KEV) in Rp. / kWh
# fixcosts	      ->  Grund- und Leistungspreis, in CHF pro Jahr (falls vorhanden)
# charge	        ->  Abgaben an Kantone und Gemeinden in Rp. / kWh
# gridusage	      ->  Total Netznutzungsentgelt in Rp. / kWh
# energy	        ->  Total Elektrizitätspreis in Rp. / kWh
# total	          ->  Total Elektrizitätstarife in Rp. / kWh



# Verbrauchsprofile typscher Haushalte

# H1: 1'600 kWh/Jahr: 2-Zimmerwohnung mit Elektroherd
# H2: 2'500 kWh/Jahr: 4-Zimmerwohnung mit Elektroherd
# H3: 4'500 kWh/Jahr: 4-Zimmerwohnung mit Elektroherd und Elektroboiler
# H4: 4'500 kWh/Jahr: 5-Zimmerwohnung mit Elektroherd und Tumbler (ohne Elektroboiler)
# H5: 7'500 kWh/Jahr: 5-Zimmer-Einfamilienhaus mit Elektroherd, Elektroboiler und Tumbler
# H6: 25'000 kWh/Jahr: 5-Zimmer-Einfamilienhaus mit Elektroherd, Elektroboiler, Tumbler und mit elektrischer Widerstandsheizung
# H7: 13'000 kWh/Jahr: 5-Zimmer-Einfamilienhaus mit Elektroherd, Elektroboiler, Tumbler, Wärmepumpe 5 kW zur Beheizung
# H8: 7'500 kWh/Jahr: Grosse, hoch elektrifizierte Eigentumswohnung
# Verbrauchsprofile von Gewerbe- und Industriebetrieben:
#   C1: 8'000 kWh/Jahr: Kleinstbetrieb, max. beanspruchte Leistung: 8 kW
# C2: 30'000 kWh/Jahr: Kleinbetrieb, max. beanspruchte Leistung: 15 kW
# C3: 150'000 kWh/Jahr: Mittlerer Betrieb, max. beanspruchte Leistung: 50 kW
# C4: 500'000 kWh/Jahr: Grosser Betrieb , max. beanspruchte Leistung: 150 kW, Niederspannung
# C5: 500'000 kWh/Jahr: Grosser Betrieb, max. beanspruchte Leistung: 150 kW, Mittelspannung, eigene Transformatorenstation
# C6: 1'500'000 kWh/Jahr: Grosser Betrieb, max. beanspruchte Leistung: 400 kW, Mittelspannung, eigene Transformatorenstation
# C7: 7'500'000 kWh/Jahr: Grosser Betrieb, max. beanspruchte Leistung: 1'630 kW, Mittelspannung, eigene Transformatorenstation



# start datawrangling



# daten 2022
dta_elcom_22_plus <- read_csv("Energiekrise_Winter_2023-main/Elcom_Tarife_2022_PLUS.xlsx - cleaned.csv") %>% 
  rename(H4_Total_2022 = H4_Total)

# hier werden die daten von 2023 eingelesen und an die daten von 2022 gejoined

#dta_elcom_23_plus <- read_csv("2023.csv") %>% 
  rename(H4_Total_2023 = H4_Total)


#dta_real_gesamt <- left_join(dta_elcom_22_plus, dta_elcom_23_plus, by = c("Gemeindenummer" = "Gemeindenummer"))

dta_real_gesamt2 <- dta_real_gesamt %>% 
transmute(Name = Betreiber_Name,
          Gemeinde_Nr,
          Gemeinde_Name,
          Kanton,
          preis_2022 = H4_Total_2022,
          preis_2023 = H4_Total_2023) %>% 
  mutate(verbrauch = 4500) %>% 
  mutate(change_in_prozent = round(((preis_2023/preis_2022)-1)*100, digits = 2),
         verbrauch = 4500,
         preis_pro_2022 = preis_2022 * verbrauch / 100,
         preis_pro_2023 = preis_2023 * verbrauch / 100,
         differenz = preis_pro_2023 - preis_pro_2022) %>% 
  rename(gde_name = Gemeinde_Name,
         gde_nr = Gemeinde_Nr) %>% 
  mutate(change_in_prozent = sqrt(change_in_prozent^2),
         entwicklung = round(((preis_2023/preis_2022)-1)*100, digits = 2)) %>% 
  ungroup() %>% 
  mutate(gruppen = as.numeric(ntile(change_in_prozent, 5)),
         
         rang = as.numeric(min_rank(desc(change_in_prozent))),
         rang_preis_pro_2023 = as.numeric(min_rank(desc(preis_pro_2023))))

# !!!
# an dieser stelle werden die iframe-codes der grafiken gejoined
# !!!



dta_real_gesamt3 <- dta_real_gesamt2 %>% 
  group_by(gde_nr) %>% 
  arrange(desc(change_in_prozent)) %>% 
  mutate(anb_nr = seq_along(gde_nr)) %>% 
  ungroup() %>% 
  gather(key = "Variable", value = "Wert", -c("gde_nr", "gde_name", "Kanton", "anb_nr")) %>% 
  mutate(Variable = paste0("Anb_", anb_nr, "_", Variable)) %>% 
  select(-anb_nr) %>% 
  spread(key = Variable, value= Wert) %>% 
  select(-c(Anb_4_change_in_prozent:Anb_4_verbrauch)) %>% 
  group_by(gde_nr) %>% 
  #filter(gde_name == "Degersheim") %>% 
  mutate(Anb_1_change_in_prozent = as.numeric(Anb_1_change_in_prozent),
         Anb_2_change_in_prozent = as.numeric(Anb_2_change_in_prozent),
         Anb_3_change_in_prozent = as.numeric(Anb_3_change_in_prozent)) %>% 
  mutate(change_in_perc_schnitt = mean(c(Anb_1_change_in_prozent, Anb_2_change_in_prozent,Anb_3_change_in_prozent), na.rm = T)) %>% 
  mutate(anzahl_anbieter = ifelse(is.na(Anb_2_change_in_prozent),1,ifelse(is.na(Anb_3_change_in_prozent),2,3)),
         Anb_1_gruppen = as.numeric(Anb_1_gruppen),
         Anb_2_gruppen = as.numeric(Anb_2_gruppen),
         Anb_3_gruppen = as.numeric(Anb_3_gruppen))


#csv für karte erstellen
write_csv(dta_real_gesamt3, "dta_real_gesamt3.csv")

#csv für regionen

#Ost
dta_ostschweiz <- dta_real_gesamt3 %>% 
  filter(Kanton %in% c("SG", "TG", "AR", "AI"))

write_csv(dta_ostschweiz, "dta_ostschweiz.csv")

#Zentral
dta_zentral <- dta_real_gesamt3 %>% 
  filter(Kanton %in% c("LU", "ZG", "OW", "NW", "UR", "SZ"))

write_csv(dta_zentral, "dta_zentral.csv")

# Aargau
dta_aargau <- dta_real_gesamt3 %>% 
  filter(Kanton == "AG")

write_csv(dta_aargau, "dta_aargau.csv")

#Solothurn

dta_solothurn <- dta_real_gesamt3 %>% 
  filter(Kanton == "SO")

write_csv(dta_solothurn, "dta_solothurn.csv")

# Beide Basel

dta_basels <- dta_real_gesamt3 %>% 
  filter(Kanton %in% c("BL", "BS"))

write_csv(dta_basels, "dta_basels.csv")



#liste für json 
strompreis_json_dta_real_gesamt3 <- list(all_gde = dta_real_gesamt3 %>%
                                           filter(gde_nr  %in% gde_required)) #hier könnte man kantonsfilter einbauen
#json für arria erstellen
jsonlite::write_json(strompreis_json_test_daten2, "strompreis_json_dta_real_gesamt3.json")



################################################################################
################################################################################
################################################################################
################################################################################
################################################################################

# nochmals test-datensatz erstellen

test_daten <- dta_elcom_22_plus %>% 
  transmute(Name = Betreiber_Name,
            Gemeinde_Nr,
            Gemeinde_Name,
            Kanton,
            preis_2022 = H4_Total,
            preis_2023 = (H4_Total + round(runif(1, -1, 1),2))) %>% #dummy-zahlen
              mutate(verbrauch = 4500) %>% 
  mutate(change_in_prozent = round(((preis_2023/preis_2022)-1)*100, digits = 2),
         verbrauch = 4500,
         preis_pro_2022 = preis_2022 * verbrauch / 100,
         preis_pro_2023 = preis_2023 * verbrauch / 100,
         differenz = preis_pro_2023 - preis_pro_2022) %>% 
  rename(gde_name = Gemeinde_Name,
         gde_nr = Gemeinde_Nr) %>% 
  mutate(change_in_prozent = sqrt(change_in_prozent^2),
         entwicklung = round(((preis_2023/preis_2022)-1)*100, digits = 2)) %>% 
  ungroup() %>% 
  mutate(gruppen = as.numeric(ntile(change_in_prozent, 5)),
         
         rang = as.numeric(min_rank(desc(change_in_prozent))),
         rang_preis_pro_2023 = as.numeric(min_rank(desc(preis_pro_2023))))

test_daten2 <- test_daten %>% 
  group_by(gde_nr) %>% 
  arrange(desc(change_in_prozent)) %>% 
  mutate(anb_nr = seq_along(gde_nr)) %>% 
  ungroup() %>% 
  gather(key = "Variable", value = "Wert", -c("gde_nr", "gde_name", "Kanton", "anb_nr")) %>% 
  mutate(Variable = paste0("Anb_", anb_nr, "_", Variable)) %>% 
  select(-anb_nr) %>% 
  spread(key = Variable, value= Wert) %>% 
  select(-c(Anb_4_change_in_prozent:Anb_4_verbrauch)) %>% 
  group_by(gde_nr) %>% 
  #filter(gde_name == "Degersheim") %>% 
  mutate(Anb_1_change_in_prozent = as.numeric(Anb_1_change_in_prozent),
         Anb_2_change_in_prozent = as.numeric(Anb_2_change_in_prozent),
         Anb_3_change_in_prozent = as.numeric(Anb_3_change_in_prozent)) %>% 
  mutate(change_in_perc_schnitt = mean(c(Anb_1_change_in_prozent, Anb_2_change_in_prozent,Anb_3_change_in_prozent), na.rm = T)) %>% 
  mutate(anzahl_anbieter = ifelse(is.na(Anb_2_change_in_prozent),1,ifelse(is.na(Anb_3_change_in_prozent),2,3)),
         Anb_1_gruppen = as.numeric(Anb_1_gruppen),
         Anb_2_gruppen = as.numeric(Anb_2_gruppen),
         Anb_3_gruppen = as.numeric(Anb_3_gruppen))

write_csv(test_daten2, "test_daten2.csv")


testtest <- test_daten2 %>% 
  filter(Kanton %in% c("LU", "ZG", "OW", "NW", "UR", "SZ"))


strompreis_json_test_daten2 <- list(all_gde = test_daten2 %>%
                                       filter(gde_nr %in% c(4726, 3401, 1, 3378)))

jsonlite::write_json(strompreis_json_test_daten2, "strompreis_json_test_daten2.json")
