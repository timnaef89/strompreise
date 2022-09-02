#libraries

library(tidyverse)
library(xlsx)


# get data 


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

dta_anbieter <- read_csv("data/elcom-data-2022.csv")

dta_anbieter2 <- read_csv("data/elcom-data-2009.csv")

dta_gde <- xlsx::read.xlsx("data/Schweizerische Gemeinden und zuständige Stromnetzbetreiber.xlsx", 1, startRow = 3)


dta_joined <- left_join(dta_gde, dta_anbieter, by = c("Name" = "operatorLabel")) 


dta2 <- dta_joined %>% 
  mutate(category = ifelse(category == "H1", "2_ZW_E_herd", category),
         category = ifelse(category == "H2", "4_ZW_E_herd", category),
         category = ifelse(category == "H3", "4_ZW_E_herd_E_boiler", category),
         category = ifelse(category == "H4", "5_ZW_E_herd_tumbler", category),
         category = ifelse(category == "H5", "5_ZEF_E_herd_boiler_tumbler", category),
         category = ifelse(category == "H6", "5_ZEF_e_heizung", category),
         category = ifelse(category == "H7", "5_ZEF_wärmepumpe", category),
         category = ifelse(category == "H8", "grosse_elektrifizierte_eigentumswohnung", category),
         category = ifelse(category == "C1", "kleinbetrieb_8KW", category),
         category = ifelse(category == "C2", "kleinbetrieb_15KW", category),
         category = ifelse(category == "C3", "mittlerer_betrieb_50KW", category),
         category = ifelse(category == "C4", "grossbetrieb_150KW_niederspannung", category),
         category = ifelse(category == "C5", "grossbetrieb_150KW_mittelspannung", category),
         category = ifelse(category == "C6", "grossbetrieb_400KW", category),
         category = ifelse(category == "C7", "grossbetrieb_1630KW", category),)



# create all dataset with all years from 2009 till 2022


years <- c(2009:2022)

dta_gesamt <- NULL

for (i in years) {
  
  temp <- read_csv(paste0("data/elcom-data-",i,".csv"))
  
  if(is.null(dta_gesamt)){
    dta_gesamt <- as.data.frame(temp)
  }else{
    dta_gesamt <- rbind(dta_gesamt, as.data.frame(temp))
  }
  
}

dta_historic <- write_csv(dta_gesamt, "dta_historic.csv")


# aktuellste tabelle anbieter rausfiltern


anbieter_check <- dta_gde %>% 
  select(Name) %>% 
  unique()



missing_operators_past <- dta_gesamt %>% 
  filter(!operatorLabel %in% anbieter_check$Name) %>% 
  select(operatorLabel) %>% 
  unique()



# There are 175 missing operators!


################################################################################
################################################################################
############################ create dummy-json #################################
################################################################################
################################################################################



df_dummy <- dta_gesamt %>% 
  filter(category %in% c("H3", "C3", "H6"),
         product == "standard",
         period >= 2018) %>% 
  select(period, operatorLabel, category, total) %>% 
  filter(category == "H3")



df_dummy2 <- dta_gde %>% 
  left_join(df_dummy, by = c("Name" = "operatorLabel"))



dummy_anbieter <- c("Energie Gossau AG", "Elektrizitätsversorgung Benken", 
                    "EW Wald AG", "Energie Uster AG","EKZ Einsiedeln AG",
                    "Energie Oberhofen AG", "Energie AG Sumiswald", "NetZulg AG", 
                    "Elektrizitätswerk der PG Wagenhausen", "Thurwerke AG")



# load group-function

# gruppen hinzufügen für späteres "gehört zu den Topgemeinden..."

# w = vector
# x = seq-start
# y = seq-end
# z = anzahl gruppen

gruppen <- function(w,x,y,z){
  as.numeric(cut(w,
                 breaks = c(-Inf,quantile(w, 
                                          seq(x, y, length.out = z)))))
}


dummy2 <- dta_gde %>% 
  filter(Name %in% dummy_anbieter) %>% 
  select(Gde.Nr., Gemeinde, Kanton, Name) %>% 
  left_join(df_dummy, by = c("Name" = "operatorLabel")) %>% 
  filter(period %in% c(2018:2022)) %>% 
  pivot_wider(names_from = period,
              names_prefix = "preis_",
              values_from = total) %>% 
  mutate(change_in_prozent = round(((preis_2022/preis_2021)-1)*100, digits = 2),
         verbrauch = 4500,
         preis_pro_2021 = preis_2021 * verbrauch / 100,
         preis_pro_2022 = preis_2022 * verbrauch / 100,
         differenz = preis_pro_2022 - preis_pro_2021) %>% 
  rename(gde_name = Gemeinde,
         gde_nr = Gde.Nr.) %>% 
  mutate(rang = min_rank(desc(change_in_prozent)),
         gruppen = gruppen(change_in_prozent,
                           .20,
                           1,
                           5),
         entwicklung = ((round(((preis_2019/preis_2018)-1)*100, digits = 2) + 
                          round(((preis_2020/preis_2019)-1)*100, digits = 2) + 
                          round(((preis_2021/preis_2020)-1)*100, digits = 2) + 
                          round(((preis_2022/preis_2021)-1)*100, digits = 2)) /4))


strompreis_json_dummy <- list(all_gde = dummy2 %>%
                                mutate(r = map(gde_nr, function(nr) dummy2 %>% 
                                          filter(nr == gde_nr)))
)

jsonlite::write_json(strompreis_json_dummy, "strompreis_json_dummy.json")


# how many gde with more than 1 netzbetreiber?

netzbetreiber_anzahl_pro_gde <- dta_gde %>% 
  group_by(Gemeinde) %>% 
  summarise(n = n())



# # preisentwicklung pro anbieter für h3
# 
# preisentwicklung_pro_anbieter <- dta_gesamt %>% 
#   filter(period %in% c(2021,2022),
#          category == "H3",
#          product == "standard",
#          operatorLabel == "St.Gallisch-Appenzellische Kraftwerke AG SAK")
# 
# 
# pseudo_gdr <- function(x, data) {
#   
#   preis_pro_anbieter <- preisentwicklung_pro_anbieter %>% 
#     filter(operatorLabel == "St.Gallisch-Appenzellische Kraftwerke AG SAK")
#   
#   label <- as.character(unique(preis_pro_anbieter$charge))
#   
#   preisentwicklung_pro_anbieter2 <- preisentwicklung_pro_anbieter %>% 
#     mutate()
#   
# } 
# 
# 
# 
#   group_by(period, operatorLabel, charge)
#   pivot_wider(names_from = period,
#               names_prefix = "durchschnittspreis_",
#               values_from = steigerung_schnitt) %>% 
#   mutate(steigerung_in_prozent = round(((unique(na.omit(preisentwicklung_pro_anbieter$durchschnittspreis_2022))/
#                                      unique(na.omit(preisentwicklung_pro_anbieter$durchschnittspreis_2021)))-1)*100,2),
#          )



dummy_gesamt <- dta_gde %>% 
  select(Gde.Nr., Gemeinde, Kanton, Name) %>% 
 # filter(Gemeinde == "Degersheim") %>%  
  rowwise() %>%
  mutate(preis_2018 = sample(c(15:30),1),
         preis_2019 = sample(c(15:30),1),
         preis_2020 = sample(c(15:30),1),
         preis_2021 = sample(c(15:30),1),
         preis_2022 = sample(c(15:30),1),
         preis_2023 = sample(c(15:30),1),
         ) %>% 
  mutate(change_in_prozent = round(((preis_2023/preis_2022)-1)*100, digits = 2),
         verbrauch = 4500,
         preis_pro_2022 = preis_2022 * verbrauch / 100,
         preis_pro_2023 = preis_2023 * verbrauch / 100,
         differenz = preis_pro_2023 - preis_pro_2022) %>% 
  rename(gde_name = Gemeinde,
         gde_nr = Gde.Nr.) %>% 
  mutate(change_in_prozent = sqrt(change_in_prozent^2),
         entwicklung = ((round(((preis_2019/preis_2018)-1)*100, digits = 2) + 
                           round(((preis_2020/preis_2019)-1)*100, digits = 2) + 
                           round(((preis_2021/preis_2020)-1)*100, digits = 2) + 
                           round(((preis_2022/preis_2021)-1)*100, digits = 2))+
                          round(((preis_2023/preis_2022)-1)*100, digits = 2))/5) %>% 
  ungroup() %>% 
  mutate(gruppen = as.numeric(ntile(change_in_prozent, 5)),
         
         rang = as.numeric(min_rank(desc(change_in_prozent))),
         rang_preis_pro_2023 = as.numeric(min_rank(desc(preis_pro_2023))))

dummy_gesamt2 <-dummy_gesamt %>% 
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
            
write_csv(dummy_gesamt2, "dummy_gesamt2.csv")


strompreis_json_dummy_gesamt <- list(all_gde = dummy_gesamt2 %>%
                                       filter(gde_nr %in% c(4726, 3401, 1, 3378)) %>% 
                                mutate(r = map(gde_nr, function(nr) dummy2 %>% 
                                                 filter(nr == gde_nr)))
)

jsonlite::write_json(strompreis_json_dummy_gesamt, "strompreis_json_dummy_gesamt.json")
