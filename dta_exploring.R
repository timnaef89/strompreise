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




# aktuellste tabelle anbieter rausfiltern


anbieter_check <- dta_gde %>% 
  select(Name) %>% 
  unique()



missing_operators_past <- dta_gesamt %>% 
  filter(!operatorLabel %in% anbieter_check$Name) %>% 
  select(operatorLabel) %>% 
  unique()



# There are 175 missing operators!
