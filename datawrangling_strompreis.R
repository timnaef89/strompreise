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