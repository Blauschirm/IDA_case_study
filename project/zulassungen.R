library(tidyverse)
library(readr)

zulassung_path <- "project/Data/Zulassungen/Zulassungen_alle_Fahrzeuge.csv"
zulassungen_raw <- read_csv2(zulassung_path)

zulassungen <- zulassungen_raw %>%
  drop_na(IDNummer) %>%
  select(IDNummer, Gemeinden, Datum = Zulassung)

zulassungen_pro_gemeinde <- zulassungen %>%
  group_by(Gemeinden) %>%
  summarise(Anzahl = length(Gemeinden)) %>%
  arrange(desc(Anzahl))