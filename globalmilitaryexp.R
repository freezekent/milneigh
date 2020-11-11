library(tidyverse)
library(sf)
library(httr)
library(readxl)
library(rgdal)

#Flanders Marine Institute (2019). Maritime Boundaries Geodatabase: Maritime Boundaries and Exclusive Economic Zones (200NM),
#version 11. Available online at https://www.marineregions.org/. https://doi.org/10.14284/386

maritime <- st_read("C:/Users/kfreeze/Downloads/World_EEZ_v11_20191118_gpkg/World_EEZ_v11_20191118_gpkg/eez_boundaries_v11.gpkg")

theme_set(theme_bw())

mergecodes <- read.csv("C:/Dropbox/Data/MergingCodes/mergingcodes1.1.csv")

url='https://www.sipri.org/sites/default/files/SIPRI-Milex-data-1949-2019.xlsx'
httr::GET(url, write_disk(tf <- tempfile(fileext = ".xlsx")))

(sipriheader <- read_excel(tf, 5L, n_max =0, range = cell_rows(6:7)) %>%
  names())
borders <- read.csv("https://raw.githubusercontent.com/geodatasource/country-borders/master/GEODATASOURCE-COUNTRY-BORDERS.CSV", stringsAsFactors=FALSE)

sipri <- read_excel(tf, 5L ,range = cell_rows(9:197), col_names = sipriheader) %>%
  setNames(paste0('year', names(.))) %>%
  select(yearCountry, year1949:year2019) %>%
  rename(Country = yearCountry)

sipri[2:72] <- sapply(sipri[2:72], as.numeric)

siprilong <- sipri %>%
  gather(year, milexp, year1949:year2019) %>%
  mutate(year = substr(year, 5, 9)) %>%
  mutate(year=as.numeric(year)) %>%
  filter(year>1991) %>%
  filter(!Country %in% c("Americas", "Asia & Oceania", "Central America and the Caribbean", "Central Asia", 
         "Central Europe", "Czechoslovakia", "East Asia", "Eastern Europe", "Europe",
         "German DR", "Middle East", "North America", "Oceania", "South-East Asia",
         "South America", "South Asia", "Sub-Saharan", "USSR", "Western Europe", 
         "Yemen, North", "Yugoslavia")) %>%
  mutate(Country = ifelse(Country=="Bosnia-Herzegovina", "Bosnia and Herzegovina", Country)) %>%
  mutate(Country = ifelse(Country=="Brunei", "Brunei Darussalam", Country)) %>%
  mutate(Country = ifelse(Country=="Central African Rep.", "Central African Republic", Country)) %>%
  mutate(Country = ifelse(Country=="Congo, Dem. Rep.", "Congo, Democratic Republic", Country)) %>%
  mutate(Country = ifelse(Country=="Congo, Republic of", "Congo", Country)) %>%
  mutate(Country = ifelse(Country=="Côte d'Ivoire", "Cote d'Ivoire", Country)) %>%
  mutate(Country = ifelse(Country=="Czechia", "Czech Republic", Country)) %>%
  mutate(Country = ifelse(Country=="Dominican Rep.", "Dominican Republic", Country)) %>%
  mutate(Country = ifelse(Country=="eSwatini", "Swaziland", Country)) %>%
  mutate(Country = ifelse(Country=="North Macedonia", "Macedonia", Country)) %>%
  mutate(Country = ifelse(Country=="Russia", "Russian Federation", Country)) %>%
  mutate(Country = ifelse(Country=="Trinidad & Tobago", "Trinidad and Tobago", Country)) %>%
  mutate(Country = ifelse(Country=="UAE", "United Arab Emirates", Country)) %>%
  mutate(Country = ifelse(Country=="UK", "United Kingdom", Country)) %>%
  mutate(Country = ifelse(Country=="USA", "United States", Country)) %>%
  mutate(Country = ifelse(Country=="Viet Nam", "Vietnam", Country)) %>%
  #filter(year==2015) %>%
  rename(cname=Country) %>%
  left_join(mergecodes, by=c("cname"))

marborders <- maritime %>%
  select(LINE_NAME, LINE_TYPE, TERRITORY1, SOVEREIGN1, TERRITORY2, SOVEREIGN2) %>%
  st_set_geometry(NULL)

marborders[1:6] <- sapply(marborders[1:6], as.character)

mar <- marborders %>%
  filter(TERRITORY1==SOVEREIGN1) %>%
  filter(TERRITORY2==SOVEREIGN2) %>%
  select(SOVEREIGN1, SOVEREIGN2) %>%
  distinct() %>%
  arrange(SOVEREIGN1) %>%
  rename(country_name=SOVEREIGN1) %>%
  rename(country_border_name=SOVEREIGN2) %>%
  mutate(country_name = ifelse(country_name=="Brunei", "Brunei Darussalam", country_name)) %>%
  mutate(country_name = ifelse(country_name=="Comores", "Comoros", country_name)) %>%
  mutate(country_name = ifelse(country_name=="East Timor", "Timor-Leste", country_name)) %>%
  mutate(country_name = ifelse(country_name=="Ivory Coast", "Cote d'Ivoire", country_name)) %>%
  mutate(country_name = ifelse(country_name=="North Korea", "Korea, North", country_name)) %>%
  mutate(country_name = ifelse(country_name=="Republic of Mauritius", "Mauritius", country_name)) %>%
  mutate(country_name = ifelse(country_name=="Republic of the Congo", "Congo", country_name)) %>%
  mutate(country_name = ifelse(country_name=="Russia", "Russian Federation", country_name)) %>%
  mutate(country_name = ifelse(country_name=="Saint Kitts and Nevis", "St Kitts and Nevis", country_name)) %>%
  mutate(country_name = ifelse(country_name=="Saint Lucia", "St Lucia", country_name)) %>%
  mutate(country_name = ifelse(country_name=="Saint Vincent and the Grenadines", "St Vincent and the Grenadines", country_name)) %>%
  mutate(country_name = ifelse(country_name=="South Korea", "Korea, South", country_name)) %>%
  mutate(country_border_name = ifelse(country_border_name=="Brunei", "Brunei Darussalam", country_border_name)) %>%
  mutate(country_border_name = ifelse(country_border_name=="Democratic Republic of the Congo", "Congo, Democratic Republic", country_border_name)) %>%
  mutate(country_border_name = ifelse(country_border_name=="Federal Republic of Somalia", "Somalia", country_border_name)) %>%
  mutate(country_border_name = ifelse(country_border_name=="North Korea", "Korea, North", country_border_name)) %>%
  mutate(country_border_name = ifelse(country_border_name=="Overlapping claim Qatar / Saudi Arabia / United Arab Emirates", "United Arab Emirates", country_border_name)) %>%
  mutate(country_border_name = ifelse(country_border_name=="Russia", "Russian Federation", country_border_name)) %>%
  mutate(country_border_name = ifelse(country_border_name=="Saint Kitts and Nevis", "St Kitts and Nevis", country_border_name)) %>%
  mutate(country_border_name = ifelse(country_border_name=="Saint Lucia", "St Lucia", country_border_name)) %>%
  mutate(country_border_name = ifelse(country_border_name=="Saint Vincent and the Grenadines", "St Vincent and the Grenadines", country_border_name)) %>%
  mutate(country_border_name = ifelse(country_border_name=="South Korea", "Korea, South", country_border_name)) %>%
  mutate(country_border_name = ifelse(country_border_name=="Western Sahara", "Morocco", country_border_name)) 

mar2 <- mar %>%
  rename(countryname=country_name) %>%
  rename(country_name=country_border_name) %>%
  rename(country_border_name=countryname)

mar3 <- rbind(mar, mar2) %>%
  distinct() %>%
  left_join(mergecodes, by = c("country_name" = "cname")) %>%
  select(country_name, country_border_name, isocc) %>%
  rename(country_code=isocc) %>%
  left_join(mergecodes, by = c("country_border_name" = "cname")) %>%
  select(country_code, isocc) %>%
  rename(country_border_code=isocc) %>%
  arrange(country_code)

mar3[1:2] <- sapply(mar3[1:2], as.character)

landmarborders2 <- borders %>%
  select(country_code, country_border_code) %>%
  mutate(land=1)

landmarborders <- borders %>%
  select(country_code, country_border_code) %>%
  rbind(mar3) %>%
  distinct() %>%
  full_join(landmarborders2, by=c("country_code", "country_border_code")) %>%
  mutate(land = ifelse(is.na(land), 0, land)) %>%
  arrange(country_code) %>%
  filter(country_border_code!="")

siprilm <- landmarborders %>%
  rename(isocc=country_border_code) %>%
  full_join(siprilong, by=c("isocc")) %>%
  group_by(country_code, year) %>%
  summarize(millm=sum(milexp, na.rm = T)) %>%
  rename(isocc=country_code) %>%
  right_join(siprilong, by=c("isocc", "year")) %>%
  mutate(millmrat = milexp/millm) %>%
  mutate(millmrat = ifelse(is.infinite(millmrat), NA, millmrat)) %>%
  select(cname, isocc,ccode, ccodewb, year, milexp, millm, millmrat)

sipril <- landmarborders %>%
  rename(isocc=country_border_code) %>%
  full_join(siprilong, by=c("isocc")) %>%
  filter(land==1) %>%
  group_by(country_code, year) %>%
  summarize(mill=sum(milexp, na.rm = T)) %>%
  rename(isocc=country_code) %>%
  right_join(siprilong, by=c("isocc", "year")) %>%
  mutate(millrat = milexp/mill) %>%
  mutate(millrat = ifelse(is.infinite(millrat), NA, millrat)) %>%
  select(cname, isocc, ccode, ccodewb, year, milexp, mill, millrat)

siprineigh <- sipril %>%
  full_join(siprilm, by=c("ccode", "year", "isocc", "cname", "ccodewb", "milexp"))

save("siprineigh", file="C:/Dropbox/Data/sipri/siprineigh.RData")