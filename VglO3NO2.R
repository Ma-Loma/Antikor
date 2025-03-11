rm(list=ls())

library(tidyverse)
library(readr)
library(viridis)
library(readxl)

bereinigen<-function (string){
  string %>% 
    gsub("ü","ue",.) %>%
    gsub("ö","oe",.) %>%
    gsub("ä","ae",.) %>%
    gsub("aße","",.) %>%
    gsub("trasse","tr",.) %>%
    gsub("ß","ss",.) %>% 
    gsub("[ .,-/]","",.)
}

Stationsnamen_Kategorie <- read_excel("data/Stationsnamen_Kategorie.xlsx") %>% 
  mutate(Station=bereinigen(Station))

no2Jahresmittel <- read_delim(
  "data/no2.txt",
  delim = ";",
  escape_double = FALSE,
  trim_ws = TRUE,
  locale = locale(decimal_mark = ",")
) %>%
  select(where(function(x)
    ! all(is.na(x)))) %>% 
  mutate(Stoff="NO2") %>% 
  mutate(Station=bereinigen(Station))

o3Jahresmittel <- read_delim(
  "data/o3.txt",
  delim = ";",
  escape_double = FALSE,
  trim_ws = TRUE,
  locale = locale(decimal_mark = ",")
) %>%
  select(where(function(x)
    ! all(is.na(x)))) %>% 
  mutate(Stoff="O3") %>% 
  mutate(Station=bereinigen(Station))

beideJahresmittel <- bind_rows(no2Jahresmittel, o3Jahresmittel) %>% 
  pivot_longer(-c(Station, Stoff), names_to="Jahr", values_to="Wert")%>% 
  filter(!is.na(Wert)) %>% 
  mutate(Jahr=as.numeric(Jahr)) %>% 
  left_join(Stationsnamen_Kategorie, by="Station")

beideJahresmittel %>% 
  filter(is.na(Kategorie)) %>% 
  .$Station %>% 
  unique()

names1<-read_lines("data/Tagesmittelwerte seit 2005.txt", n_max = 1) %>%
  str_split(";") %>% 
  unlist()

names2<-read_lines("data/Tagesmittelwerte seit 2005.txt",skip=1, n_max = 1) %>%
  str_split(";") %>% 
  unlist()

namesAll<-names1 %>% 
  paste(names2, sep = "_")

tageswerte <- read_delim(
  "data/Tagesmittelwerte seit 2005.txt",
  delim = ";",
  escape_double = FALSE,
  trim_ws = TRUE,
  locale = locale(decimal_mark = ","),
  na = c("", "NA", "#", "Dfue"),
  skip = 3,
  col_names = namesAll
) %>%
  mutate(
    Zeitpunkt = Zeitpunkt_ %>%
      dmy() %>%
      lubridate::as_date(),
    .keep = "unused",
    .before = 1
  ) 
tageswerteLong<-tageswerte %>%
  #pivot_longer(!Zeitpunkt,names_sep="_", names_to="Stoff", values_to="Wert")
  pivot_longer(
    !Zeitpunkt,
    cols_vary = "slowest",
    names_to = c("Station",".value"),
    names_sep = "_"
  ) %>% 
  pivot_longer(
    cols = c(NO2, O3),
    names_to = "Stoff",
    values_to = "Wert"
  ) %>% 
  filter(!is.na(Wert)) %>%
  mutate(Station=bereinigen(Station)) %>% 
  left_join(Stationsnamen_Kategorie, by="Station")

beideJahresmittel %>% 
  ggplot(aes(x=Jahr, y=Wert, group=Station,color=Kategorie))+
  geom_line(aes(group=Station))+
  facet_wrap(vars(Stoff), scales="free_y")

beideJahresmittel %>% 
  filter(Kategorie=="Ländlich"&Stoff=="O3"&Wert<50) %>% 
  select(Station) %>% 
  unique()
  

ggsave(
  "O3_NO2_Zeitverlauf_Jahresmittel.png",
  width = 20,
  height = 10,
  dpi = 300
)

tageswerteLong %>% 
  ggplot(aes(x=Zeitpunkt, y=Wert))+
  geom_line(aes(color=c(Station)))+
  facet_grid(cols=vars(Kategorie),rows=vars(Stoff))

ggsave(
  "O3_NO2_Zeitverlauf_Tagesmittel.png",
  width = 20,
  height = 10,
  dpi = 300
)

beideJahresmittel %>%
  pivot_wider(names_from = Stoff, values_from = Wert) %>%
  ggplot(aes(x = NO2, y = O3)) +
  xlab("NO2 yearly average") + ylab("O3 yearly average") +
  geom_point(aes(color = factor(Jahr))) +
  geom_line(aes(color = factor(Jahr), group = Station)) +
  scale_color_viridis(discrete=TRUE, option="viridis")+
  facet_wrap(vars(Kategorie))+
  coord_cartesian(xlim = c(0, 40), ylim = c(25, 85))

ggsave(
  "O3_NO2_Korrelation_Jahresmittel.png",
  width = 20,
  height = 10,
  dpi = 300
)


tageswerteLong %>%
  mutate(Quartal=lubridate::quarter(Zeitpunkt) %>% paste0("Q",.)) %>% 
#  slice_sample(n=100000) %>%
  pivot_wider(names_from = Stoff, values_from = Wert) %>% 
  ggplot(aes(x = NO2, y = O3)) +
  xlab("NO2 daily average") + ylab("O3 daily average") +
  geom_point(aes(color = Zeitpunkt),shape=".") +
  #geom_line(aes(color = Zeitpunkt, group = Station)) +
  scale_color_viridis(discrete=FALSE, option="viridis", labels=as.Date)+
  facet_grid(rows=vars(Quartal),cols=vars(Kategorie))+
  coord_cartesian(xlim = c(0, 80), ylim = c(0, 160))

ggsave(
  "O3_NO2_Korrelation_Tagesmittel.png",
  width = 20,
  height = 10,
  dpi = 300
)
