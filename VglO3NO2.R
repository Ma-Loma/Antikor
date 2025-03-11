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

Stationsnamen_Kategorie$Category<-Stationsnamen_Kategorie$Category %>% 
  gsub("Verkehr","Urban Traffic",.) %>%
  gsub("Ländlich","Rural Background",.) %>%
  gsub("Stadt","Urban Background",.)

no2Jahresmittel <- read_delim(
  "data/no2.txt",
  delim = ";",
  escape_double = FALSE,
  trim_ws = TRUE,
  locale = locale(decimal_mark = ",")
) %>%
  select(where(function(x)
    ! all(is.na(x)))) %>% 
  mutate(Pollutant="NO2") %>% 
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
  mutate(Pollutant="O3") %>% 
  mutate(Station=bereinigen(Station))

beideJahresmittel <- bind_rows(no2Jahresmittel, o3Jahresmittel) %>% 
  pivot_longer(-c(Station, Pollutant), names_to="year", values_to="Concentration")%>% 
  filter(!is.na(Concentration)) %>% 
  mutate(year=as.numeric(year)) %>% 
  left_join(Stationsnamen_Kategorie, by="Station")

beideJahresmittel %>% 
  filter(is.na(Category)) %>% 
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
    time = Zeitpunkt_ %>%
      dmy() %>%
      lubridate::as_date(),
    .keep = "unused",
    .before = 1
  )

tageswerteLong<-tageswerte %>%
  #pivot_longer(!time,names_sep="_", names_to="Pollutant", values_to="Concentration")
  pivot_longer(
    !time,
    cols_vary = "slowest",
    names_to = c("Station",".value"),
    names_sep = "_"
  ) %>% 
  pivot_longer(
    cols = c(NO2, O3),
    names_to = "Pollutant",
    values_to = "Concentration"
  ) %>% 
  filter(!is.na(Concentration)) %>%
  mutate(Station=bereinigen(Station)) %>% 
  left_join(Stationsnamen_Kategorie, by="Station")

beideJahresmittel %>% 
  ggplot(aes(x=year, y=Concentration, group=Station,color=Category))+
  geom_line(aes(group=Station))+
  facet_wrap(vars(Pollutant), scales="free_y")+
  labs(x="year",y="Concentration yearly average [µg/m³]")

ggsave(
  "O3_NO2_Zeitverlauf_Jahresmittel.png",
  width = 20,
  height = 10,
  dpi = 300
)

beideJahresmittel %>% 
  filter(Category=="Ländlich"&Pollutant=="O3"&Concentration<50) %>% 
  select(Station) %>% 
  unique()

tageswerteLong %>% 
  ggplot(aes(x=time, y=Concentration))+
  geom_line(aes(color=Station))+
  facet_grid(cols=vars(Category),rows=vars(Pollutant))+
  labs(y="Concentration daily average [µg/m³]")

ggsave(
  "O3_NO2_Zeitverlauf_Tagesmittel.png",
  width = 20,
  height = 10,
  dpi = 300
)

beideJahresmittel %>%
  pivot_wider(names_from = Pollutant, values_from = Concentration) %>%
  mutate(O3=if_else(is.na(O3)&Category=="Urban Traffic",30,O3)) %>%
  mutate(O3=if_else(is.na(O3)&Category=="Urban Background",31,O3)) %>%
  ggplot(aes(x = NO2, y = O3)) +
  xlab("NO2 yearly average") + ylab("O3 yearly average") +
  geom_point(aes(color = factor(year),shape = Category)) +
  geom_line(aes(color = factor(year), group = Station)) +
  scale_color_viridis(discrete=TRUE, option="viridis")+
#  facet_wrap(vars(Category))+
  coord_cartesian(xlim = c(0, 40), ylim = c(30, 85))+
  scale_shape_manual(values = c(2:4))+ 
  guides(
    colour = guide_legend(
      title = "Year")
  )

ggsave(
  "O3_NO2_Korrelation_Jahresmittel.png",
  width = 20,
  height = 10,
  dpi = 300
)

tageswerteLong %>%
  mutate(Quartal=lubridate::quarter(time,fiscal_start = 12) %>% paste0("Q",.)) %>% 
  mutate(Quartal=Quartal %>% 
           gsub("Q1","Dec-Feb",.) %>%
           gsub("Q2","Mar-Mai",.) %>%
           gsub("Q3","Jun-Aug",.) %>%
           gsub("Q4","Sep-Nov",.)) %>% 
#  slice_sample(n=100000) %>%
  pivot_wider(names_from = Pollutant, values_from = Concentration) %>% 
  ggplot(aes(x = NO2, y = O3)) +
  xlab("NO2 daily average [µg/m³]") + ylab("O3 daily average [µg/m³]") +
  geom_point(aes(color = time),shape=".") +
  #geom_line(aes(color = time, group = Station)) +
  scale_color_viridis(discrete=FALSE, option="viridis", labels=as.Date)+
  facet_grid(rows=vars(Category),cols=vars(Quartal))+
  coord_cartesian(xlim = c(0, 80), ylim = c(0, 160))

ggsave(
  "O3_NO2_Korrelation_Tagesmittel.png",
  width = 20,
  height = 10,
  dpi = 300
)
