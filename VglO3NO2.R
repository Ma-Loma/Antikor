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
  gsub("Stadt","Urban Background",.) %>%
  gsub("Vorort","Suburban",.)

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
  left_join(Stationsnamen_Kategorie, by="Station") %>% 
  mutate(decade = round(year / 10) * 10)

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

decline_fct <- function(df) {
  tryCatch({
    nls(O3 ~ a / NO2 + b,
        data = df,
        start = list(a = 180.1, b = 40))
  }, error = function(e)
    NA)
}

# Perform nonlinear fit per group
fit_decline <- function(df) {
  df%>%
    group_modify(~ {
      model <- decline_fct(.x)
      if (length(model)>1) {
        new_data <- tibble(NO2 = seq(min(.x$NO2, na.rm = TRUE),
                                     max(.x$NO2, na.rm = TRUE), length.out = 100))
        new_data$O3_fit <- predict(model, newdata = new_data)
        new_data$a<-coef(model)["a"]
        new_data$b<-coef(model)["b"]
        return(new_data)
      } else {
        return(tibble(NO2 = numeric(), O3_fit = numeric()))
      }
    })
}

jahresmittel_grouped_wide <- beideJahresmittel %>%
  pivot_wider(names_from = Pollutant, values_from = Concentration) %>%
  mutate(year_cat=as.character(year)) %>%
  rbind(., mutate(., Category = "All")) %>%
  rbind(., mutate(., year_cat = "All")) %>%
  group_by(Category, year_cat)

jahres_fits <- jahresmittel_grouped_wide %>%
 fit_decline()
fitted_yearly_parameters <-
  jahres_fits %>%
  select(!c(NO2, O3_fit)) %>%
  unique()

plJahresmittel <- function(df,df_fits) {
  df %>% 
    ggplot(aes(x = NO2, y = O3)) +
    xlab("NO2 yearly average") + ylab("O3 yearly average") +
    geom_point(aes(color = year,shape = Category)) +
    geom_line(aes(color = year, group = Station)) +
    geom_line(data=df_fits,aes(x=NO2,y=O3_fit,group = interaction(year_cat,Category),linetype=Category),inherit.aes = FALSE) +
    scale_color_viridis(discrete=FALSE, option="viridis")+
    coord_cartesian(xlim = c(0, 40), ylim = c(30, 85))+
    scale_shape_manual(values = c(2:6))
}

pl2<-jahresmittel_grouped_wide %>% 
#  filter(Category=="All"|year_cat=="All") %>% 
  mutate(O3=if_else(is.na(O3)&Category=="Urban Traffic",30,O3)) %>%
  mutate(O3=if_else(is.na(O3)&Category=="Urban Background",31,O3)) %>%
  plJahresmittel(.,jahres_fits)

pl2+
  facet_wrap(vars(year_cat))

ggsave(
  "O3_NO2_Korrelation_Jahresmittel.png",
  width = 20,
  height = 10,
  dpi = 300
)

fitted_yearly_parameters %>% 
  ggplot(aes(x = a, y = b,color=as.numeric(year),shape=Category)) +
  scale_color_viridis(discrete=FALSE, option="viridis")+
  geom_point()+
  geom_path(aes(group=Category))

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


tageswerte_grouped_wide<-tageswerteLong %>%
  mutate(Quartal = lubridate::quarter(time, fiscal_start = 12) %>%
           paste0("Q", .)) %>%
  mutate(
    Quartal = Quartal %>%
      gsub("Q1", "Dec-Feb", .) %>%
      gsub("Q2", "Mar-Mai", .) %>%
      gsub("Q3", "Jun-Aug", .) %>%
      gsub("Q4", "Sep-Nov", .)
  ) %>%
  mutate(Quartal = factor(Quartal, levels = c(
    "Dec-Feb", "Mar-Mai", "Jun-Aug", "Sep-Nov"
  ))) %>%
  #  slice_sample(n=100000) %>%
  pivot_wider(names_from = Pollutant, values_from = Concentration) %>%
  rbind(., mutate(., Category = "All")) %>%
  rbind(., mutate(., Quartal = "All")) %>%
  group_by(Category, Quartal)

tages_fits <- tageswerte_grouped_wide %>%
  fit_decline()
fitted_daily_parameters <-
  tages_fits %>%
  select(!c(NO2, O3_fit)) %>%
  unique()  
  
  
tageswerte_grouped_wide %>%
  ggplot(aes(x = NO2, y = O3)) +
  xlab("NO2 daily average [µg/m³]") + ylab("O3 daily average [µg/m³]") +
  geom_point(aes(color = time), shape = ".") +
  geom_line(data = tages_fits, color = "red", aes(x = NO2, y = O3_fit)) +
  scale_color_viridis(discrete = FALSE,
                      option = "viridis",
                      labels = as.Date) +
  facet_grid(rows = vars(Category), cols = vars(Quartal)) +
  coord_cartesian(xlim = c(0, 80), ylim = c(0, 160))

ggsave(
  "O3_NO2_Korrelation_Tagesmittel.png",
  width = 20,
  height = 10,
  dpi = 300
)
