library(tidyverse)
library(readr)
library(viridis)

no2 <- read_delim(
  "data/no2.txt",
  delim = ";",
  escape_double = FALSE,
  trim_ws = TRUE,
  locale = locale(decimal_mark = ",")
) %>%
  select(where(function(x)
    ! all(is.na(x)))) %>% 
  mutate(Stoff="NO2")

o3 <- read_delim(
  "data/o3.txt",
  delim = ";",
  escape_double = FALSE,
  trim_ws = TRUE,
  locale = locale(decimal_mark = ",")
) %>%
  select(where(function(x)
    ! all(is.na(x)))) %>% 
  mutate(Stoff="O3")

beide <- bind_rows(no2, o3) %>% 
  pivot_longer(-c(Station, Stoff), names_to="Jahr", values_to="Wert")%>% 
  filter(!is.na(Wert)) %>% 
  mutate(Jahr=as.numeric(Jahr))

beide %>% 
  ggplot(aes(x=Jahr, y=Wert, group=Station))+
  geom_line(aes(group=Station))+
  facet_wrap(vars(Stoff), scales="free_y")

ggsave(
  "O3_NO2_Zeitverlauf.png",
  width = 20,
  height = 10,
  dpi = 300
)

beide %>%
  pivot_wider(names_from = Stoff, values_from = Wert) %>%
  ggplot(aes(x = NO2, y = O3)) +
  xlab("NO2 Jahresmittelwert") + ylab("O3 Jahresmittelwert") +
  geom_point(aes(color = factor(Jahr))) +
  geom_line(aes(color = factor(Jahr), group = Station)) +
  scale_color_viridis(discrete=TRUE, option="viridis")+
  coord_cartesian(xlim = c(0, 40), ylim = c(25, 85))

ggsave(
  "O3_NO2_Korrelation.png",
  width = 20,
  height = 10,
  dpi = 300
)
