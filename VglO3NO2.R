library(tidyverse)
library(readr)

no2 <- read_delim(
  "data/no2.txt",
  delim = ";",
  escape_double = FALSE,
  trim_ws = TRUE
) %>%
  select(where(function(x)
    ! all(is.na(x)))) %>% 
  mutate(Stoff="NO2")


o3 <- read_delim(
  "data/o3.txt",
  delim = ";",
  escape_double = FALSE,
  trim_ws = TRUE
) %>%
  select(where(function(x)
    ! all(is.na(x)))) %>% 
  mutate(Stoff="O3")

beide <- bind_rows(no2, o3) %>% 
  pivot_longer(-c(Station, Stoff), names_to="Jahr", values_to="Wert")%>% 
  filter(!is.na(Wert))



beide %>% 
  ggplot(aes(x=Jahr, y=Wert, group=Station,color=Stoff))+
  geom_point()

beide %>% 
  pivot_wider(names_from=Stoff, values_from=Wert) %>%
  ggplot(aes(x=NO2, y=O3,color=Station)) + 
  xlab("NO2 Jahresmittelwert") + ylab("O3 Jahresmittelwert") +
  geom_line() +
  coord_cartesian(xlim=c(0,400),ylim = c(0, 850))

ggsave("O3_NO2_Korrelation.png", width=20, height=10, dpi=300)
  