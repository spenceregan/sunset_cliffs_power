library(tidyverse)
library(lubridate)

setwd("~/Repos/CU/Future_Power_Grid/Project2/Load Profiles")

SCPA_SDGE_ratio = 0.01924

sdge <- read.csv('Large_Data/SDGE_Interconnected_Project_Sites_2022-10-31.csv', header = TRUE)

sdge_cumulative <- sdge %>%
    mutate(App.Approved.Date = ymd(App.Approved.Date),
           App.Approved.Year = year(App.Approved.Date)) %>%
    mutate(Azimuth = as.numeric(Azimuth)) %>%
    filter(Technology.Type == 'Solar', App.Approved.Date > "2012-12-31") %>%
    arrange(App.Approved.Date) %>%
    mutate(Orientation = case_when(Azimuth > 225 ~ "West",
                                   Azimuth >135 & Azimuth <= 225 ~ "South",
                                   Azimuth <= 135 ~ "East",
                                   TRUE ~ "South")) %>%
    group_by(Orientation) %>%
    mutate(`Capacity AC` = SCPA_SDGE_ratio * cumsum(System.Size.AC) / 1000) %>%
    ungroup() %>%
    select(App.Approved.Date, Orientation, `Capacity AC`)

sdge_cumulative_t <- sdge %>%
  mutate(App.Approved.Date = ymd(App.Approved.Date),
         App.Approved.Year = year(App.Approved.Date)) %>%
  mutate(Azimuth = as.numeric(Azimuth)) %>%
  filter(Technology.Type == 'Solar', App.Approved.Date > "2012-12-31") %>%
  arrange(App.Approved.Date) %>%
  mutate(`Capacity AC` = SCPA_SDGE_ratio * cumsum(System.Size.AC) / 1000) %>%
  select(App.Approved.Date, `Capacity AC`)

capacity_regr <- data.frame(
  App.Approved.Date = as_date(rep(c("2018-01-01", "2025-01-01"), 3)),
  Orientation = rep(c("East", "South", "West"), 2))
m1 <- lm(`Capacity AC` ~ App.Approved.Date*factor(Orientation), 
         data = filter(sdge_cumulative, App.Approved.Date > "2017-12-31"))
proj <- predict.lm(m1, capacity_regr)
capacity_regr$`Capacity AC` <- predict.lm(m1, capacity_regr)

(cum_plot <- sdge_cumulative %>%
    ggplot(aes(x = App.Approved.Date,
               y = `Capacity AC`,
               color = Orientation)) +
    geom_line() +
  geom_line(data = capacity_regr, linetype = "dashed")+
  geom_text(data = capacity_regr[c(2,4,6),],
            aes(label = paste(round(`Capacity AC`, 2), "MW")),
            nudge_y = 0.75, nudge_x = -1) + 
    labs(x = "Install Date",
         y = "Capacity AC (MW)",
         title = "SCPA Projected 2025 DG Solar Capacity") + 
  xlim(as_date(c("2018-01-01", "2025-06-01"))))
  

cum_plot


sdge %>%
  filter(Technology.Type == 'Solar', App.Approved.Date > "2012-12-31") %>%
  mutate(Azimuth = as.numeric(Azimuth)) %>%
  ggplot() +
  geom_density(adjust = 0.05, aes(x = Azimuth)) +
  # coord_polar() +
  scale_x_continuous(breaks = c(0, 90, 180, 270))

sdge %>%
  filter(Technology.Type == 'Solar', App.Approved.Date > "2012-12-31") %>%
  mutate(Tilt = as.numeric(Tilt)) %>%
  filter(Tilt < 45) %>%
  ggplot() +
  geom_density(adjust = 1, aes(x = Tilt)) +
  scale_x_continuous(breaks = c(0, 10, 20, 30, 40))

azi_summary <- sdge %>%
  filter(Technology.Type == 'Solar', App.Approved.Date > "2012-12-31") %>%
  mutate(Azimuth = as.numeric(Azimuth)) %>%
  mutate(Orientation = case_when(Azimuth > 225 ~ "West",
                                 Azimuth >135 & Azimuth <= 225 ~ "South",
                                 Azimuth <= 135 ~ "East",
                                 is.na(Azimuth) ~ "South")) %>%
  group_by(Orientation) %>% 
  summarise(SCPA_MW_ac = SCPA_SDGE_ratio * sum(System.Size.AC) / 1000)
show(azi_summary)
