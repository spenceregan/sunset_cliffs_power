library(tidyverse)
library(lubridate)
library(zoo)
library(chron)

setwd("~/Repos/CU/Future_Power_Grid/Project2/Load Profiles/Net_Load_Profile")

SCPA_SDGE_ratio = 0.01924
sdge_2022_annual_sales = 21300*1000



ev_daily_load <- read_csv('ev_5min.csv',
                          col_types = cols(
                            Time = col_datetime(format = "%D %R"),
                            Weekday = "n",
                            Weekend = "n"
                          )
                          ) %>%
  mutate(Time = as.numeric(Time - Time[1]) / 60.) %>%
  pivot_longer(cols = -Time, names_to = "Weekday", values_to = "EV_load") %>%
  mutate(Weekday = Weekday == "Weekday")

dg_gen <- read_csv('dg_5min.csv',
                   col_types = cols(
                     Datetime = col_datetime(format = "%D %R"),
                     Gen = "n"
                     )
                   )

provided_load <- read_csv('provided_load_profile.csv',
                          col_types = cols(
                            Datetime = col_datetime(format = "%D %R"), 
                            `Load (MW)` = "n"
                          ),
                          locale = locale(tz = "UTC")) %>%
  filter(year(Datetime) == 2022) %>%
  mutate(Time = 60 * hour(Datetime) + minute(Datetime),
         Weekday = wday(Datetime) > 1 & wday(Datetime) < 7) %>%
  rename(Load = `Load (MW)`)

p_annual_sales <- sum(provided_load$`Load (MW)`) / 12.0

SCPA_SDGE_ratio <- p_annual_sales / sdge_2022_annual_sales

net_load <- provided_load %>%
  left_join(dg_gen) %>%
  inner_join(ev_daily_load) %>%
  mutate(net_load = Load - Gen + SCPA_SDGE_ratio * EV_load / 1000) %>%
  select(net_load) %>%
  write_csv('net_load_MW_5min.csv')
