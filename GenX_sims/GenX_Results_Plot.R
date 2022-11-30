library(tidyverse)
library(lubridate)
library(patchwork)

sim_folder <- "sim1.3 - geothermal added"
results_folder <- "Results12"

shw_per <- 6
tdr <- TRUE

rep_per_map <- read_csv(paste(sim_folder, 
                              "TDR_Results/Period_map.csv", 
                              sep = "/"
                              ), col_types = cols(.default = "i")
                        ) %>%
  mutate(Representitive = Period_Index == Rep_Period) %>%
  select(-Rep_Period)

ts_per_period <-  read_csv(paste(sim_folder, 
                                 "TDR_Results/Load_data.csv", 
                                 sep = "/"))$Timesteps_per_Rep_Period[1]

plt_rep_pers <- read_csv(paste(sim_folder, "Load_data.csv", sep = "/")) %>%
  mutate(Period_Index = Time_Index %/% ts_per_period + 1) %>%
  select(Time_Index, Load_MW_z1, Period_Index) %>%
  right_join(rep_per_map) %>%
  filter(Time_Index %% 12 == 0) %>%
  mutate(Time_Index = ymd_hm("2025-01-01 00:00") + minutes(5 * (Time_Index - 1))) %>%
    mutate(Rep_Period_Index = as.factor(Rep_Period_Index)) %>%
    select(-Period_Index) %>%
  ggplot() +
  geom_col(aes(
    x = Time_Index,
    y = Load_MW_z1,
    fill = Rep_Period_Index),
    # width = 0.5,
    alpha = 0.35 + 0.65 * plt_rep_pers$Representitive) + 
  scale_fill_brewer(palette = "Spectral", name = "Period Bin") + 
  labs(x = NULL, y = "Load (MW)", legend) +
  theme(panel.background = element_rect(fill = "gray20"),
        panel.grid = element_line(color = "gray30"))

capacity <- read_csv(paste(sim_folder, 
                                results_folder, 
                                "capacity.csv", 
                                sep = "/"
                                )
                     )

plt_cap_col <- capacity %>%
  ggplot(aes(x = Resource, y = EndCap, fill = Resource)) + 
    geom_col() + 
    labs(y = "Capacity (MW)",
         x = NULL,
         fill = NULL,
         title = "Installed Capacity"
    ) + 
    scale_fill_viridis_d() +
    theme(axis.text.x = element_text(angle = 20, hjust = 1, vjust = 1),
          axis.ticks.x = element_blank(),
          axis.ticks.y.right = element_line(),
          axis.title.y.right = element_text(size = 9),
          axis.text.y.right = element_text(size = 8),
          legend.position = "none",
          title = element_text(size = 10)) + 
  geom_text(aes(label = round(EndCap,2)), vjust = -0.3)

total_stor = sum(capacity$EndEnergyCap)

bat_energy = capacity %>%
  filter(Resource == 'battery4' | Resource == "battery8") %>%
  summarize(total = sum(EndEnergyCap)) %>%
  pull(total)

ph_energy = capacity %>%
  filter(Resource == 'pumped_hydro_long' | Resource == "pumped_hydro_short") %>%
  summarize(total = sum(EndEnergyCap)) %>%
  pull(total)

per_ind <- rep_per_map %>%
  filter(Representitive == TRUE, Rep_Period_Index == shw_per) %>%
  pull(Period_Index)

tstart <- if (tdr) {shw_per} else {per_ind}
  
power <- read_csv(paste(sim_folder, 
                            results_folder, 
                            "power.csv", 
                            sep = "/"
                            )
                      ) %>%
  slice(3:n()) %>%
  slice(((tstart - 1) * ts_per_period + 1):(tstart * ts_per_period - 1)) %>%
  select(-Total)

charge <- read_csv(paste(sim_folder, 
                         results_folder, 
                         "charge.csv", 
                         sep = "/"
                         )
                   ) %>%
  slice(3:n()) %>%
  slice(((tstart - 1) * ts_per_period + 1):(tstart * ts_per_period - 1)) %>%
  select(Resource, battery4, battery8, pumped_hydro_short, pumped_hydro_long)

price <- read_csv(paste(sim_folder, 
                        results_folder, 
                        "prices.csv", 
                        sep = "/"
                        )
                  ) %>%
  slice(3:n()) %>%
  slice(((tstart - 1) * ts_per_period + 1):(tstart * ts_per_period - 1)) %>%
  rename(LMP = `1`) %>%
  mutate(Per = row_number(),
         Date = ymd_hm("2025-01-01 00:00") + minutes(5*((per_ind-1) * ts_per_period + Per)),
         LMP = 12*LMP) %>%
  select(Date, LMP)


soc <- read_csv(paste(sim_folder, 
                        results_folder, 
                        "storage.csv", 
                        sep = "/"
                      )
                ) %>%
  slice(3:n()) %>%
  slice(((tstart - 1) * ts_per_period + 1):(tstart * ts_per_period - 1)) %>%
  mutate(Per = row_number(),
         Date = ymd_hm("2025-01-01 00:00") + minutes(5*((per_ind-1) * ts_per_period + Per))) %>%
  mutate(total_soc = (battery4 + battery8 + pumped_hydro_long + pumped_hydro_short)/total_stor) %>%
  select(Date, battery4, battery8, pumped_hydro_long, pumped_hydro_short, total_soc)

power_net <- power %>%
  left_join(charge, by = "Resource", suffix = c("", "_c")) %>%
  mutate(Per = row_number(),
         Date = ymd_hm("2025-01-01 00:00") + minutes(5*((per_ind-1) * ts_per_period + Per))) %>%
  select(-Resource, -Per) %>%
  mutate(battery4 = battery4 - battery4_c,
         battery8 = battery8 - battery8_c,
         pumped_hydro_long = pumped_hydro_long - pumped_hydro_long,
         pumped_hydro_short = pumped_hydro_short - pumped_hydro_short_c
         ) %>%
  select(-battery4_c, -battery8_c, -pumped_hydro_short_c, -pumped_hydro_long_c)

  

plt_power <- power_net %>%
  pivot_longer(cols = -c("Date"), 
               names_to = "Resource", 
               values_to = "Gen") %>%
  mutate(Resource = factor(Resource, levels = c(
    "battery4",
    "battery8",
    "pumped_hydro_long",
    "pumped_hydro_short",
    "onshore_wind",
    "solar_btm_curt",
    "solar_btm",
    "solar_pv_sat",
    "hydro",
    "geothermal"
  ))) %>%
  filter(Resource %in% capacity[capacity$EndCap > 0,]$Resource) %>%
  mutate(Resource = fct_drop(Resource)) %>%
  ggplot() + 
  geom_area(aes(x = Date, y = Gen, fill = Resource), alpha = 0.75) + 
  labs(x = NULL, y = "Generator Output (MW)", title = "Dispatch Summary") +
  scale_fill_viridis_d() + 
  theme(panel.background = element_rect(fill = "gray40"),
        panel.grid = element_line(color = "gray50"),
        legend.position = "none")

plt_price <- price %>%
  ggplot() + 
    geom_line(aes(x = Date, y = LMP)) + 
    labs(x = NULL) + 
    scale_y_continuous(breaks = range(signif(price$LMP,2))) + 
    theme(axis.text.x = element_blank(),
          axis.ticks.x = element_blank(),
          axis.title.y.right = element_text(size = 10),
          panel.background = element_blank(),
          aspect.ratio = 1/20)

plt_soc <- soc %>%
  ggplot() + 
  geom_line(aes(x = Date, y = total_soc)) + 
  labs(x = NULL, y = "SOC") + 
  scale_y_continuous(breaks = round(range(soc$total_soc),digits = 2)) + 
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.title.y.right = element_text(size = 10),
        panel.background = element_blank(),
        aspect.ratio = 1/20)

plt_cap <- capacity %>%
  filter(Resource != "Total") %>%
  mutate(Resource = factor(Resource, levels = c(
    "battery4",
    "battery8",
    "pumped_hydro_long",
    "pumped_hydro_short",
    "onshore_wind",
    "solar_btm_curt",
    "solar_btm",
    "solar_pv_sat",
    "hydro",
    "geothermal"
  ))) %>%
  filter(EndCap > 0.0) %>%
  mutate(Resource = fct_drop(Resource)) %>%
  ggplot(aes(x = "Installed_Capacity", y = EndCap, fill = Resource)) +
  geom_bar(position = "stack",
           stat = "identity"
           ) +
  labs(y = "Installed Capacity",
       x = NULL,
       fill = NULL
       ) + 
  scale_fill_viridis_d() +
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y.right = element_line(),
        axis.title.y.right = element_text(size = 9),
        axis.text.y.right = element_text(size = 8),
        panel.background = element_blank(),
        aspect.ratio = 20,
        legend.position = "left",
        title = element_text(size = 10))

(plt_power / plt_price / plt_soc) | plt_cap

# plt_power %>%
#   ggplot() +
#   geom_area(aes(x = Date, y = Gen, fill = Resource)) +
#   facet_wrap(. ~ Resource, ncol = 3) +
#   coord_polar()
# 
# plt_power %>%
#   filter(Gen > 1000) %>%
#   pivot_wider(names_from = Resource, values_from = Gen) %>%
#   select(hydro, solar_pv_sat, battery4, battery8) %>%
#   pairs()
  
# plt_rep_pers
