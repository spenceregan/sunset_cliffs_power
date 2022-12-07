library(tidyverse)
library(lubridate)
library(patchwork)
library(ggridges)

setwd("~/Repos/CU/Future_Power_Grid/Project2/GenX_sims")

sim_folder <- "sim1.4_final"
results_folder <- "Results_full"

shw_per <- 8
tdr <- FALSE

periods <- 2016


rep_per_map <- read_csv(paste(sim_folder, 
                              "TDR_Results/Period_map.csv", 
                              sep = "/"
                              ), col_types = cols(.default = "i")
                        ) %>%
  mutate(Representitive = Period_Index == Rep_Period) %>%
  select(-Rep_Period)

ts_per_period <- 2016

rep_pers <- read_csv(paste(sim_folder, "Load_data.csv", sep = "/")) %>%
  mutate(Period_Index = Time_Index %/% ts_per_period + 1) %>%
  select(Time_Index, Load_MW_z1, Period_Index) %>%
  right_join(rep_per_map) %>%
  filter(Time_Index %% 12 == 0) %>%
  mutate(Time_Index = ymd_hm("2025-01-01 00:00") + minutes(5 * (Time_Index - 1))) %>%
    mutate(Rep_Period_Index = as.factor(Rep_Period_Index)) %>%
    select(-Period_Index)
  
plt_rep_pers <- rep_pers %>%  
  ggplot() +
  geom_col(aes(
    x = Time_Index,
    y = Load_MW_z1,
    fill = Rep_Period_Index),
    # width = 0.5,
    alpha = 0.35 + 0.65 * rep_pers$Representitive) + 
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

plt_cap_col

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

ordc <- read_csv(paste(sim_folder, results_folder, "ReserveMargin_w.csv", sep = "/")) %>%
  rename(shdw = "CapRes_1") %>%
  slice(((tstart - 1) * ts_per_period + 1):(tstart * ts_per_period - 1)) %>%
  mutate(Per = row_number(),
         Date = ymd_hm("2025-01-01 00:00") + minutes(5*((per_ind-1) * ts_per_period + Per))) %>%
  select(Date, shdw)

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
  labs(x = NULL, y = "Generator Output (MW)", title = paste("Weeklong Dispatch Summary - Period ", shw_per)) +
  scale_fill_viridis_d() + 
  theme(panel.background = element_rect(fill = "gray40"),
        panel.grid = element_line(color = "gray50"),
        legend.position = "right")

plt_price <- price %>%
  ggplot() + 
    geom_line(aes(x = Date, y = LMP)) + 
    labs(x = NULL, y = "LMP") + 
    scale_y_continuous(breaks = range(signif(price$LMP,2))) + 
    theme(axis.text.x = element_blank(),
          axis.ticks.x = element_blank(),
          axis.title.y = element_text(size = 9),
          panel.background = element_blank(),
          aspect.ratio = 1/20)

plt_soc <- soc %>%
  ggplot() + 
  geom_line(aes(x = Date, y = total_soc)) + 
  labs(x = NULL, y = "SOC") + 
  scale_y_continuous(breaks = round(range(soc$total_soc),digits = 2)) + 
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.title.y = element_text(size = 9),
        panel.background = element_blank(),
        aspect.ratio = 1/20)

plt_shdw <- ordc %>%
  ggplot() + 
  geom_line(aes(x = Date, y = shdw)) + 
  labs(x = NULL, y = "RA $") + 
  scale_y_continuous(trans = "log1p", 
                     breaks = round(signif(range(ordc$shdw),2))) + 
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.title.y = element_text(size = 9),
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
           stat = "identity",
           show.legend = FALSE
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
        aspect.ratio = 30,
        legend.position = "none",
        title = element_text(size = 10))

((plt_power / plt_price / plt_soc / plt_shdw) | plt_cap) 

plt_power

net_revenue <- read_csv(paste(sim_folder, 
                              results_folder, 
                              "NetRevenue.csv", 
                              sep = "/"
                              )) %>%
  mutate(
    `Investment Cost` = Inv_cost_MW + Inv_cost_MWh,
    `O&M Cost` = Fixed_OM_cost_MW + Fixed_OM_cost_MWh + Var_OM_cost_in + Var_OM_cost_out,
    ) %>%
  rename(`Charging Cost` = Charge_cost,
         `RA Revenue` = ReserveMarginRevenue,
         `Energy Revenue` = EnergyRevenue) %>%
  select(
    Resource,
    `Investment Cost`,
    `O&M Cost`,
    `Charging Cost`,
    `RA Revenue`,
    `Energy Revenue`
    ) %>%
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
  pivot_longer(cols = -Resource, 
               names_to = "CostRevenue", 
               values_to = "AnnualTotal"
               ) %>%
  mutate(AnnualTotal = AnnualTotal * if_else(CostRevenue %in% c("Energy Revenue", "RA Revenue"), 1, -1)) %>%
  mutate(CostRevenue = factor(CostRevenue, levels = c(
    "RA Revenue",
    "Energy Revenue",
    "O&M Cost",
    "Investment Cost",
    "Charging Cost"
  )),  AnnualTotal = AnnualTotal / 1000000) %>%
  ggplot(aes(x = Resource, y = AnnualTotal, fill = CostRevenue)) + 
    geom_bar(position = "stack", stat = "identity") + 
    labs(x = NULL,
       fill = NULL, 
       y = "Annual Total ($MM)",
       title = "Generator Costs & Revenues"
       ) + 
    # scale_y_continuous(trans = "log", 
    #                    breaks = scales::breaks_log(),
    #                    limits = c(1, NA)) + 
  theme(axis.text.x = element_text(angle = 20, hjust = 1, vjust = 1))

plt_cap_hor <- capacity %>%
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
  ggplot(aes(y = "Installed_Capacity", x = EndCap, fill = Resource)) +
  geom_bar(position = "stack",
           stat = "identity"
  ) +
  labs(x = "Total Installed Capacity",
       y = NULL,
       fill = NULL
  ) + 
  scale_x_continuous(trans = "reverse") + 
  scale_fill_viridis_d() +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.ticks.x.bottom = element_line(),
        axis.title.x.bottom = element_text(size = 9),
        axis.text.x.bottom = element_text(size = 8),
        panel.background = element_blank(),
        aspect.ratio = 1/20,
        legend.position = "none",
        legend.key.size = unit(0.25, "cm"),
        title = element_text(size = 10))

# Rate Calculation with TDR Data

# tdr_demand <- read_csv(paste(sim_folder, "TDR_Results/Load_data.csv", sep = "/")) %>%
#   rename(load = Load_MW_z1) %>%
#   select(Time_Index, load) %>%
#   mutate(Rep_Period_Index = (Time_Index - 1) %/% periods + 1,
#          Period_Time_Index = (Time_Index - 1) %% periods) %>%
#   select(-Time_Index)

tdr_lmp <- read_csv(paste(sim_folder, results_folder, "prices.csv", sep = "/")) %>%
  rename(lmp = `1`) %>%
  select(lmp) %>%
  mutate(Time_Index = row_number())

tdr_shdw <- read_csv(paste(sim_folder, results_folder, "ReserveMargin_w.csv", sep = "/")) %>%
  rename(shdw = "CapRes_1") %>%
  select(shdw) %>%
  mutate(Time_Index = row_number())
    
demand_summary <- read_csv(paste(sim_folder, "Load_data.csv", sep = "/")) %>%
  rename(load = "Load_MW_z1") %>%
  select(Time_Index, load) %>%
  left_join(tdr_demand) %>%
  left_join(tdr_lmp) %>%
  left_join(tdr_shdw) %>%
  mutate(Date = ymd_hm("2025-01-01 00:00") + minutes(5*(row_number()-1)),
         month = month(Date, label = T, abbr = T),
         hour = hour(Date)) %>%
  group_by(month, hour) %>%
  summarise(sum_demand = sum(load), 
            avg_lmp = 12 * mean(lmp),
            sum_shdw = sum(shdw))



price_profile <- read_csv(paste(sim_folder, "Load_data.csv", sep = "/")) %>%
  rename(load = "Load_MW_z1") %>%
  select(Time_Index, load) %>%
  left_join(tdr_demand) %>%
  left_join(tdr_lmp) %>%
  left_join(tdr_shdw) %>%
  mutate(shdw_cost = shdw*load)
  
shdw_sum <- sum(price_profile$shdw_cost)

# (ridges_month_lmp <- demand_summary %>%
#   ggplot() + 
#   stat_density_ridges(geom = "density_ridges_gradient", 
#                       calc_ecdf = T,
#                       aes(x = 12 * avg_lmp, 
#                           y = month, 
#                           fill = 0.5 - abs(0.5 - stat(ecdf)),
#                           )
#                       ) + 
#     scale_fill_viridis_c(name = "Tail probability", direction = -1) +
#     labs(x = "LMP ($/MWh)", y = NULL, fill = NULL))
# 
# (ridges_hour_lmp <- demand_summary %>%
#     filter(month %in% c("Jul", "Aug", "Sep")) %>%
#     ggplot() + 
#     geom_density_ridges(alpha = 0.95, 
#                         aes(x = 12 * avg_lmp, 
#                             y = fct_rev(as.factor(hour)), 
#                             fill = abs(hour - 12)
#                             ),
#                         linetype = 0,
#                         bandwidth = 1
#                         ) + 
#     scale_fill_viridis_c(name = "Tail probability", direction = -1) +
#     labs(x = "LMP ($/MWh)", y = "hour", fill = NULL) +
#     theme(legend.position = "none"))
# 
(ridges_month_lmp <- demand_summary %>%
    ggplot(aes(x = hour, y = fct_rev(as.factor(month)))) +
    geom_tile(aes(fill = 12 * avg_lmp), alpha = 0.75) +
    labs(x = "hour of day", y = "month", fill = NULL, title = "Average LMP") +
    scale_fill_viridis_c(name = "($/MWh)"))

(tile_shdw_sum <- demand_summary %>%
    ggplot(aes(x = hour, y = fct_rev(as.factor(month)))) +
    geom_tile(aes(fill = sum_shdw), alpha = 0.75) +
    labs(x = "hour of day", y = "month", fill = NULL,
         title = "RA Price Summary") +
    scale_fill_viridis_c(name = "RA Price Sum ($/MW)"))

(july_summary <- read_csv(paste(sim_folder, "Load_data.csv", sep = "/")) %>%
  rename(load = "Load_MW_z1") %>%
  select(Time_Index, load) %>%
  left_join(tdr_demand) %>%
  left_join(tdr_lmp) %>%
  left_join(tdr_shdw) %>%
  mutate(Date = ymd_hm("2025-01-01 00:00") + minutes(5*(row_number()-1)),
         month = month(Date, label = T, abbr = T),
         day = day(Date),
         time = hms::as_hms(Date)) %>%
  filter(month == "Jul")) %>%
  ggplot(aes(x = day, y = time)) +
  geom_tile(aes(fill = shdw), alpha = 0.75) +
  labs(x = "day", y = "time", fill = NULL, title = "July RA Shadow Prices") +
  scale_fill_viridis_c(name = "RA Price ($/MW)")

curtailment <- read_csv(paste(sim_folder, results_folder, "curtail.csv", sep = "/")) %>%
  slice(3:n()) %>%
  mutate(Date = ymd_hm("2025-01-01 00:00") + minutes(5*(row_number()-1)),
         month = month(Date, label = T, abbr = T),
         day = day(Date),
         hour = hour(Date))

(tile_curtail <- curtailment %>%
  group_by(month, hour) %>%
  summarise(curt_total = sum(Total) / 12,
            curt_wind = sum(onshore_wind) / 12,
            curt_solar = sum(solar_pv_sat) / 12
            ) %>%
  ggplot(aes(x = hour, y = fct_rev(as.factor(month)))) +
  geom_tile(aes(fill = curt_total), alpha = 0.75) +
  labs(x = "hour", y = "month", fill = "Curtailment (MWh)", title = "Sum of Curtailed Generation") +
  scale_fill_viridis_c())

(tile_curtail_wind <- curtailment %>%
    group_by(month, hour) %>%
    summarise(curt_total = sum(Total) / 12,
              curt_wind = sum(onshore_wind) / 12,
              curt_solar = sum(solar_pv_sat) / 12
    ) %>%
    ggplot(aes(x = hour, y = fct_rev(as.factor(month)))) +
    geom_tile(aes(fill = curt_wind), alpha = 0.75) +
    labs(x = "hour", y = "month", fill = "Curtailment (MWh)", title = "Sum of Curtailed Wind Generation") +
    scale_fill_viridis_c())

(tile_curtail_solar <- curtailment %>%
    group_by(month, hour) %>%
    summarise(curt_total = sum(Total) / 12,
              curt_wind = sum(onshore_wind) / 12,
              curt_solar = sum(solar_pv_sat) / 12
    ) %>%
    ggplot(aes(x = hour, y = fct_rev(as.factor(month)))) +
    geom_tile(aes(fill = curt_solar), alpha = 0.75) +
    labs(x = "hour", y = "month", fill = "Curtailment (MWh)", title = "Sum of Curtailed Solar Generation") +
    scale_fill_viridis_c())

# 
# avail <- read_csv(paste(sim_folder, "TDR_Results/Generators_variability.csv", sep = "/")) %>%
#   mutate(VRE_avail = (onshore_wind + solar_pv_sat) / 2)
# 
# (capres <- read_csv(paste(sim_folder, results_folder, "ReserveMargin_w.csv", sep = "/")) %>%
#   rename(shdw = "CapRes_1") %>%
#   mutate(Time_Index = row_number()) %>%
#   select(Time_Index, shdw) %>%
#   inner_join(avail) %>%
#   inner_join(demand) %>%
#   mutate(net_demand = load * (1-VRE_avail))) %>%
#   ggplot() + 
#   geom_point(aes(x = net_demand, y = shdw))
# 
# capres <- read_csv(paste(sim_folder, results_folder, "ReserveMargin_w.csv", sep = "/")) %>%
#   rename(shdw = "CapRes_1") %>%
#   mutate(Time_Index = row_number()) %>%
#   select(Time_Index, shdw)
  
 
net_revenue / plt_cap_hor

# plt_rep_pers

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

