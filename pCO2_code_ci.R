# load libraries
library(tidyverse) # for data processing
library(openxlsx) # for writing to excel file
library(tidybayes) # for credible intervals
library(here) # for file structure

# Data preparation --------------------------------------------------------


# load data
sol_dat <- readxl::read_excel(here("Data",
                                   "Input", 
                                   "paleosol_data.xlsx"))

# clean column names
sol_dat <- sol_dat %>% 
  # remove empty columns
  select(-c(5, 10)) %>% 
  # rename columns
  rename("d13C_atm_min" = "d13C atm", 
         "d13C_atm_max" = "...4", 
         "soil_temp_min" = "T",
         "soil_temp_max" = "...8", 
         "S_z_min" = "S(z)", 
         "S_z_max" = "...12") %>% 
  # remove empty rows
  drop_na(Sample) %>% 
  # increase Sz_max to 2000
  mutate(S_z_max = 2000)



# pCO2 calculation --------------------------------------------------------



# calculate distributions of possible 
# combinations
sol_dat_dist <- sol_dat %>% 
  mutate(
    # set up temperature range
    temp_range = map2(soil_temp_min, soil_temp_max, seq, by = 1), 
    # calculate d13Cs
    d13Cs = map2(d13Ccarb, temp_range, 
                 ~ .x - (11.98 - 0.12 * .y)),
    # set up atmospheric d13C range
    d13Ca_range = map2(d13C_atm_min, d13C_atm_max, seq, by = 0.1), 
    # set up Sz range, 
    S_z_range = map2(S_z_min, S_z_max, seq, by = 100)) %>% 
  select(Sample, S_z_range, d13Cs, d13Cresp, d13Ca_range) %>% 
  # expand to grid
  unnest(S_z_range) %>%
  unnest(d13Cs) %>% 
  unnest(d13Ca_range) %>% 
  group_split(Sample) %>% 
  map_df(expand, Sample, 
      S_z_range, d13Cs, 
      d13Cresp, d13Ca_range) %>% 
  # calculate pCO2
  mutate(pCO2 = S_z_range * ((d13Cs - 1.0044 * d13Cresp - 4.4) / 
                               (d13Ca_range - d13Cs))) %>% 
  group_by(Sample) 
  

# with fixed S_Z_values
sol_dat_fix <- sol_dat %>% 
  mutate(
    # set up temperature range
    temp_range = map2(soil_temp_min, soil_temp_max, seq, by = 1), 
    # calculate d13Cs
    d13Cs = map2(d13Ccarb, temp_range, 
                 ~ .x - (11.98 - 0.12 * .y)),
    # set up atmospheric d13C range
    d13Ca_range = map2(d13C_atm_min, d13C_atm_max, seq, by = 0.1), 
    Sz_500 = 500, 
    Sz_1000 = 1000, 
    Sz_1500 = 1500) %>% 
  # expand to grid
  unnest(d13Cs) %>% 
  unnest(d13Ca_range) %>% 
  group_split(Sample) %>% 
  map_df(expand, Sample,
         Sz_500, 
         Sz_1000, 
         Sz_1500,
         d13Cs, 
         d13Cresp, d13Ca_range) %>% 
  # calculate pCO2
  mutate(pCO2_Sz_500 = Sz_500 * ((d13Cs - 1.0044 * d13Cresp - 4.4) / 
                               (d13Ca_range - d13Cs)), 
         pCO2_Sz_1000 = Sz_1000 * ((d13Cs - 1.0044 * d13Cresp - 4.4) / 
                                (d13Ca_range - d13Cs)),
         pCO2_Sz_1500 = Sz_1500 * ((d13Cs - 1.0044 * d13Cresp - 4.4) / 
                                (d13Ca_range - d13Cs))) %>% 
  group_by(Sample) 



# Credible intervals ------------------------------------------------------


## highest density intervals
# mean
sol_dat_mean_hdi <- sol_dat_dist %>% 
  mean_hdi(pCO2) %>% 
  select(Sample, lwr_ci = .lower,
         mean_pCO2 = pCO2, 
         upr_ci = .upper)



# median
sol_dat_median_hdi <- sol_dat_dist %>% 
  median_hdi(pCO2) %>% 
  select(Sample, lwr_ci = .lower,
         mean_pCO2 = pCO2, 
         upr_ci = .upper)


## quantile intervals
# mean
sol_dat_mean_qi <- sol_dat_dist %>% 
  mean_qi(pCO2) %>% 
  select(Sample, lwr_ci = .lower,
         mean_pCO2 = pCO2, 
         upr_ci = .upper)



# median
sol_dat_median_qi <- sol_dat_dist %>% 
  median_qi(pCO2) %>% 
  select(Sample, lwr_ci = .lower,
         mean_pCO2 = pCO2, 
         upr_ci = .upper)


### same for fixed Sz values
# mean
sol_dat_fix_mean_hdi <- sol_dat_fix %>% 
  mean_hdi(pCO2_Sz_500, 
            pCO2_Sz_1000, 
            pCO2_Sz_1500) %>% 
  select(-starts_with("."))



# Save to excel file ------------------------------------------------------


# mean hdi
write.xlsx(sol_dat_mean_hdi, file = here("Data", 
                                         "Output", 
                                         "mean-pCO2-hdi.xlsx"))

# median hdi
write.xlsx(sol_dat_median_hdi, file = here("Data", 
                                           "Output",
                                           "median-pCO2-hdi.xlsx"))

# mean qi
write.xlsx(sol_dat_mean_qi, file = here("Data", 
                                        "Output",
                                        "mean-pCO2-qi.xlsx"))

# median qi
write.xlsx(sol_dat_median_qi, file = here("Data", 
                                          "Output",
                                          "median-pCO2-qi.xlsx"))

# fixed Sz_values
write.xlsx(sol_dat_fix_mean_hdi, file = here("Data", 
                                             "Output",
                                             "mean-pCO2-hdi-fixed-Sz.xlsx"))

