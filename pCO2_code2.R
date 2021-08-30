# load libraries
library(tidyverse) # for data processing
library(openxlsx) # for writing to excel file


# load data
sol_dat <- readxl::read_excel("paleosol_data.xlsx")

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
  drop_na(Sample)


sol_dat %>% 
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
  unnest(c(S_z_range, d13Cs)) %>% 
  unnest(d13Ca_range) %>% 
  group_split(Sample) %>% 
  map_df(expand, Sample, 
      S_z_range, d13Cs, 
      d13Cresp, d13Ca_range) %>% 
  # calculate pCO2
  mutate(pCO2 = S_z_range * ((d13Cs - 1.0044 * d13Cresp - 4.4) / 
                               (d13Ca_range - d13Cs))) %>% 
  group_by(Sample) %>% 
  summarise(est = tidybayes::median_qi(pCO2), 
            est_mean = tidybayes::mean_qi(pCO2))
  
