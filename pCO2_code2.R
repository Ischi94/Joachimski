# load libraries
library(tidyverse) # for data processing
library(openxlsx) # for writing to excel file
library(tidybayes) # for credible intervals

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
  unnest(c(S_z_range, d13Cs)) %>% 
  unnest(d13Ca_range) %>% 
  group_split(Sample) %>% 
  map_df(expand, Sample, 
      S_z_range, d13Cs, 
      d13Cresp, d13Ca_range) %>% 
  # calculate pCO2
  mutate(pCO2 = S_z_range * ((d13Cs - 1.0044 * d13Cresp - 4.4) / 
                               (d13Ca_range - d13Cs))) %>% 
  group_by(Sample) 
  
# calculate credible intervals 

# mean
sol_dat_mean <- sol_dat_dist %>% 
  summarise(lwr_ci = mean_qi(pCO2)[[2]], 
            mean_pCO2 = mean_qi(pCO2)[[1]], 
            upr_ci = mean_qi(pCO2)[[3]]) 

# median
sol_dat_median <- sol_dat_dist %>% 
  summarise(lwr_ci = median_qi(pCO2)[[2]], 
            mean_pCO2 = median_qi(pCO2)[[1]], 
            upr_ci = median_qi(pCO2)[[3]]) 
  

# save to excel file

# mean
write.xlsx(sol_dat_mean, file = "mean-pCO2.xlsx")

# median
write.xlsx(sol_dat_median, file = "median-pCO2.xlsx")


sol_dat_mean %>% 
  ggplot(aes(mean_pCO2, Sample, 
             xmin = lwr_ci, 
             xmax = upr_ci)) +
  geom_pointrange()
