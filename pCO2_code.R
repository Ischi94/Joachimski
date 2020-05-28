# load libraries
library(tidyverse)

# load data
sol_dat <- readxl::read_excel("paleosol_data.xlsx")


# we start with sample BG2 first to get into the matter
# define soil temperature range
temp_range <- seq(sol_dat$soil_temp_min[1], sol_dat$soil_temp_max[1], 1)

# set up vector for output
d13Cs <- vector("numeric", length(temp_range))

# iterate through each temperature value and calculate d13Cs
for(i in temp_range){
  d13Cs[i - 14] <- sol_dat$d13Ccarb[1] - (11.98 - 0.12 * i)
}

# iterate through each d13Cs value and each Sz value and calculate pC02
# preallocate data frame
pCO2 <- tibble(Sz = seq(500, 1500, 100), 
               temp_range = temp_range, empty_row = rep(0,11)) %>% 
  pivot_wider(names_from = temp_range, values_from = empty_row, names_prefix = "temp_")


# calculate pcO2 value for each soil temp/ Sz combination
for (i in seq_along(d13Cs)) {
  for (j in seq_along(pCO2$Sz)) {
    pCO2[i, j +1] <- pCO2$Sz[j] * ((d13Cs[i] - 1.0044 * sol_dat$d13Cresp[1] - 4.4) / (sol_dat$d13Catm[1] - d13Cs[i]))
  }
}

# plot it
# convert to long format for plotting
pCO2_long <- pCO2 %>% 
  pivot_longer(cols = -Sz, names_to = "soil_temp", values_to = "pCO2")

ggplot(pCO2_long) +
  geom_histogram(aes(pCO2), binwidth = 40) +
  geom_vline(xintercept = mean(pCO2_long$pCO2), colour = "red")

# bootstrapping 
# set up mean function
mean.fun <- function(pCO2_long, pCO2) mean(pCO2_long[pCO2], na.rm = TRUE)

boot_sample <- boot::boot(data = pCO2_long$pCO2,
                          statistic = mean.fun, R = 1000, sim="ordinary")

boot_ci <- boot::boot.ci(boot_sample, type = "bca")

boot_result <- tibble(sample_name = sol_dat$Sample[1], lwr_ci = boot_ci$bca[4],
                      mean_pCO2 = boot_ci$t0, upr_ci = boot_ci$bca[5])

boot_sample_values <- boot_sample$t %>% 
  as_tibble() %>% 
  add_column(trials = 1:1000) %>% 
  select(mean_pCO2 = V1, trials)

ggplot(boot_sample_values) +
  geom_histogram(aes(mean_pCO2), binwidth = 7) +
  geom_vline(xintercept = mean(boot_result$mean_pCO2), color = "darkred") +
  geom_vline(xintercept = mean(boot_result$lwr_ci), 
             linetype = "dashed", color = "darkred") +
  geom_vline(xintercept = mean(boot_result$upr_ci), 
             linetype = "dashed", color = "darkred")





