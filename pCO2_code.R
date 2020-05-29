# load libraries
library(tidyverse)


# functions 
# set up mean function for bootstrapping
mean.fun <- function(pCO2_long, pCO2) mean(pCO2_long[pCO2], na.rm = TRUE)

# same for median
median.fun <- function(pCO2_long, pCO2) median(pCO2_long[pCO2], na.rm = TRUE)


# load data
sol_dat <- readxl::read_excel("paleosol_data.xlsx")

# set-up ------------------------------------------------------------------

# set up data frame for output
boot_mean_df <- data.frame(sample_name = sol_dat$Sample, 
                    lwr_ci = vector("numeric", length(sol_dat$Sample)),
                    mean_pCO2 = vector("numeric", length(sol_dat$Sample)), 
                    upr_ci = vector("numeric", length(sol_dat$Sample)))

# same for median
boot_median_df <- boot_mean_df




# loop trhough every sample x ---------------------------------------------


for (x in seq_along(sol_dat$Sample)) {
  
# define soil temperature range
temp_range <- seq(sol_dat$soil_temp_min[x], sol_dat$soil_temp_max[x], 1)

# set up vector for output
d13Cs <- vector("numeric", length(temp_range))

# iterate through each temperature value and calculate d13Cs
for(i in temp_range){
  d13Cs[i -  (max(temp_range) - length(temp_range))] <- 
    sol_dat$d13Ccarb[x] - (11.98 - 0.12 * i)
}

# iterate through each d13Cs value and each Sz value and calculate pC02
# preallocate data frame
pCO2 <- tibble(Sz = seq(500, 1500, 100), 
               temp_range = temp_range, empty_row = rep(0,length(temp_range))) %>% 
  pivot_wider(names_from = temp_range, values_from = empty_row, names_prefix = "temp_")


# calculate pcO2 value for each soil temp/ Sz combination
for (i in seq_along(d13Cs)) {
  for (j in seq_along(pCO2$Sz)) {
    pCO2[i, j +1] <- pCO2$Sz[j] * ((d13Cs[i] - 1.0044 * sol_dat$d13Cresp[x] - 4.4) / (sol_dat$d13Catm[x] - d13Cs[i]))
  }
}

# convert to long format for proceeding
pCO2_long <- pCO2 %>% 
  pivot_longer(cols = -Sz, names_to = "soil_temp", values_to = "pCO2")

# bootstrapping mean
# set seed
set.seed(1234)
boot_mean <- boot::boot(data = pCO2_long$pCO2,
                          statistic = mean.fun, R = 1000, sim="ordinary")

boot_ci_mean <- boot::boot.ci(boot_mean, type = "bca")

# save in data frame
boot_result_mean <- c(as.double(boot_ci_mean$bca[4]),
                      as.double(boot_ci_mean$t0), 
                      as.double(boot_ci_mean$bca[5]))

boot_mean_df[boot_mean_df$sample_name == sol_dat$Sample[x],2:4] <- boot_result_mean

# same for median
# set seed
set.seed(1234)
boot_median <- boot::boot(data = pCO2_long$pCO2,
                        statistic = median.fun, R = 1000, sim="ordinary")

boot_ci_median <- boot::boot.ci(boot_median, type = "bca")

# save in data frame
boot_result_median <- c(as.double(boot_ci_median$bca[4]),
                      as.double(boot_ci_median$t0), 
                      as.double(boot_ci_median$bca[5]))

boot_median_df[boot_median_df$sample_name == sol_dat$Sample[x],2:4] <- boot_result_median
}



