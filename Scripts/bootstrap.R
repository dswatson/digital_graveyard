### BAYESIAN BOOTSTRAP ###

# Load libraries, register cores
library(data.table)
library(tidyverse)
library(mgcv)
library(doMC)
registerDoMC(8)

# Set seed
set.seed(123, kind = "L'Ecuyer-CMRG")

# Import data
un_dat <- readRDS('./Data/un_dat.rds')
fb_dat <- readRDS('./Data/fb_dat.rds')

# Bootstrap loop 
boot <- function(country, b) {
  # Reduce data
  un_dat <- un_dat[Location == country & Age >= 13]
  fb_dat <- fb_dat[Country == country]
  # Draw random Dirichlet weights 
  un_wts <- rexp(nrow(un_dat))
  un_wts <- (un_wts / sum(un_wts)) * nrow(un_dat)
  fb_wts <- rexp(nrow(fb_dat) - 1)
  fb_wts <- fb_wts / sum(fb_wts) * (1 - 1/nrow(fb_dat))
  fb_wts <- c(fb_wts, 1/nrow(fb_dat)) * nrow(fb_dat)
  # Build death model
  mr_mod <- gam(Mortality_Rate ~ ti(Age) + ti(Time) + 
                ti(Age, Time), data = un_dat, family = betar(), 
                weights = un_wts)
  # Build Facebook model
  fb_mod <- gam(Users ~ s(Age, bs = 'cr', k = 10), data = fb_dat, family = nb(), 
                weights = fb_wts)
  # Build population model
  pop_mod <- gam(Population ~ ti(Age, k = 10) + ti(Time, k = 10) + 
                 ti(Age, Time, k = 10), data = un_dat, 
                 family = gaussian(link = 'log'),
                 weights = un_wts)
  # Build grid
  df <- crossing(
    Assumption = c('Shrinking', 'Growing'),
          Time = 2018:2100,
           Age = 18:100
  ) %>% as.data.table(.)
  # Predict mortality rate and population data
  df[, mr_hat := predict(mr_mod, df, type = 'response')
    ][, pop_hat := predict(pop_mod, df, type = 'response')
  # Predict baseline FB data
    ][Time == 2018, fb_hat := 
      predict(fb_mod, df[Time == 2018], type = 'response')
  # Call bullshit on any markets with penetration rates > 1
    ][fb_hat > pop_hat, fb_hat := pop_hat]
  # Extend predictions under the shrinking assumption
  for (year in 2019:2100) {
    survivors <- df[Assumption == 'Shrinking' & Time == year - 1,
                    fb_hat * (1 - mr_hat)]
    df[Assumption == 'Shrinking' & Time == year, 
       fb_hat := lag(survivors, default = 0)]
  }
  # Extend predictions under the growing assumption
  baseline <- df[Assumption == 'Growing' & Time == 2018, fb_hat]
  df[Assumption == 'Growing', fb_hat := baseline * 1.13^(Time - 2018), by = Time
  # But no penetration rates > 1
    ][fb_hat > pop_hat, fb_hat := pop_hat]
  # Compute annual live and dead profiles
  shrink_alive <- df[Assumption == 'Shrinking', sum(fb_hat), by = Time]$V1
  shrink_dead <- diff(shrink_alive) * -1
  grow_alive <- df[Assumption == 'Growing', sum(fb_hat), by = Time]$V1
  grow_dead <- df[Assumption == 'Growing' & Time != 2100, 
                  sum(mr_hat * fb_hat), by = Time]$V1
  out <- crossing(
    Assumption = c('Shrinking', 'Growing'),
        Status = c('Alive', 'Dead'),
          Year = 2019:2100
  ) %>% as.data.table(.)
  out[Assumption == 'Shrinking' & Status == 'Alive', Profiles := shrink_alive
    ][Assumption == 'Shrinking' & Status == 'Dead', Profiles := shrink_dead
    ][Assumption == 'Growing' & Status == 'Alive', Profiles := grow_alive
    ][Assumption == 'Growing' & Status == 'Dead', Profiles := grow_dead
    ][, Country := country
    ][, Run := b]
  return(out[, .(Country, Year, Assumption, Status, Profiles, Run)])
}

# Execute in parallel
df <- foreach(country = fb_dat[, unique(Country)], .combine = rbind) %:% 
  foreach(b = seq_len(500), .combine = rbind) %dopar%
  boot(country, b)
saveRDS(df[Assumption == 'Shrinking'], './Results/global_shrinking_boot.rds')
saveRDS(df[Assumption == 'Growing'], './Results/global_growing_boot.rds')





