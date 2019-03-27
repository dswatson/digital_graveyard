### GLOBAL MODELS ###

# Load libraries, register cores
library(data.table)
library(tidyverse)
library(mgcv)
library(doMC)
registerDoMC(8)

# Import data
un_dat <- readRDS('un_dat.rds')
fb_dat <- readRDS('fb_dat.rds')

# Analysis loop per country 
death_cumsum <- function(country) {
  # Reduce data
  un_dat <- un_dat[Location == country & Age >= 13]
  fb_dat <- fb_dat[Country == country]
  # Build death model
  mr_mod <- gam(Mortality_Rate ~ ti(Age) + ti(Time) + 
                ti(Age, Time), data = un_dat, family = betar())
  # Build Facebook model
  fb_mod <- gam(Users ~ s(Age, bs = 'cr', k = 10), data = fb_dat, family = nb())
  # Build population model
  pop_mod <- gam(Population ~ ti(Age, k = 10) + ti(Time, k = 10) + 
                 ti(Age, Time, k = 10), data = un_dat, 
                 family = gaussian(link = 'log'))
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
  # No such thing as markets with penetration rate > 1
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
  df[Assumption == 'Growing' & Time > 2018, 
     fb_hat := baseline * 1.13^(Time - 2018), by = Time
  # But no penetration rates > 1
    ][fb_hat > pop_hat, fb_hat := pop_hat]
  # Compute annual deaths
  shrink_fb <- df[Assumption == 'Shrinking', sum(fb_hat), by = Time]$V1
  grow_fb <- df[Assumption == 'Growing' & Time != 2100, 
                sum(mr_hat * fb_hat), by = Time]$V1
  out <- crossing(
    Assumption = c('Shrinking', 'Growing'),
          Time = 2019:2100
  ) %>% as.data.table(.)
  out[Assumption == 'Shrinking', FB_Deaths := diff(shrink_fb) * -1
    ][Assumption == 'Growing', FB_Deaths := grow_fb
    ][, CumSum := cumsum(FB_Deaths), by = Assumption
    ][, Country := country]
  return(out[, .(Assumption, Country, Time, FB_Deaths, CumSum)])
}

# Execute in parallel
df <- foreach(country = fb_dat[, unique(Country)], .combine = rbind) %dopar%
  death_cumsum(country)
saveRDS(df[Assumption == 'Shrinking'], 'global_shrinking.rds')
saveRDS(df[Assumption == 'Growing'], 'global_growing.rds')



