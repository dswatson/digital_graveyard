### CHECK MODEL EDFS ###

# Set working directory
setwd('./Documents/DPhil/Deaths_on_FB')

# Load libraries, register cores
library(data.table)
library(tidyverse)
library(mgcv)
library(ggsci)
library(doMC)
registerDoMC(4)

# Import data
un_dat <- readRDS('./Data/Final/un_dat.rds')
fb_dat <- readRDS('./Data/Final/fb_dat.rds')

# Function for counting dead profiles under various assumptions
edf_check <- function(country, k1 = 15, k2 = 40) {
  
  # Build death model
  mr_mod <- gam(logit_mr ~ s(Age, by = Time, k = k1), 
                data = un_dat[Location == country & Age >= 10])
  
  # Build Facebook model
  fb_mod <- gam(Users ~ s(Age, k = k2), data = fb_dat[Country == country])
  
  # Record EDF
  data.table(
    Country = country,
      Model = c('fb', 'mr'),
      max_k = c(k1, k2),
        EDF = c(sum(fb_mod$edf), sum(mr_mod$edf))
  )
  
}

# Execute in parallel
out <- foreach(place = fb_dat[, unique(Country)], .combine = rbind) %dopar%
  edf_check(place, k1 = 15, k2 = 40)

# Write to disk
saveRDS(out, 'edf_check.rds')

# Check histo
out[Model == 'fb', hist(EDF, breaks = 20)]
out[Model == 'mr', hist(EDF, breaks = 20)]
out[Model == 'fb', summary(EDF)]
out[Model == 'mr', summary(EDF)]


