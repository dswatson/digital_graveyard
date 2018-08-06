# Set working directory
setwd('./Documents/DPhil/Deaths_on_FB/Data/Final')

# Load libraries
library(data.table)
library(tidyverse)
library(mgcv)

### Death Data ###

# Import UN data
mort <- read_csv('mortality.csv') 
pop <- read_csv('population.csv') 

# For columns 3-23, delete the space and class as numeric
for (j in 3:23) {
  mort[[j]] <- gsub(' ', '', mort[[j]]) %>% as.numeric(.)
  pop[[j]] <- gsub(' ', '', pop[[j]]) %>% as.numeric(.)
}

# Make sure both data frames have all and only the same columns 
mort <- mort %>% select(-Total)
pop <- pop %>% 
  mutate(Age_95 = Age_95 + Age_100) %>%
  select(-Age_100)

# Fix Time column in mort
mort <- mort %>% 
  mutate(Time = gsub(' -.*', '', Time) %>% as.integer(.))

# Tidy data, reclass age as integer
mort <- mort %>% 
  gather(Age, Deaths, -Time, -Location) %>%
  mutate(Age = gsub('Age_', '', Age) %>% as.integer(.),
      Deaths = Deaths / 5,
         Idx = paste(Location, Time, Age, sep = '.')) %>%
  as.data.table(.)
pop <- pop %>% 
  gather(Age, Population, -Time, -Location) %>%
  mutate(Age = gsub('Age_', '', Age) %>% as.integer(.),
         Idx = paste(Location, Time, Age, sep = '.')) %>%
  as.data.table(.)

# Merge data, calculate mortality rate, Winsorize distribution
un_dat <- merge(mort, pop[, .(Idx, Population)], by = 'Idx'
  )[, Idx := NULL
# Calculate mortality rate
  ][, Mortality_Rate := Deaths / Population
# Winsorize the distribution
  ][Mortality_Rate >= 0.99, Mortality_Rate := 0.99
# Logit transform for easier modelling
  ][, logit_mr := qlogis(Mortality_Rate + 1e-6)
# Fix a few country names 
  ][Location == 'Viet Nam', Location := 'Vietnam'
  ][Location == 'United States of America', Location := 'United States']

# Export
saveRDS(un_dat, 'un_dat.rds')

### Facebook Data ###

# Import Facebook data
fb <- read_csv('fb_dat.csv') %>%
# Remove the 65+ bucket
  filter(Age != 65) %>%
  as.data.table(.)
# Anchor Facebook numbers with a guaranteed 0 users at age 100
anchor <- data.table(
  Country = fb[, unique(Country)],
      Age = 100,
    Users = 0
)
fb <- rbind(fb, anchor)

# Export
saveRDS(fb, 'fb_dat.rds')


