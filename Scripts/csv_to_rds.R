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
# Fix country names 
  ][Location == 'Antigua and Barbuda', Location := 'Antigua'
  ][Location == 'Bolivia (Plurinational State of)', Location := 'Bolivia'
  ][Location == 'Brunei Darussalam', Location := 'Brunei'
  ][Location == 'Cabo Verde', Location := 'Cape Verde'
  ][Location == 'China, Hong Kong SAR', Location := 'Hong Kong'
  ][Location == 'China, Macao SAR', Location := 'Macau'
  ][Location == 'China, Taiwan Province of China', Location := 'Taiwan'
  ][Location == 'Congo', Location := 'Republic of the Congo'
  ][Location == "Lao People's Democratic Republic", Location := 'Laos'
  ][Location == 'Micronesia (Fed. States of)', Location := 'Federated States of Micronesia'
  ][Location == 'Republic of Korea', Location := 'South Korea'
  ][Location == 'Republic of Moldova', Location := 'Moldova'
  ][Location == 'Russian Federation', Location := 'Russia'
  ][Location == 'Saint Lucia', Location := 'St. Lucia'
  ][Location == 'TFYR Macedonia', Location := 'Macedonia'
  ][Location == 'United Republic of Tanzania', Location := 'Tanzania'
  ][Location == 'United States of America', Location := 'United States'
  ][Location == 'United States Virgin Islands', Location := 'US Virgin Islands'
  ][Location == 'Venezuela (Bolivarian Republic of)', Location := 'Venezuela'
  ][Location == 'Viet Nam', Location := 'Vietnam']

### Facebook Data ###

# Import Facebook data
fb_dat <- read_csv('fb_dat.csv') %>%
# Remove the 65+ bucket
  filter(Age != 65) %>%
  as.data.table(.)
# Remove countries with 0 users
zeros <- fb_dat %>%
  group_by(Country) %>%
  summarise(Total = sum(Users)) %>%
  filter(Total == 0)
fb_dat <- fb_dat[!Country %in% zeros$Country]
# Anchor Facebook numbers with a guaranteed 0 users at age 100
anchor <- data.table(
  Country = fb_dat[, unique(Country)],
      Age = 100,
    Users = 0
)
fb_dat <- rbind(fb_dat, anchor)
# Fix country names
fb_dat[Country == 'The Gambia', Country := 'Gambia'
  ][Country == 'Czech Republic', Country := 'Czechia'
  ][Country == 'The Bahamas', Country := 'Bahamas']

# Harmonize countries
overlap <- intersect(un_dat$Location, fb_dat$Country)
un_dat <- un_dat[Location %in% overlap]
fb_dat <- fb_dat[Country %in% overlap]

# Export
saveRDS(un_dat, 'un_dat.rds')
saveRDS(fb, 'fb_dat.rds')


