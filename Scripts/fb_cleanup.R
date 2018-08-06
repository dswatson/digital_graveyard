# Set working directory
setwd('./Documents/DPhil/Deaths_on_FB/Data/Final')

# Load libraries
library(tidyverse)

# Import Facebook data
fb <- read_csv('Npopdata_03-08-18.csv')
colnames(fb)[1] <- 'Age'

# Tidy data
fb <- fb %>%
  gather(Code, Users, -Age) %>%
  mutate(Users = Users / 1e6L)

# Replace country codes with country names
codes <- read_csv('country_codes.csv')

# Merge, export
fb <- fb %>%
  inner_join(codes, by = 'Code') %>%
  select(Country, Age, Users) 
write_csv(fb, 'fb_dat.csv')