# Set working directory
setwd('./Documents/DPhil/Death_on_FB')

# Load libraries
library(data.table)
library(tidyverse)
library(mgcv)

### Death Data ###
d <- fread('num_deaths.csv') %>% as.data.frame(.)

# For columns 3-23, delete the space and class as numeric
for (j in 3:23) {
  d[, j] <- gsub(' ', '', d[, j]) %>% as.numeric(.)
}

# Reclass time as numeric
d <- d %>% 
  mutate(Year = gsub(' .*', '', Time) %>% as.numeric(.)) 

# Remove unnecessary columns and tidy that data
d <- d %>%
  select(-Total, -Time) %>%
  gather(Age, Deaths, -Year, -Location)

# Reclass age as numeric
d <- d %>% 
  mutate(Age = gsub('Age_', '', Age) %>% as.numeric(.)) 

### Facebook Penetration Data ###
p <- fread('UK_FB.csv') %>%
  mutate(FB_Penetration = FB_Penetration / 100)

# Projection function
proj <- function(time, place) {  
  
  # Fudge the years
  if (time == 2014) {
    d_time <- 2010
  } else if (time > 2014) {
    d_time <- 2015
  }
  
  # Death model
  death_mod <- gam(Deaths ~ s(Age, k = -1), 
                   data = d %>% filter(Year == d_time, 
                                   Location == place))
  
  # Penetration model
  fb_mod <- gam(FB_Penetration ~ s(Age, k = 6), 
                data = p %>% filter(Year == time))
  
  # Deaths on FB model
  df <- data.frame(Age = seq_len(100))
  df$Dead_Profiles <- predict(death_mod, df) * predict(fb_mod, df)
  return(sum(df$Dead_Profiles))
  
}

proj(2015, 'United Kingdom')


