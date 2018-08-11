### GLOBAL NUMBERS ###

# Set working directory
setwd('./Documents/DPhil/Deaths_on_FB')

# Load libraries
library(data.table)
library(tidyverse)
library(mgcv)
library(ggsci)

# Import data
un_dat <- readRDS('./Data/Final/un_dat.rds')
fb_dat <- readRDS('./Data/Final/fb_dat.rds')

# Function for counting dead profiles under various assumptions
death_cumsum <- function(country, assumption = 'shrinking') {
  
  # Build death model
  mr_mod <- gam(logit_mr ~ s(Age, by = Time, k = 15), 
                data = un_dat[Location == country & Age >= 10])
  
  # Build Facebook model
  fb_mod <- gam(Users ~ s(Age, k = 40), 
                data = fb_dat[Country == country])
  
  # Make predictions, trim distributions
  df <- data.table(
    Time = 2018, 
     Age = 13:100
  )
  df[, mr_hat := predict(mr_mod, df) %>% plogis(.)
    ][, fb_hat := predict(fb_mod, df)
    ][mr_hat > 1, mr_hat := 1
    ][mr_hat < 0, mr_hat := 0
    ][fb_hat < 0, fb_hat := 0]
  
  # Project mortality rates and Facebook users
  if (assumption == 'shrinking') {
    # Calculate attrition for next year
    for (year in 2019:2095) {
      last_df <- df[Time == year - 1]
      new_df <- data.table(
        Time = year,
         Age = 13:100
      )
      new_df[, mr_hat := predict(mr_mod, new_df) %>% plogis(.)
        ][mr_hat > 1, mr_hat := 1
        ][mr_hat < 0, mr_hat := 0
        ][, fb_hat := last_df[, fb_hat * (1 - mr_hat)]]
      df <- rbind(df, new_df)
    } 
  } else if (assumption == 'growing') {
    # Compound growth
    for (year in 2019:2095) {
      y_diff <- year - 2018
      new_df <- data.table(
        Time = year,
         Age = 13:100
      )
      new_df[, mr_hat := predict(mr_mod, new_df) %>% plogis(.)
        ][, fb_hat := predict(fb_mod, new_df)
        ][mr_hat > 1, mr_hat := 1
        ][mr_hat < 0, mr_hat := 0
        ][fb_hat < 0, fb_hat := 0
        ][, fb_hat := fb_hat * 1.13^y_diff]
      df <- rbind(df, new_df)
    }
    # Now cap user totals at 90% of population
    pop_mod <- gam(Population ~ s(Age, by = Time, k = 15), 
                   data = un_dat[Location == country & Age >= 10])
    df[, pop_hat := predict(pop_mod, df) / 1000
      ][pop_hat < 0, pop_hat := 0
      ][fb_hat > 0.9 * pop_hat, fb_hat := 0.9 * pop_hat]
  }
  
  # Multiply projections to estimate number of dead profiles
  df[, Dead_Profiles := mr_hat * fb_hat]
  
  # Reduce to annual deaths
  df %>%
    group_by(Time) %>%
    summarise(Dead_Profiles = sum(Dead_Profiles)) %>%
    ungroup(.) %>%
    mutate(Country = country,
            CumSum = cumsum(Dead_Profiles)) %>%
    select(Country, Time, CumSum) %>%
    return(.)
  
}

# Shrinking
df <- data.table(
  Country = NA_character_,
     Time = NA_integer_,
   CumSum = NA_real_
)
for (country in fb_dat[, unique(Country)]) {
  i <- death_cumsum(country, assumption = 'shrinking')
  df <- rbind(df, i)
}
df <- na.omit(df)
saveRDS(df, './Data/Final/global_shrinking.rds')

# Plot 
df <- df %>%
  group_by(Time) %>%
  summarise(CumSum = sum(CumSum)) %>%
  ungroup(.)
p <- ggplot(df, aes(Time, CumSum)) + 
  geom_area(fill = pal_d3()(1), alpha = 0.9) + 
  labs(title = 'Global Accumulation of Dead Profiles:/nScenario I',
           x = 'Year',
           y = 'Dead Profiles (Millions)') +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5)) 

# Growing
df <- data.table(
  Country = NA_character_,
     Time = NA_integer_,
   CumSum = NA_real_
)
for (country in fb_dat[, unique(Country)]) {
  i <- death_cumsum(country, assumption = 'growing')
  df <- rbind(df, i)
}
df <- na.omit(df)
saveRDS(df, './Data/Final/global_growing.rds')

# Plot 
df <- df %>%
  group_by(Time) %>%
  summarise(CumSum = sum(CumSum)) %>%
  ungroup(.)
p <- ggplot(df, aes(Time, CumSum)) + 
  geom_area(fill = pal_d3()(1), alpha = 0.9) +
  labs(title = 'Global Accumulation of Dead Profiles:\nScenario II',
       x = 'Year',
       y = 'Dead Profiles (Millions)') +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5)) 












