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



##### PART I: CURVE FITTING AND MULTIPLYING #####

### Figure 1 ###

# Build death model
mr_mod <- gam(logit_mr ~ s(Age, by = Time, k = 15),
              data = un_dat[Age >= 10 & Location == 'United States'])

# Build Facebook model
fb_mod <- gam(Users ~ s(Age, k = 40), 
              data = fb_dat[Country == 'United States'])

# Fill in 2018 data, trim distributions
df <- data.table(
  Time = 2018, 
   Age = 13:100
)
df[, mr_hat := predict(mr_mod, df) %>% plogis(.)
  ][, fb_hat := predict(fb_mod, df)
  ][mr_hat > 1, mr_hat := 1
  ][mr_hat < 0, mr_hat := 0
  ][fb_hat < 0, fb_hat := 0]

# Assume shrinking penetration rates
for (year in 2019:2030) {
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

# Plot "real" vs. projected mortality rates
p <- ggplot() + 
  geom_point(data = un_dat %>% filter(Location == 'United States',
                                          Time == 2030,
                                           Age >= 10), 
             aes(Age, Mortality_Rate * 1000)) + 
  geom_line(data = df %>% filter(Time == 2030, Age <= 98), 
            aes(Age, mr_hat * 1000),
            size = 0.75, color = 'blue') + 
  labs(title = 'Projected Mortality: United States, 2030',
           y = 'Deaths Per Thousand') +
  theme_bw() + 
  theme(plot.title = element_text(hjust = 0.5))

### Figure 2 ###

# Add User column to data frame
df[Time == 2018 & Age %in% c(18:64, 100), 
   Users := fb_dat[Country == 'United States', Users]]

# Assume shrinking penetration rates
for (year in 2019:2030) {
  last_df <- df[Time == year - 1 & Age %in% c(18:64, 100)]
  df[Time == year & Age %in% c(18:64, 100),
     Users := last_df[, Users * (1 - mr_hat)]]
}

# Plot "real" vs. projected Facebook user totals
p <- ggplot() + 
  geom_point(data = df %>% filter(Time == 2030, Age %in% 18:64),
             aes(Age, Users)) + 
  geom_line(data = df %>% filter(Time == 2030, Age %in% 18:64), 
            aes(Age, fb_hat), size = 0.75, color = 'blue') + 
  labs(title = 'Projected Facebook Users: United States, 2030',
           y = 'Users (Millions)') + 
  theme_bw() + 
  theme(plot.title = element_text(hjust = 0.5))

### Figure 3 ###

# Plot product of two estimated functions over the range [13, 100]
df[, Dead_Profiles := mr_hat * fb_hat * 1000]
p <- ggplot(df[Time == 2030], aes(Age, Dead_Profiles)) + 
  geom_line(size = 0.75, color = 'blue') + 
  labs(title = 'Projected Dead Profiles: United States, 2030',
           y = 'Dead Profiles (Thousands)') + 
  theme_bw() + 
  theme(plot.title = element_text(hjust = 0.5))



##### PART II: RIBBON PLOTS #####

# Top countries by users
totals <- fb_dat %>%
  group_by(Country) %>%
  summarise(Total = sum(Users)) %>%
  arrange(desc(Total)) %>%
  # Just top 10
  filter(Total > 40) %>% 
  # Selecting a palette from D3
  mutate(Color = pal_d3()(10)[c(2, 1, 3:10)])

# Function for counting dead profiles under various assumptions
death_cumsum <- function(country, assumption = 'shrinking') {
  
  # Build death model
  mr_mod <- gam(logit_mr ~ s(Age, by = Time, k = 15), 
                data = un_dat[Age >= 10 & Location == country])
  
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
                   data = un_dat[Age >= 10 & Location == country])
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
    as.data.table(.) %>%
    return(.)
  
}

### Figure 4 ### 

df <- foreach(place = totals$Country, .combine = rbind) %dopar%
  death_cumsum(place, assumption = 'shrinking')

# We want biggest countries first
most_dead <- df %>%
  filter(Time == 2095) %>%
  inner_join(totals, by = 'Country') %>%
  arrange(desc(CumSum))
df[, Country := factor(Country, levels = most_dead$Country)]

# Build plot
p <- ggplot(df, aes(Time, CumSum, group = Country, fill = Country)) + 
  geom_area(alpha = 0.9) + 
  labs(title = 'Accumulation of Dead Profiles',
           x = 'Year',
           y = 'Dead Profiles (Millions)') +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5)) + 
  scale_fill_manual(values = most_dead$Color) 


### Figure 5 ###

df <- foreach(place = totals$Country, .combine = rbind) %dopar%
  death_cumsum(place, assumption = 'growing') 

# We want biggest countries last
most_dead <- df %>%
  filter(Time == 2095) %>%
  inner_join(totals, by = 'Country') %>%
  arrange(desc(CumSum))
df[, Country := factor(Country, levels = most_dead$Country)]

# Build plot
p <- ggplot(df, aes(Time, CumSum, group = Country, fill = Country)) + 
  geom_area(alpha = 0.9) + 
  labs(title = 'Accumulation of Dead Profiles',
           x = 'Year',
           y = 'Dead Profiles (Millions)') +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5)) + 
  scale_fill_manual(values = most_dead$Color) 




# Why so many more dead users in the USA than in India?

fn <- function(country) {
  # Build death model
  mr_mod <- gam(logit_mr ~ s(Age, by = Time, k = 15), 
                data = un_dat[Age >= 10 & Location == country])
  
  # Build Facebook model
  fb_mod <- gam(Users ~ s(Age, k = 40), 
                data = fb_dat[Country == country])
  
  # Fill in 2018 data, trim distributions
  df <- data.table(
    Time = 2018, 
     Age = 13:100
  )
  df[, mr_hat := predict(mr_mod, df) %>% plogis(.)
    ][, fb_hat := predict(fb_mod, df)
    ][mr_hat > 1, mr_hat := 1
    ][mr_hat < 0, mr_hat := 0
    ][fb_hat < 0, fb_hat := 0
    ][, Country := country]
  return(df)
}
usa <- fn('United States')
india <- fn('India')
df <- rbind(usa, india)

# Also need a different dataframe for the ribbon
df_rib <- cbind(usa[Age >= 38, .(Age, fb_hat.usa = fb_hat)],
                india[Age >= 38, .(fb_hat.india = fb_hat)])

# Plot that shiz
p <- ggplot() + 
  geom_line(data = df, aes(Age, fb_hat, color = Country), size = 0.75) +
  geom_ribbon(data = df_rib, 
              aes(x = Age, ymin = fb_hat.india, ymax = fb_hat.usa),
              fill = 'blue', alpha = 0.25) + 
  labs(title = 'User Curves by Age and Country',
           y = 'Facebook Users (Millions)') +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5)) + 
  scale_color_d3()


# By continent
countries <- fread('./Data/Final/countries.csv')
colors <- data.table(
  Continent = c('Asia', 'North America', 'Europe', 
                'South America', 'Africa', 'Oceania'),
      Color = pal_d3()(6)
)

# Scenario I
df <- readRDS('./Data/Final/global_shrinking.rds')
df <- merge(df, countries, by = 'Country') %>%
  group_by(Time, Continent) %>%
  summarise(CumSum = sum(CumSum)) %>%
  ungroup(.) %>%
  as.data.table(.)

# We want biggest countries first
most_dead <- df %>%
  filter(Time == 2095) %>%
  inner_join(colors, by = 'Continent') %>%
  arrange(desc(CumSum))
df[, Continent := factor(Continent, levels = most_dead$Continent)]

# Plot 
p <- ggplot(df, aes(Time, CumSum, group = Continent, fill = Continent)) + 
  geom_area(alpha = 0.9) +
  labs(title = 'Global Accumulation of Dead Profiles:\nScenario I',
           x = 'Year',
           y = 'Dead Profiles (Millions)') +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_fill_manual(values = most_dead$Color)

# Scenario II
df <- readRDS('./Data/Final/global_growing.rds')
df <- merge(df, countries, by = 'Country') %>%
  group_by(Time, Continent) %>%
  summarise(CumSum = sum(CumSum)) %>%
  ungroup(.) %>%
  as.data.table(.)

# We want biggest countries first
most_dead <- df %>%
  filter(Time == 2095) %>%
  inner_join(colors, by = 'Continent') %>%
  arrange(desc(CumSum))
df[, Continent := factor(Continent, levels = most_dead$Continent)]

# Plot 
p <- ggplot(df, aes(Time, CumSum, group = Continent, fill = Continent)) + 
  geom_area(alpha = 0.9) +
  labs(title = 'Global Accumulation of Dead Profiles:\nScenario II',
       x = 'Year',
       y = 'Dead Profiles (Millions)') +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_fill_manual(values = most_dead$Color)







