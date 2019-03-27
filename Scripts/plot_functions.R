### FIGURES ###

# Load libraries
library(data.table)
library(ggsci)
library(tidyverse)

### Cumulative Deaths, Standard Errors ###
# Import data
df <- rbind(readRDS('./Results/Models/global_shrinking_boot.rds'),
            readRDS('./Results/Models/global_growing_boot.rds'))

# Plotting function
plot_cumsum <- function(country, scenario) {
  # Filter out the living
  df <- df[Status == 'Dead']
  # Filter by country
  df <- df[Country == country]
  # Filter by scenario
  if (scenario == 'A') {
    df <- df[Assumption == 'Shrinking']
  } else if (scenario == 'B') {
    df <- df[Assumption == 'Growing']
  }
  # Compute cumulative sum
  df[, CumSum := cumsum(Profiles) / 1000, by = Run]
  # Compute mean and standard error
  df[, Mean := mean(CumSum), by = Year]
  df[, SE := sd(CumSum), by = Year]
  # Filter
  df <- distinct(df[, .(Year, Mean, SE)])
  # Plot
  ggplot(df, aes(Year, Mean)) + 
    geom_line(color = 'blue') + 
    geom_ribbon(aes(ymin = Mean - SE, ymax = Mean + SE), alpha = 0.5) +
    labs(title = paste0('Accumulation of Dead Profiles:\n',
                        country, ', Scenario ', scenario),
         y = 'Profiles (Millions)') + 
    theme_bw() + 
    theme(plot.title = element_text(hjust = 0.5))
}


### The Living vs. the Dead ###
# Import data
df <- readRDS('./Results/global_models.rds')

# Plotting function
plot_status <- function(country, scenario) {
  # Filter by country
  df <- df[Country == country]
  # Filter by scenario
  if (scenario == 'A') {
    df <- df[Assumption == 'Shrinking']
  } else if (scenario == 'B') {
    df <- df[Assumption == 'Growing']
  }
  # Compute cumulative sum
  df[Status == 'Dead', Profiles := cumsum(Profiles)]
  # Set to millions
  df[, Profiles := Profiles / 1000]
  # Plot
  ggplot(df, aes(Year, Profiles, group = Status, color = Status)) + 
    geom_line(size = 0.75) + 
    scale_color_d3() + 
    labs(title = paste0('Facebook Profiles:\n', 
                        country, ', Scenario ', scenario),
         y = 'Profiles (Millions)') + 
    theme_bw() + 
    theme(plot.title = element_text(hjust = 0.5))
}



