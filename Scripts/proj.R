# Projection function
proj <- function(time, place, assumption = 'constant') {
  
  # Libraries
  require(mgcv)
  require(data.table)
  require(tidyverse) 
  
  # Build death model
  death_mod <- gam(logit_mr ~ s(Age, by = Time), 
                   data = un_dat[Age >= 10 & Location == place])
  
  # Build Facebook model
  fb_mod <- gam(Users ~ s(Age), data = fb_dat[Country == place])
  
  # Make predictions, trim distributions
  df <- data.table(
    Time = 2018, 
     Age = 13:100
  )
  df[, mr_hat := predict(death_mod, df) %>% plogis(.)
    ][, fb_hat := predict(fb_mod, df)
    ][mr_hat > 1, mr_hat := 1
    ][mr_hat < 0, mr_hat := 0
    ][fb_hat < 0, fb_hat := 0]
  
  # Calculate dead profiles under various assumptions
  if (time > 2018) {
    if (assumption == 'shrinking') {
      # Calculate attrition for next year
      for (year in 2019:time) {
        last_df <- df[Time == year - 1]
        new_df <- data.table(
          Time = year,
           Age = 13:100
        )
        new_df[, mr_hat := predict(death_mod, new_df) %>% plogis(.)
          ][mr_hat > 1, mr_hat := 1
          ][mr_hat < 0, mr_hat := 0
          ][, fb_hat := last_df[, fb_hat * (1 - mr_hat)]]
        df <- rbind(df, new_df)
      } 
    } else if (assumption == 'constant') {
      # Make predictions, trim distributions
      new_df <- data.table(
        Time = rep(2019:time, each = 88), 
         Age = rep(13:100, times = (time - 2019 + 1))
      )
      new_df[, mr_hat := predict(death_mod, new_df) %>% plogis(.)
        ][, fb_hat := predict(fb_mod, new_df)
        ][mr_hat > 1, mr_hat := 1
        ][mr_hat < 0, mr_hat := 0
        ][fb_hat < 0, fb_hat := 0]
      df <- rbind(df, new_df)
    } else if (assumption == 'growing') {
      # Compound growth
      for (year in 2019:time) {
        y_diff <- year - 2018
        new_df <- data.table(
          Time = year,
           Age = 13:100
        )
        new_df[, mr_hat := predict(death_mod, new_df) %>% plogis(.)
          ][, fb_hat := predict(fb_mod, new_df)
          ][mr_hat > 1, mr_hat := 1
          ][mr_hat < 0, mr_hat := 0
          ][fb_hat < 0, fb_hat := 0
          ][, fb_hat := fb_hat * 1.13^y_diff]
        df <- rbind(df, new_df)
      }
      # Now cap user totals at 90% of population
      pop_mod <- gam(Population ~ s(Age, by = Time), 
                     data = un_dat[Age >= 10 & Location == place])
      df[, pop_hat := predict(pop_mod, df) / 1000
        ][pop_hat < 0, pop_hat := 0
        ][fb_hat > 0.9 * pop_hat, fb_hat := 0.9 * pop_hat]
    }
  }
  
  # Integrate the curve, export results
  out <- df[, sum(mr_hat * fb_hat)]
  return(out)
  
}
