#### Biga Data for social Sciences cleaning data ###

library("tidyverse")
library("stringr")
library(dplyr)

race_df <- read_csv("KU LEUVEN/Classes/Big Data for Social Sciences/Second Part/data cleaning/ACSDT5Y2020.B02001_data_with_overlays_2022-04-27T145410.csv")
summary(race_df)
glimpse(race_df)

race_df = race_df[-1,] 
race_df[c('code', 'zcta')] <- str_split_fixed(race_df$NAME, ' ', 2)

#race_df_filter = race_df_filter %>% select('zcta', ends_with("E")) 

race_df = race_df[c('zcta', 'B02001_001E', 'B02001_002E', 'B02001_003E', 
                    'B02001_004E', 'B02001_005E', 'B02001_006E', 'B02001_007E', 'B02001_008E',
                    'B02001_009E', 'B02001_010E')]

race_df <- race_df %>% rename(tot_pop = B02001_001E)
race_df <- race_df %>% rename(white = B02001_002E)
race_df <- race_df %>% rename(black = B02001_003E)
race_df <- race_df %>% rename(alaskan_native = B02001_004E)
race_df <- race_df %>% rename(asian = B02001_005E)
race_df <- race_df %>% rename(pacific_islan = B02001_006E)
race_df <- race_df %>% rename(other = B02001_007E)
race_df <- race_df %>% rename(mixed_1 = B02001_008E)
race_df <- race_df %>% rename(mixed_2 = B02001_009E)
race_df <- race_df %>% rename(mixed_3 = B02001_010E)

race_df$tot_pop = as.integer(race_df$tot_pop)
race_df$white = as.integer(race_df$white)
race_df$black = as.integer(race_df$black)
race_df$alaskan_native = as.integer(race_df$alaskan_native)
race_df$asian = as.integer(race_df$asian)
race_df$pacific_islan = as.integer(race_df$pacific_islan)
race_df$other = as.integer(race_df$other)
race_df$mixed_1 = as.integer(race_df$mixed_1)
race_df$mixed_2 = as.integer(race_df$mixed_2)
race_df$mixed_3 = as.integer(race_df$mixed_3)

glimpse(race_df)

race_df$mixed_race = race_df$mixed_1 + race_df$mixed_2 + race_df$mixed_3

race_df = race_df[c('zcta', 'tot_pop', 'white', 'black', 'alaskan_native', 
                    'asian', 'pacific_islan', 'other', 'mixed_race')]

sum(is.na(race_df)) # we dont have missing data 
   
 