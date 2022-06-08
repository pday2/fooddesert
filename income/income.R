#### Biga Data for social Sciences cleaning data ###
####                   Income                    ###

# first we install the packages we need for the cleaning:

#install.packages("naniar")
#install.packages("tidyverse")
#install.packages("stringr")
#install.packages("dplyr")
#install.packages("naniar")

# Now we run the packages:

library("tidyverse")
library("stringr")
library("dplyr")
library("naniar")

income_df <- read_csv("KU LEUVEN/Classes/Big Data for Social Sciences/Second Part/data cleaning/ACSST5Y2020.S1901_data_with_overlays_2022-04-26T161742.csv")
summary(income_df)
glimpse(income_df)

income_df = income_df %>% select(ends_with("E"))

income_df = income_df[-1,] 
income_df[c('code', 'zcta')] <- str_split_fixed(income_df$NAME, ' ', 2)


income_df = income_df[c('zcta', 'S1901_C01_001E', 'S1901_C01_002E', 'S1901_C01_003E', 
                        'S1901_C01_004E', 'S1901_C01_005E', 'S1901_C01_006E', 'S1901_C01_007E', 'S1901_C01_008E', 'S1901_C01_009E',
                        'S1901_C01_010E', 'S1901_C01_011E', 'S1901_C01_013E')]

income_df <- income_df %>% rename(hous_total = S1901_C01_001E)
income_df <- income_df %>% rename(und_10000 = S1901_C01_002E)
income_df <- income_df %>% rename(b10000_14999 = S1901_C01_003E)
income_df <- income_df %>% rename(b15000_24999 = S1901_C01_004E)
income_df <- income_df %>% rename(b25000_34999 = S1901_C01_005E)
income_df <- income_df %>% rename(b35000_49999 = S1901_C01_006E)
income_df <- income_df %>% rename(b50000_74999 = S1901_C01_007E)
income_df <- income_df %>% rename(b75000_99999 = S1901_C01_008E)
income_df <- income_df %>% rename(b100000_149999 = S1901_C01_009E)
income_df <- income_df %>% rename(b150000_199999 = S1901_C01_010E)
income_df <- income_df %>% rename(b200000_more = S1901_C01_011E)
income_df <- income_df %>% rename(mean_income = S1901_C01_013E)


income_df1 <-replace(income_df$mean_income, income_df$mean_income == "N",NA)
income_df1 <-replace(income_df$mean_income, income_df$mean_income == "-",NA)
income_df$mean_income = income_df1

income_df %>% replace_with_na(replace = list(mean_income = "N"))

sum(is.na(income_df$mean_income)) # we have a lot of missing data (1325)

income_df$mean_income = as.integer(income_df$mean_income)

# we used to ways to select the 3 first number of the zip code to create a variable of the region

income_df$region <- sub("^(\\d{3}).*$", "\\1", income_df$zcta)
income_df$region <- substr(income_df$zcta, 0, 3)


income <- group_by(income_df, region)

income1 <- summarise(income,avg_reg = mean(mean_income, na.rm = TRUE)) # Calculate the mean for the region

income_df <- merge(x = income, y = income1, by = "region", all = TRUE) # merge the og table with the mean income per region.

income_df <- income_df %>% mutate(mean_income = coalesce(mean_income,avg_reg)) # We set missing values in income mean
                                                                               # to the mean of the region depending on the zip code.

sum(is.na(income_df$mean_income)) # We still have 23 missing values that we ca drop or replace with the average
                                  # for sake of this project we will input this missing values by hand.

income_df = income_df[c('zcta', 'mean_income')] # This is the final data frame we will export to excel to fill in the 23 missing values by hand missing values. 

write.csv(income_df,"KU LEUVEN/Classes/Big Data for Social Sciences/Second Part/data cleaning/income_cleaned.csv", row.names = FALSE)
