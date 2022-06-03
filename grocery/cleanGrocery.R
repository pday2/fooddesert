library("tidyverse")
if (Sys.info()[1] == "Windows") {
  setwd("C:/Users/peter/My Tresors/Documentsacer/KULeuven/CollAnaBigData/data/grocery")
} else {
  setwd("/home/muddy/Tresors/Documentsacer/KULeuven/CollAnaBigData/data/grocery")   
}
# https://www.openicpsr.org/openicpsr/project/123042/version/V1/view
# National Neighborhood Data Archive (NaNDA): 
#          Grocery Stores by ZIP Code Tabulation Area, United States, 2003-2017
groceryFile <- 'nanda_grocery_zcta_2003-2017_01P.csv'
grocery <- read.csv(groceryFile)

grocery2017 <- grocery %>% filter(year==2017)

sum(is.na(grocery2017$population)) # = 142
sum(is.na(grocery2017$popden_445110)) # = 421

grocery2017Filter = grocery2017 %>% filter( !is.na(population) |  !is.na(popden_445110))

groceryData = grocery2017Filter[c('zcta19', 'year', 'population', 'aland10', 
                                  'count_sales_445110', 'popden_sales_445110', 
                                  'aden_sales_445110')]
sum(is.na(groceryData$count_sales_445110)) # = 0 !!!
groceryData <- groceryData %>% rename(zcta=zcta19)
# row.name = FALSE - don't need to save row numbers
write.csv(groceryData,'grocery.csv',row.names = FALSE)
