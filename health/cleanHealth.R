library("tidyverse")
if (Sys.info()[1] == "Windows") {
  setwd("C:/Users/peter/My Tresors/Documentsacer/KULeuven/CollAnaBigData/data/health")
} else {
  setwd("/home/muddy/Tresors/Documentsacer/KULeuven/CollAnaBigData/data/health")   
}
# https://chronicdata.cdc.gov/500-Cities-Places/PLACES-Local-Data-for-Better-Health-ZCTA-Data-2021/qnzd-25i4
healthFile <- 'PLACES__Local_Data_for_Better_Health__ZCTA_Data_2021_release.csv'
health <- read.csv(healthFile)
# LocationName: 5 digits ZIP Code Tabulation Area code
# LocationID: 5-digit Zip Code Tabulation Area (ZCTA5) code
# Geolocation: Latitude, Longitude of city centroid (Format: Point(Longitude Latitude))
# TotalPopulation: Total population of Census 2010 
colnames(health)
health <- select(health, -c( 'DataSource','Data_Value_Footnote_Symbol',  
                             'Data_Value_Footnote','Low_Confidence_Limit', 
                             'High_Confidence_Limit','DataValueTypeID'))
# Measure == 'Obesity among adults aged >=18 years'
levels(health$Measure)
levels(health$MeasureId)
# Obesity among adults aged >=18 years
# Depression among adults aged >=18 years
# Cancer (excluding skin cancer) among adults aged >=18 years
# Cholesterol screening among adults aged >=18 years
# Coronary heart disease among adults aged >=18 years
healthFilter <- health %>% filter(MeasureId=='OBESITY' |
                                  MeasureId=='DEPRESSION' |
                                  MeasureId=='CANCER' |
                                  MeasureId=='CHOLSCREEN' |
                                  MeasureId=='CHD')

healthwide <- pivot_wider(healthFilter, id_cols=c(Year,LocationName,Geolocation, TotalPopulation),
                          names_from=c(MeasureId),values_from=Data_Value)
# Rename LocationName to zcta for easy joins!
healthwide <- healthwide %>% rename(zcta=LocationName)
write.csv(healthwide,'health.csv')

#---------------------
# Warning message:
# Values from `Data_Value` are not uniquely identified; output will contain list-cols.
health %>%
  dplyr::group_by(Short_Question_Text, MeasureId, Measure) %>%
  dplyr::summarise(n = dplyr::n(), .groups = "drop") %>%
  dplyr::filter(n > 1L) 
