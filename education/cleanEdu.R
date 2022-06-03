library("tidyverse")
if (Sys.info()[1] == "Windows") {
  setwd("C:/Users/peter/My Tresors/Documentsacer/KULeuven/CollAnaBigData/data/education")
} else {
  setwd("/home/muddy/Tresors/Documentsacer/KULeuven/CollAnaBigData/data/education")   
}
# https://chronicdata.cdc.gov/500-Cities-Places/PLACES-Local-Data-for-Better-Health-ZCTA-Data-2021/qnzd-25i4
eduFile <- 'ACSST5Y2020.S1501_data_with_overlays_2022-04-27T105238.csv'
edu <- read.csv(eduFile, skip = 1, na.strings='-')

# Want cols 1,2,131-184

edu <- edu[c(1,2,131:184)]
eduEsts <- edu %>% select('id', starts_with("Estimate")) 

# clean up ZCTA codes
eduEsts$id <- str_replace(eduEsts$id, '8600000US','')
# Remove 'Estimate..Percent..AGE.BY.EDUCATIONAL.ATTAINMENT..Population'
eduEstsRename <- eduEsts %>% 
  rename_with(.fn = ~ str_replace(.x, 'Estimate..Percent..AGE.BY.EDUCATIONAL.ATTAINMENT..Population.',''),
              .cols = starts_with("Estimate"))

# Remove the columns which whose values are all (X)
eduEstsRename <- eduEstsRename[-c(2,7,17,20,23,26)]

# Rename id to zcta for easy joins!
eduEstsRename <- eduEstsRename %>% rename(zcta=id)

# Change 18.to.24.years.. to 18-24
eduEstsRename <- eduEstsRename %>% 
  rename_with(.fn = ~ str_replace(.x, '18.to.24.years..','18-24_'),
              .cols = starts_with("18"))
# Change 25.to.34.years.. to 25-34
eduEstsRename <- eduEstsRename %>% 
  rename_with(.fn = ~ str_replace(.x, '25.to.34.years..','25-34_'),
              .cols = starts_with("25.to.34"))
# Change 25.years.and.over.. to gt25
eduEstsRename <- eduEstsRename %>% 
  rename_with(.fn = ~ str_replace(.x, '25.years.and.over..','gt25_'),
              .cols = starts_with("25.years.and.over.."))

# Change 65.years.and.over.. to gt65
eduEstsRename <- eduEstsRename %>% 
  rename_with(.fn = ~ str_replace(.x, '65.years.and.over..','gt65_'),
              .cols = starts_with("65.years.and.over.."))

# Change 35.to.44.years.. to 35-44
eduEstsRename <- eduEstsRename %>% 
  rename_with(.fn = ~ str_replace(.x, '35.to.44.years..','35-44_'),
              .cols = starts_with("35.to.44"))

# Change 45.to.64.years.. to 45-64
eduEstsRename <- eduEstsRename %>% 
  rename_with(.fn = ~ str_replace(.x, '45.to.64.years..','45-64_'),
              .cols = starts_with("45.to.64"))

# Change High.school.graduate.or.higher to HS
eduEstsRename <- eduEstsRename %>% 
  rename_with(.fn = ~ str_replace(.x, 'High.school.graduate.or.higher','HS'),
              .cols = ends_with('High.school.graduate.or.higher'))
# Change High.school.graduate..includes.equivalency to HS
eduEstsRename <- eduEstsRename %>% 
  rename_with(.fn = ~ str_replace(.x, 'High.school.graduate..includes.equivalency.','HSequiv'),
              .cols = ends_with('High.school.graduate..includes.equivalency.'))
# Change Some.college.or.associate.s.degree to assoc
eduEstsRename <- eduEstsRename %>% 
  rename_with(.fn = ~ str_replace(.x, 'Some.college.or.associate.s.degree','assoc'),
              .cols = ends_with('Some.college.or.associate.s.degree'))
# Change Less.than.9th.grade to lt9
eduEstsRename <- eduEstsRename %>% 
  rename_with(.fn = ~ str_replace(.x, 'Less.than.9th.grade','lt9'),
              .cols = ends_with('Less.than.9th.grade'))
# Change Bachelor.s.degree.or.higher to bach
eduEstsRename <- eduEstsRename %>% 
  rename_with(.fn = ~ str_replace(.x, 'Bachelor.s.degree.or.higher','gtbach'),
              .cols = ends_with('Bachelor.s.degree.or.higher'))
# Change Bachelor.s.degree to bach
eduEstsRename <- eduEstsRename %>% 
  rename_with(.fn = ~ str_replace(.x, 'Bachelor.s.degree','bach'),
              .cols = ends_with('Bachelor.s.degree'))
# Change Graduate.or.professional.degree to grad
eduEstsRename <- eduEstsRename %>% 
  rename_with(.fn = ~ str_replace(.x, 'Graduate.or.professional.degree','grad'),
              .cols = ends_with('Graduate.or.professional.degree'))
# Change 9th.to.12th.grade..no.diploma to ltHS
eduEstsRename <- eduEstsRename %>% 
  rename_with(.fn = ~ str_replace(.x, '9th.to.12th.grade..no.diploma','ltHS'),
              .cols = ends_with('9th.to.12th.grade..no.diploma'))
# Change Less.than.high.school.graduate to ltHS
eduEstsRename <- eduEstsRename %>% 
  rename_with(.fn = ~ str_replace(.x, 'Less.than.high.school.graduate','ltHS'),
              .cols = ends_with('Less.than.high.school.graduate'))
# Change Some.college..no.degree to ltBach
eduEstsRename <- eduEstsRename %>% 
  rename_with(.fn = ~ str_replace(.x, 'Some.college..no.degree','ltBach'),
              .cols = ends_with('Some.college..no.degree'))
# Change Associate.s.degree to assoc
eduEstsRename <- eduEstsRename %>% 
  rename_with(.fn = ~ str_replace(.x, 'Associate.s.degree','assoc'),
              .cols = ends_with('Associate.s.degree'))

# Drop NAs for now at least
eduData <- eduEstsRename %>% drop_na()

sum(is.na(eduData))

# row.name = FALSE - don't need to save row numbers
write.csv(eduEstsRename,'edu.csv',row.names = FALSE)