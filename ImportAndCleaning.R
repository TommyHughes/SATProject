# Packages
library(tidyr)
library(dplyr)
library(data.table)
library(ggplot2)
library(stringr)
library(caTools)

# IMPORTING------------------------------------------------------------------------------------------

# Let's begin by importing the SAT ISD performance files as a list of data.table's.
# From these files we will obtain a data.table containing information about a ISDs 
# student population, county, and related average SAT math scores.
importNames <- c("sat_district_data_class_2014.csv", "sat_district_data_class_2015.csv",
                 "sat_district_data_class_2016.csv", "sat_district_data_class_2017.csv",
                 "DataUSALocationIncomeTexas.csv")

scoresList <- lapply(importNames[1:4], data.table::fread, 
                     drop = c('County', 'Region', 'RegnName', 'Reading',
                              'Writing', 'Total', 'Grads_Mskd', 'Exnees_Mskd',
                              'Part_Rate', 'Crit_Mskd', 'Above_Crit_Rate', 'ERW'))

# Let's name the data.tables in scoresList by year.
names(scoresList) <- sapply(importNames[1:4], stringr::str_extract, pattern = "20..")

# Now we will import another file containing median household income by county and year (2013-2016)
# as a data.table. We want to name variables so they coincide with tables in scoresList.
countyHouseholdIncome <- data.table::fread(importNames[5], drop = 'geo', col.names = c("Year", "CntyName", "MedHI"),
                                           colClasses = c("numeric", "character", "Null", "numeric"))

# The MedHI column is imported as a character vector when it should be numeric.
countyHouseholdIncome$MedHI <- as.numeric(countyHouseholdIncome$MedHI)

# Some entries in countyHouseholdIncome$CntyName have ", TX" added to the end of it. This will 
# cause problems when joining with scoresList. Let's remove this character string from any entries
# which contain it.
countyHouseholdIncome$CntyName <- sapply(countyHouseholdIncome$CntyName,
                                         stringr::str_replace,
                                         pattern = ", TX",
                                         replacement = "")

# The information in countyHouseholdIncome only covers years 2013 through 2016. A separate file contains
# median household income data by county in Texas for 2017.
temp_HI2017 <- data.table::fread(input = "CntyNameHI2017.csv", colClasses = c("character", "numeric"))

# The county names in temp_HI2017 are missing the word "County" at the end which will create problems when
# joining with scoresList. To correct this, we will add the character string "County" with a space in front
# to the end of the county names in temp_HI2017$CntyName
temp_HI2017$CntyName <- sapply(temp_HI2017$CntyName, stringr::str_c, "County", sep = " ")

# Let's add a year column to this data so it is easier to join with countyHouseholdIncome.
temp_HI2017 <- temp_HI2017 %>%
  tibble::add_column(Year = 2017, .before = "CntyName")

# Now we join temp_HI2017 to countyHouseholdIncome by adding the rows of temp_HI2017 to the bottom of
# countyHouseholdIncome.
 countyHouseholdIncome <- rbind(countyHouseholdIncome, temp_HI2017)

# CLEANING--------------------------------------------------------------------------------------------

#' scoresList[[year]]$District has incorrect characters in all years.
#' We'll correct this by removing unwanted characters, namely, "="s and """s.
#' Note we DON'T want to convert these to integers or doubles since they start
#' with 00.
for(i in 1:4){
  scoresList[[i]]$District <- scoresList[[i]]$District %>% 
    sapply(stringr::str_replace_all, pattern = '=|"', replacement = "")
}

# We also want to make the year the test was taken into a variable, so we will add a year column to 
# each data.table in scoresList.
for(i in 1:4){
  scoresList[[i]] <- scoresList[[i]] %>% 
    tibble::add_column(Year = 2013+i, .before = "Math")
}

# Now let's combine the data.tables stored in scoresList into one master table, masterScores.We want to simply
# stack the tables on top of each other.
masterScores <- data.table::rbindlist(scoresList)

# Now let's join countyHouseholdIncome to masterScores. We will join by Year and CntyName
masterScores <- masterScores[countyHouseholdIncome, on = c("Year", "CntyName"), nomatch = 0]

# We want to factor masterScores$Group to make it easier to summarize useful info about specific populations.
masterScores$Group <- factor(masterScores$Group)
