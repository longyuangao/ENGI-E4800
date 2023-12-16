setwd("~/Desktop/Fall 2023 Classes/Data Science & Ethics Capstone") 
library(tidyverse)
library(readxl)
library(dplyr)
library(lubridate)
library(openxlsx)

census_data <- read_csv("CC-EST2020-ALLDATA6.csv")
governors_data <- read_csv("Black State Governors Spreadsheet - Sheet1.csv")
attorney_general_data <- read_csv("Black State Attorney Generals - Sheet1.csv")
congress_data <- read_csv("Black House of Representatives Spreadsheet - Sheet1.csv")
variable_coder <- read_xlsx("VARIABLE for coder X.xlsx")
state_scj <- read_xlsx("State Supreme Court Justices.xlsx")


## 1: Cleaning the datasets 

# a) Congress data 
## Remove all rows without anything in the "Name" column 
## Change the states names to be consistent with the way they are in the "variables" spreadsheet
## Parse the "First Took Office" column; need the first year to be after 2010 and the second before 2020; can probably split on " " then "(" then ")" then "-"
## Split the data into one dataframe for house and another for senate 


congress_data_cleaned = congress_data %>% filter(!is.na(Name)) 
congress_data_cleaned = congress_data_cleaned %>% mutate(`State or Territory` = recode(`State or Territory`, 
                                                         "AL" = "Alabama",
                                                         "CA" = "California",
                                                         "CO" = "Colorado", 
                                                         "CT" = "Connecticut", 
                                                         "DE" = "Delaware",
                                                         "DC" = "District of Columbia",
                                                         "FL" = "Florida", 
                                                         "GA" = "Georgia", 
                                                         "IL" = "Illinois", 
                                                         "IN" = "Indiana", 
                                                         "LA" = "Louisiana",
                                                         "MD" = "Maryland",
                                                         "MA" = "Massachusetts", 
                                                         "MI" = "Michigan", 
                                                         "MN" = "Minnesota", 
                                                         "MS" = "Mississippi", 
                                                         "MO" = "Missouri",
                                                         "NV" = "Nevada",
                                                         "NJ" = "New Jersey", 
                                                         "NY" = "New York", 
                                                         "NC" = "North Carolina", 
                                                         "OH" = "Ohio",
                                                         "OK" = "Oklahoma", 
                                                         "PA" = "Pennsylvania",
                                                         "SC" = "South Carolina", 
                                                         "TN" = "Tennessee",
                                                         "TX" = "Texas",
                                                         "UT" = "Utah",
                                                         "VI" = "Virgin Islands", 
                                                         "VA" = "Virginia",
                                                         "WA" = "Washington", 
                                                         "WI" = "Wisconsin" ))

start_date_parse = function(term_date) {
  
  ## parsing the test to get rid of spaces and dashes
  d = strsplit(term_date, split = " ")[[1]][2] %>% strsplit(split = "-") %>% unlist
  
  # Getting rid of all special characters; "(" and ")" using gsub, then converting the string to a date and extracting the start year
  
  start_date = as.Date(gsub('[^[:alnum:] ]', "", d[1]), format = "%Y") %>% year()
} 

end_date_parse = function(term_date) {
  
  ## parsing the test to get rid of spaces and dashes
  d = strsplit(term_date, split = " ")[[1]][2] %>% strsplit(split = "-") %>% unlist
  
  # Getting rid of all special characters; "(" and ")" using gsub, then converting the string to a date and extracting the start year
  
  end_date = as.Date(gsub('[^[:alnum:] ]', "", d[2]), format = "%Y") %>% year()
} 

congress_data_cleaned$`Start Date` = lapply(congress_data_cleaned$`First Took Office`, start_date_parse)
congress_data_cleaned$`End Date` = lapply(congress_data_cleaned$`First Took Office`, end_date_parse)

congress_data_final = congress_data_cleaned %>% filter((`Start Date` > 2010 & `End Date` < 2020) | (`End Date` > 2010  & `End Date` < 2020) | (`Start Date` > 2010  & `Start Date` < 2020))
congress_data_final$`Start Date` = as.numeric(as.character(congress_data_final$`Start Date`))
congress_data_final$`End Date` = as.numeric(as.character(congress_data_final$`End Date`))




senate_df = congress_data_final %>% filter(Service == "Senate") %>% select(-"First Took Office")
house_df = congress_data_final %>% filter(Service == "House")  %>% select(-"First Took Office")




write.xlsx(senate_df, "Black_senate.xlsx")
write.xlsx(house_df, "Black_house.xlsx")


# b) Attorney general data 
## Filter for "African American" or "Haitian American" in the "Ethnicity" column
## Convert the Term start column to a datetime object then filter for a start date after 1/1/2010 and before 1/1/2021

attorney_general_data_cleaned = attorney_general_data %>% select(c(-"Picture", -"Party", -"Notes", -"Ref"))


race_check = function(race) {
  race_list = strsplit(race, split = "\n") %>% unlist
  if ("African American" %in% race_list | "Jamaican American" %in% race_list | "Haitian American" %in% race_list) {
    return(TRUE)
  }
  else {
    return(FALSE)
  }
}


drop_ind = c()
for (i in 1:nrow(attorney_general_data_cleaned)) {
  if (race_check(attorney_general_data_cleaned$Ethnicity[i]) == FALSE) {
     drop_ind = append(drop_ind, i)
  } 
}

attorney_general_data_cleaned = attorney_general_data_cleaned[-c(drop_ind),]

start_date_parse_ag = function(term_date) {
  y = strsplit(term_date, split = ",") %>% unlist
  start_date = as.Date(y[2], format = "%Y") %>% year()
  return(start_date)
} 

end_date_parse_ag = function(term_date) {
  if (term_date != "present") {
    y = strsplit(term_date, split = ",") %>% unlist
    end_date = as.Date(y[2], format = "%Y") %>% year()
    return(end_date)
  }
  else {
    end_date = Sys.Date() %>% year()
    return(end_date)
  }
} 

attorney_general_data_cleaned$`Term_start` = lapply(attorney_general_data_cleaned$`Term start`, start_date_parse_ag)
attorney_general_data_cleaned$`Term_end` = lapply(attorney_general_data_cleaned$`Term end`, end_date_parse_ag)

attorney_general_data_final = attorney_general_data_cleaned %>% mutate(State = recode(State, "United States Virgin Islands" = "Virgin Islands"))

attorney_general_data_final$`Term start` = as.numeric(as.character(attorney_general_data_final$`Term start`))
attorney_general_data_final$`Term end` = as.numeric(as.character(attorney_general_data_final$`Term end`))


attorney_general_data_final = attorney_general_data_cleaned %>% filter((`Term_start` > 2010 & `Term_end` < 2020) | (`Term_end` > 2010  & `Term_end` < 2020) | (`Term_start` > 2010  & `Term_start` < 2020))




write.xlsx(attorney_general_data_final, "Attorney_general.xlsx")


## c) Census data 
## Can use years 3-14 (2010-2020) 
## Then mutate the data so we have a new column for the total number of people racialized as Black (using “BA_MALE”, “BA_FEMALE”, “HBA_MALE”, “HBA_FEMALE”) “BA” = Black alone “HBA” = Hispanic, Black alone 
## Can mutate another column to be the ratio between Black pop and total pop; giving us the % of Black population by country in a given year

census_data_cleaned = census_data %>% filter(YEAR >= 3)
census_data_cleaned = census_data_cleaned %>% mutate(`Black_population` = (BA_MALE + BA_FEMALE)) 
census_data_cleaned = census_data_cleaned %>% mutate(`Percentage Black` = Black_population/TOT_POP)

(sum(census_data_cleaned$Black_population)/sum(census_data_cleaned$TOT_POP))

#census_data_cleaned = census_data_cleaned %>% mutate(Percentage_Black_Agg = group_by(`STNAME`))

black_pop = census_data_cleaned %>% group_by(`CTYNAME`) %>% summarise(black_population = sum(Black_population))
total_pop = census_data_cleaned %>% group_by(`CTYNAME`) %>% summarise(total_population = sum(TOT_POP))
county_percentages = census_data_cleaned %>% group_by(CTYNAME) %>% summarise(cty_percentage = sum(`Black_population`)/sum(`TOT_POP`))
county_state_census = census_data_cleaned %>% select(c(STNAME,CTYNAME)) %>% unique()



census_aggregate = left_join(county_state_census, black_pop, by = "CTYNAME") %>% left_join(total_pop, by = "CTYNAME") %>% left_join(county_percentages, by = "CTYNAME")



# census_data_cleaned = left_join(census_data_cleaned, county_percentages, by = "CTYNAME")

#census_data_final = census_data_cleaned %>% select(c("STNAME", "CTYNAME", "TOT_POP", "BA_MALE", "BA_FEMALE", "Black_population", "Percentage Black", "cty_percentage")) 

write.csv(census_aggregate, "census_data_final.csv")
write.csv(census_aggregate, "census_data_final.xlsx")


## d) Governor's data 
## Filter for "African American" in the "Minority Ethnicity" column; using the "in" function 
## Convert the Term start column to a datetime object then filter for a start date after 1/1/2010 and before 1/1/2021

governors_data_cleaned = governors_data 
governors_data_cleaned$Ethnicity = governors_data_cleaned$`Minority
ethnicity`

drop_ind = c()
for (i in 1:nrow(governors_data_cleaned)) {
  if (race_check(governors_data_cleaned$Ethnicity[i]) == FALSE) {
    drop_ind = append(drop_ind, i)
  } 
}

governors_data_cleaned = governors_data_cleaned[-c(drop_ind),]


governors_data_cleaned$`Term_start` = lapply(governors_data_cleaned$`Term start`, start_date_parse_ag)
governors_data_cleaned$`Term_end` = lapply(governors_data_cleaned$`Term end`, end_date_parse_ag)

governors_data_final = governors_data_cleaned %>% filter((`Term_start` > 2010 & `Term_end` < 2020) | (`Term_end` > 2010  & `Term_end` < 2020) | (`Term_start` > 2010  & `Term_start` < 2020))


governors_data_final$`Term start` = as.numeric(as.character(governors_data_final$`Term start`))
governors_data_final$`Term end` = as.numeric(as.character(governors_data_final$`Term end`))
write.xlsx(governors_data_final, "governors_data_final.xlsx")



## Questions/assumptions:
# Counting Haitian American and Jamaican American as folks racialized as Black in the attorney general data 
# Counting Hispanic and Black alone as Black for % of US population
# Should I count the attorney general total as 43 or 50 since there are 7 states where the position is not elected? 


## e) State Supreme Court Justices
## No pre-processing necessary! 


## Having trouble with the term dates for a few of the spreadsheets; don't have time to figure it out now but will do it at some point 
