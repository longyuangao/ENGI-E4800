setwd("~/Desktop/Fall 2023 Classes/Data Science & Ethics Capstone") 
library(tidyverse)
library(readxl)
library(dplyr)
library(lubridate)
library(openxlsx)


senate = read.xlsx("Black_senate.xlsx")
house = read.xlsx("Black_house.xlsx")
ag = read.xlsx("Attorney_general.xlsx")
gov = read.xlsx("governors_data_final.xlsx")
supreme = read_csv("Black State Supreme Court Justices - Sheet1.csv")
census = read_csv("census_data_final.csv")
state_scj = read_excel("State Supreme Court Justices.xlsx")
variable_coder <- read_xlsx("VARIABLE for coder X.xlsx")
house_reps = read_excel("House_reps.xlsx")



## Now that all my final dataframes are written, it's time to find the total number of Black representatives across the 5-6 offices I chose, the total number
## of people in these positions (using the most conservative approach possible, i.e. no seat changes), then find the % of Black representation BY STATE. 


senate_total = 100
house_total = 435
state_governor_total = 50 
attorney_general_total = 43 



## Quick calculations to find the total number of elected state supreme court justices in the US; all appointed states are excluded
elections = state_scj$`*Method of selection*` %>% unique()
elections = elections[c(1,3,5,7)]



state_scj = state_scj %>% filter(`*Method of selection*` %in% elections)
state_supreme_court_total = sum(state_scj$`*Number of justices*`)


black_reps_tot = nrow(senate) + nrow(house) + nrow(ag) + nrow(gov) + nrow(supreme)
total_rep = senate_total + house_total + state_governor_total + attorney_general_total + state_supreme_court_total
#percentage_black_rep 


# Quick way to get all 50 state names: 
library(datasets)
state_names = datasets::state.name


## Need to break this down by state:
## Final df should have a state column, total Black rep column, total rep column, and a % Black_rep column. 
## State column: State name 
## Total Black rep column: Create a function that iterates through each state in the state column and adds 1 for each match that a dataset has for said state (CAN PROBABLY USE SUMMARIZE + COUNT)
## Total rep column: Will need to do this manually based on each position; i.e. senate has 2 seats/state, etc. and then sum up the total for each state 
## % Black rep column: mutate the previous 2 columns and divide 


## Need to change all state column names to "State", need to get rid of US Virgin Islands & DC in each df 
house = rename(house, State = `State.or.Territory`)
senate = rename(senate, State = `State.or.Territory`)

all_dfs = list(ag,gov,house,senate,supreme)



clean_state_names = function(dataset) {
  dataset = dataset %>% filter(State %in% state_names)
}


ag = clean_state_names(ag)
gov = clean_state_names(gov)
house = clean_state_names(house)
senate = clean_state_names(senate)
supreme = clean_state_names(supreme)



count_df = data.frame(state_names, 'count'=0)


for (dataset in all_dfs) {
  df = dataset %>% group_by(State) %>% summarise(n = n())
  for (state in df$State) {
    if (state %in% count_df$state_names) {
      df1 = df %>% filter(State == state)
      new_reps = df1$n
      state_ind = which(count_df$state_names == state)
      print(state_ind)
      count_df[state_ind,2] = count_df[state_ind,2] + new_reps
    }
  }  
}










# test_df = count_df
# 
# t = ag %>% group_by(State) %>% summarise(n = n())
# 
# for (state in t$State) {
#   if (state %in% test_df$state_names) {
#     t1 = t %>% filter(State == state)
#     t2 = t1$n
#     state_ind = which(test_df$state_names == state)
#     print(state_ind)
#     test_df[state_ind,2] = test_df[state_ind,2] + t2
#     #merge_df = test_df %>% filter(state_names == state) %>% mutate(count = count + t2)
#     #left_join(test_df, merge_df, by = "state_names")
#   }
# }






## Total rep column: Will need to do this manually based on each position; i.e. senate has 2 seats/state, etc. and then sum up the total for each state 


state_reps = data.frame("State" = state_names, 'total_reps' = 0)
state_reps$senate = 2
state_reps = left_join(state_reps, house_reps, by = "State")
state_scj = state_scj %>% rename("State" = "*Court*")
state_reps = left_join(state_reps, state_scj, by = "State") %>% select(c(-"*Method of selection*", -"*Term length*"))
state_reps[is.na(state_reps)] = 0 
state_reps$attorney_general = 1
non_elected_ag = c("Alaska", "Hawaii", "New Hampshire", "New Jersey", "Wyoming", "Tennessee", "Maine")
state_reps[state_reps$State %in% non_elected_ag,"attorney_general"] = 0
state_reps$governor = 1

state_reps$total_reps = (state_reps$senate + state_reps$representatives + state_reps$attorney_general + state_reps$governor + state_reps$`*Number of justices*`)

count_df = count_df %>% rename("State" = "state_names")
state_reps = left_join(state_reps, count_df, by ="State")

state_reps$percent_black_rep = state_reps$count/state_reps$total_reps


write_csv(state_reps, "Black_representation.csv")
write.xlsx(state_reps, "Black_representation.xlsx")



## Joining the data; 
## Can impute all the counties with missing info for Black population with the state mean

census = census %>% rename("ALLCOUNTIES" = "CTYNAME", "STATE" = "STNAME")



rep_df = read_csv("Black_representation.csv")

left_join(variable_coder, census, by = c("ALLCOUNTIES", "STATE"))


## Need to do more parsing to the census data before I can merge it; some of the counties are the same but they are written differently,
## i.e. the word "County" is in the census data but now in the variable coder 

# Use https://pypi.org/project/fuzzy-pandas/ to merge the census and rep_df