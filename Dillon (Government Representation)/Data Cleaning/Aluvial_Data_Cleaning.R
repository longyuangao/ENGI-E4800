library(dplyr)
library(tidyverse)
library(readr)

data = read.csv("full_data_final.csv")

data = data %>% select(c(-"X", -"GISJOIN", -"STATEFIPS", -"COUNTYFIPS", -"cty_percentage"))

time_1 = c("EP.SV1", "EP.LRA1", "MM.LRA1")
time_2 = c("GR.SV2", "GR.LRA2", "EP.SV2", "EP.LRA2")
time_3 = c("EP.SV3", "GR.LRA3")

data_1 = data %>% select(c("STATE", "ALLCOUNTIES", all_of(time_1)))

data_2 = data %>% select(c("STATE", "ALLCOUNTIES", all_of(time_2)))

data_3 = data %>% select(c("STATE", "ALLCOUNTIES", all_of(time_3)))


data_1$dummy = data_1$EP.SV1 + data_1$EP.LRA1 + data_1$MM.LRA1
data_2$dummy = data_2$GR.SV2 + data_2$GR.LRA2 + data_2$EP.SV2 + data_2$EP.LRA2
data_3$dummy = data_3$EP.SV3 + data_3$GR.LRA3

## we are defining the presence of significant racism by the majority (at least 50%) of all variables in the time period having a '1'

data_1 = data_1 %>% mutate('Time 1' = case_when(dummy > 1 ~ "Yes", 
                                                dummy < 2 ~ "Naur"))

data_2 = data_2 %>% mutate('Time 2' = case_when(dummy > 1 ~ "Yes", 
                                                dummy < 2 ~ "Naur"))

data_3 = data_3 %>% mutate('Time 3' = case_when(dummy > 0 ~ "Yes", 
                                                dummy < 1 ~ "Naur"))

alluvial_df = left_join(data_1, data_2, by = c("STATE", "ALLCOUNTIES")) %>% left_join(data_3, by = c("STATE", "ALLCOUNTIES")) %>% select(c("STATE", "ALLCOUNTIES", "Time 1", "Time 2", "Time 3"))

write_csv(alluvial_df, "alluvial.csv")