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

data_1 = data_1 %>% mutate('Time_1' = case_when(dummy > 1 ~ "Yes", 
                                                dummy < 2 ~ "No"))

data_2 = data_2 %>% mutate('Time_2' = case_when(dummy > 1 ~ "Yes", 
                                                dummy < 2 ~ "No"))

data_3 = data_3 %>% mutate('Time_3' = case_when(dummy > 0 ~ "Yes", 
                                                dummy < 1 ~ "No"))

alluvial_df = left_join(data_1, data_2, by = c("STATE", "ALLCOUNTIES")) %>% left_join(data_3, by = c("STATE", "ALLCOUNTIES")) %>% select(c("STATE", "ALLCOUNTIES", "Time_1", "Time_2", "Time_3"))

alluvial_df.step3 = alluvial_df |> 
  select(Time_1, Time_2, Time_3) |> 
  group_by(Time_1, Time_2, Time_3) |> 
  mutate(n =n()) |> unique() |> ungroup() |>
  mutate(time_from = Time_2,
         time_to = Time_3,
         step_from = 2,
         step_to = 3) |>
  select(-c(Time_1, Time_2, Time_3))

alluvial_df.step2 = alluvial_df |>
  select(Time_1, Time_2) |> 
  group_by(Time_1, Time_2) |> 
  mutate(n =n()) |> unique() |> ungroup() |>
  mutate(time_from = Time_1,
         time_to = Time_2,
         step_from = 1,
         step_to = 2) |>
  select(-c(Time_1, Time_2))

final_alluvial.df <- rbind(alluvial_df.step2, alluvial_df.step3)

write_csv(final_alluvial.df, "final_alluvial_df_again.csv")
