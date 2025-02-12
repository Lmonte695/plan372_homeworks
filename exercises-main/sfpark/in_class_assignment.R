rm(list = ls())

library(tidyverse)

setwd("C:/Users/Lily/Documents/collegefiles/PLAN 372/plan372_hmks/exercises-main/sfpark/data")

sfpark = read.csv("sfpark.csv")

mean(sfpark$entries)

unique(sfpark$district)


total_entries = sfpark %>%
  group_by(date, facility) %>%
  summarize(entries = sum(entries),
            exits = sum(exits)) %>%
  mutate(date = as.Date(date, format = "%d/%m/%y")) %>%
  ungroup

avg_entries = sfpark %>%
  group_by(facility) %>%
  summarize(mean_entry = round(mean(entries, na.rm = T)),
            mean_exits = round(mean(exits, na.rm = T)),
            median_entry = round(median(entries, na.rm = T)),
            median_exits = round(median(exits, na.rm = T)))
ungroup

exercise1 = sfpark %>%
  group_by(date, facility) %>%
  summarize(entries = sum(entries),
            exits = sum(exits)) %>%
  group_by(facility) %>%
  summarize(mean_entry = round(mean(entries, na.rm = T)),
            mean_exits = round(mean(exits, na.rm = T)),
            median_entry = round(median(entries, na.rm = T)),
            median_exits = round(median(exits, na.rm = T))) %>%
  ungroup



exercise2 = sfpark %>%
  mutate(date2 = as.Date(date, format = "%m/%d/%Y"),
         weekday = wday(date2),
         weekday_lbl = wday(date2, label = T)) %>% 
  group_by(date2) %>% 
  ungroup

exercise3 = sfpark %>% 
  mutate(date2 = as.Date(date, format = "%m/%d/%Y"),
         weekday = wday(date2),
         weekday_lbl = wday(date2, label = T),
         year = year(date2),
         month = month(date2)) %>% 
  group_by(facility, year)%>% 
  summarize(mean(entries)) %>%
  filter(exercise3$mean(entries) == max(exercise3$mean(entries))) %>%
  ungroup

wide_data = total_entries %>% 
  mutate(date = as.Date(date, format = "%m/%d/%y"),
         year = year(date)) %>% 
  group_by(facility, year) %>%
  summarize(entries = mean(entries)) %>% 
  ungroup %>% 
  pivot_wider(names_from = year,
              values_from = entries)
  

