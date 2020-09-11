library(tidyindexR)
library(tidyverse)
library(hammond)
all = rio::import("../UNESCO_ICD/reports/data_outputs/icd_raw_data.csv")
meta = rio::import("../UNESCO_ICD/reports/data_outputs/index_meta_data.csv")
#Standardise country names and clean up
tmp = all %>% mutate(country = hcountrycode(all$geocode)) %>%
  filter(complete.cases(country), variablename %in% meta$variablename) %>%
  filter(complete.cases(value)) %>%
  distinct()
#remove countries with not enough indicators
cutoff = 0.7 #70 percent availability
check = tmp %>% group_by(country) %>% summarise(n = length(unique(variablename))) %>%
  filter(n > cutoff*nrow(meta))
message("We are dropping:")
message(paste(setdiff(tmp$country, check$country), collapse = "\n"))
tmp = tmp %>% filter(country %in% check$country)
#remove indicators with not enough countries
cutoff = 100
check = tmp %>% group_by(variablename) %>% summarise(n = length(unique(geocode))) %>%
  filter(n > cutoff)
message("We are dropping:")
message(paste(setdiff(tmp$variablename, check$variablename), collapse = "\n"))
tmp = tmp %>% filter(variablename %in% check$variablename)
z = index_data_pad(tmp)
y = index_data_knn(z)
z = y %>% filter(year == max(year))
