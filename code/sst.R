# notes ----
# Sea Surface data prep
# sarah.power@alaska.gov
# 11/08/2018

# load ----
library(tidyverse)
library(RDS)
library(here)
options(scipen=999)

# data ----
SSTdf <- readRDS('data/mur_SST_stat6_all_columns.rds') %>%
  select(STAT_AREA, NMFSAREA, date, sst.mean, year, month, julian, week) %>%
  rename_all(tolower) %>%
  filter(nmfsarea == "620") 

x <- SSTdf %>%
  group_by(year, month) %>%
  summarize(month_mean = mean(sst.mean))

write.csv(x, 'data/month620sst.csv')
  

# analysis ----