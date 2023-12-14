###
###
#' 
#' Script for:
#' 
#' 
###
###

# Clear memory to make sure there are not files loaded that could cause problems
rm(list=ls())

##
##
##### libraries and functions #####
##
##
library(openxlsx) # v4.2.5.2
library(dplyr)    # v1.1.2
library(ggplot2)  # v3.4.2

#####

##
##
##### Read and formatting data #####
##
##

# read clutch dataset to filter for those clutches that can be included in this analysis
data_clutches <- read.xlsx("./data/Pitt_et_all_database.xlsx",
                           colNames=T,
                           sheet = 1)
data_clutches <- data_clutches %>% 
  filter(analysis_egg_volume == 'YES') %>% 
  select(analysis_egg_volume, NESTBOX, NUMBER_EGGS_LAID) %>% 
  mutate(NESTBOX = as.factor(NESTBOX))

# egg volume data
data <- read.xlsx("./data/Pitt_et_all_database.xlsx",
                  colNames=T,
                  sheet = 2)

# select eggs from clutches that should be included in this analysis
data <- data %>% 
  filter(NESTBOX %in% data_clutches$NESTBOX) %>% # from 1794 to 1677
  filter(!is.na(volume)) # from 1677 to 1571


# data formatting
data$LOCATION <- as.factor(data$LOCATION)
data$NESTBOX <- as.factor(data$NESTBOX)
data$EXPERIMENTAL_GROUP <- as.factor(data$EXPERIMENTAL_GROUP)
data$julian_earliest_lay_date <- as.numeric(data$julian_earliest_lay_date)
data$julian_latest_lay_date <- as.numeric(data$julian_latest_lay_date)
data$LATEST_LAYING_ORDER <- as.numeric(data$LATEST_LAYING_ORDER)
data$EARLIEST_LAYING_ORDER <- as.numeric(data$EARLIEST_LAYING_ORDER)
data$Group <- as.factor(data$Group)

# assigning eggIDs
data <- data %>%
  group_by(NESTBOX, EGG_LABEL, EXPERIMENTAL_GROUP) %>% 
  mutate(eggID=cur_group_id())

# merging data on total number of eggs to egg table
data <- left_join(x = data, 
                  y = data_clutches %>% select(NESTBOX, NUMBER_EGGS_LAID), 
                  by = 'NESTBOX')
  
#####

##
##
##### filtering data for final analysis #####
##
##

data01 <- data %>% # 1571 eggs
  filter(!is.na(NUMBER_EGGS_LAID)) %>% # 1514 egs
  filter(LATEST_LAYING_ORDER <= 9) %>% # 1355 eggs
  filter(accurate == 1) # 1160 eggs measured on laying date included

# number of eggs
length(unique(data01$eggID))
length(unique(data01$NESTBOX))

##
##
##### save data ######
##
##
saveRDS(object = data01, file = './data/individual_egg_data.RDS')

## continue in next scripts

#####

