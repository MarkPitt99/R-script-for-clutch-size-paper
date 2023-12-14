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
library(tidyr)    # v1.3.0
library(lme4)     # v1.1.34
library(glmmTMB)  # v1.1.7
library(DHARMa)   # v0.10.4
library(gt)       # v0.9.0
library(gtsummary)# v1.7.2
library(ggplot2)  # v3.4.2

# function to include drop1 output in gtsummary tables
source("./scripts/00_functions/PCL_drop1_output.R")

#####

##
##
##### Read and formatting data #####
##
##

##
## clutch data
data_clutches <- read.xlsx("./data/Pitt_et_all_database.xlsx",
                           colNames=T,
                           sheet = 1)

data_clutches <- data_clutches %>% 
  filter(analysis_nestling_survival == 'YES') %>% 
  select(analysis_nestling_survival, NESTBOX, EXPERIMENTAL_GROUP, NUMBER_HATCHED_EGGS) %>% 
  mutate(NESTBOX = as.factor(NESTBOX))

##
## nestling survival data
data <- read.xlsx("./data/Pitt_et_all_database.xlsx",
                  colNames=T,
                  sheet = 3)

# retain data appropriate for this analysis (see reasons in table)
data <- data %>% 
  filter(nestbox_rearing %in% data_clutches$NESTBOX) %>%
  mutate(nestbox_rearing = as.character(nestbox_rearing)) %>% 
  left_join(x = .,
            y = data_clutches %>% 
              select(nestbox_rearing = NESTBOX, EXPERIMENTAL_GROUP, NUMBER_HATCHED_EGGS) %>% 
              mutate(nestbox_rearing = as.character(nestbox_rearing)),
            by = 'nestbox_rearing')


# data formatting
data$julian_hatch <- as.numeric(data$julian_hatch)

# double check that no NA (i.e., 'missing data') in relevant columns for analysis
table(is.na(data$NUMBER_EGGS_LAID))
table(is.na(data$EXPERIMENTAL_GROUP))
table(is.na(data$HABITAT))
table(is.na(data$julian_1st_egg))

length(unique(data$nestbox_hatch))
length(unique(data$nestbox_rearing))

#####

##
##
##### model number of nestlings alive at a given time point #####
##
##

##
## TO BE CONTINUED





