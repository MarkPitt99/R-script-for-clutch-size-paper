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
library(lme4)     # v1.1.34
library(rptR)     # v0.9.22

#####

##
##
##### data #####
##
##
data <- readRDS(file = './data/individual_egg_data.RDS')
head(data)
length(unique(data$eggID)) # number of eggs
length(unique(data$NESTBOX))     # number of nest-boxes

#####

##
##
##### Part 1: REPEATABILITY ANALYSIS #####
##
##
rep_volume_model <- rpt(volume ~
                          LATEST_LAYING_ORDER:EXPERIMENTAL_GROUP:HABITAT +
                          LATEST_LAYING_ORDER:EXPERIMENTAL_GROUP +
                          LATEST_LAYING_ORDER:HABITAT +
                          EXPERIMENTAL_GROUP:HABITAT +
                          LATEST_LAYING_ORDER +
                          EXPERIMENTAL_GROUP +
                          HABITAT +
                          poly(julian_latest_lay_date, 2)[,2] +
                          poly(julian_latest_lay_date, 2)[,1]+
                          (1|NESTBOX) +
                          (1|eggID),
                        grname = c("NESTBOX", "eggID"), 
                        data = data,
                        datatype = "Gaussian", 
                        nboot = 1000, 
                        npermut = 0)
summary(rep_volume_model)

#####

##
##
##### Egg weight and egg volume correlation (Only includes the removed eggs that were weighed) #####
##
##
weight <- data %>%
  group_by(eggID) %>% 
  summarise(mean_volume = mean(volume),
            weight = mean(weight_g)) %>% 
  filter(!is.na(weight))

cor.test(weight$weight, weight$mean_volume)#correlation test to see if egg weight and egg volume are correlated

#####