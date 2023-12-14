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
## individual nestling data
data_nestlings <- read.xlsx("./data/Pitt_et_all_database.xlsx",
                            colNames=T,
                            sheet = 3)

data_nestlings <- data_nestlings %>% 
  select(RING_NUMBER, julian_hatch, habitat_hatch, nestbox_hatch, habitat_rearing, nestbox_rearing)

##
## read clutch dataset to filter for those clutches that can be included in this analysis
data_clutches <- read.xlsx("./data/Pitt_et_all_database.xlsx",
                           colNames=T,
                           sheet = 1)

data_clutches <- data_clutches %>% 
  filter(analysis_nestling_survival == 'YES') %>% 
  select(analysis_nestling_survival, NESTBOX, EXPERIMENTAL_GROUP, n_sibs2 = BROODSIZE_DAY3_PREMANIPULATION) %>% 
  mutate(NESTBOX = as.factor(NESTBOX))

##
## nestling weights data
data <- read.xlsx("./data/Pitt_et_all_database.xlsx",
                  colNames=T,
                  sheet = 4)


##
##
## filtering and merging all datasets

# adding nestling info
data <- data %>% 
  filter(AGE == 6) %>% 
  filter(NESTBOX %in% data_clutches$NESTBOX) %>% 
  left_join(., 
            y = data_nestlings, 
            by = 'RING_NUMBER') %>% 
  mutate(nestbox_hatch = as.character(nestbox_hatch))

# adding egg removal experimental group
data <- data %>%   
  left_join(.,
            y = data_clutches %>% 
              mutate(NESTBOX = as.character(NESTBOX)) %>% 
              select(nestbox_hatch = NESTBOX, EXPERIMENTAL_GROUP, n_sibs2), # add egg removal group of the nestbox of hatching
            by = 'nestbox_hatch') %>% 
  mutate(julian_hatch = as.numeric(julian_hatch)) %>% 
  filter(!is.na(EXPERIMENTAL_GROUP))

head(data) # data ready for analysis
summary(data)

length(unique(data$nestbox_hatch))
length(unique(data$nestbox_rearing))

table(is.na(data$habitat_rearing))
table(is.na(data$habitat_hatch))
table(is.na(data$EXPERIMENTAL_GROUP))
table(is.na(data$time_category))
table(is.na(data$n_sibs2))
table(is.na(data$julian_hatch))
table(is.na(data$nestbox_hatch))
table(is.na(data$nestbox_rearing))

#####

##
##
##### analysis of mass day 6 #####
##
##
mass_d6_model_full <-lmer(WEIGHT~
                            habitat_rearing:EXPERIMENTAL_GROUP +
                            habitat_hatch:EXPERIMENTAL_GROUP +
                            
                            EXPERIMENTAL_GROUP +
                            habitat_rearing +
                            habitat_hatch +
                            
                            time_category +
                            scale(n_sibs2, scale=F) +
                            poly(julian_hatch, 2)[,2] +
                            poly(julian_hatch, 2)[,1]+
                            (1|nestbox_hatch) +
                            (1|nestbox_rearing),
                          data = data)
# simulate residuals 
weight_d6_model_full_residuals <- simulateResiduals(mass_d6_model_full, n = 1000, plot = T) # not too bad

# model results
summary(mass_d6_model_full)
drop1(mass_d6_model_full, test = 'Chisq')

# model without interactions
mass_d6_model <-lmer(WEIGHT~

                       EXPERIMENTAL_GROUP +
                       habitat_rearing +
                       habitat_hatch +
                       
                       time_category +
                       scale(n_sibs2, scale=F) +
                       scale(julian_hatch, scale=F) +
                       (1|nestbox_hatch) +
                       (1|nestbox_rearing),
                     data = data)
# model results
summary(mass_d6_model)
drop1(mass_d6_model, test = 'Chisq')

##
##
##### Table of results #####
##
##

## base table
weight_d6_table00 <- mass_d6_model %>%
  tbl_regression(intercept = T,
                 label = list(
                   `(Intercept)` = "Intercept",
                   `EXPERIMENTAL_GROUP` = "Experimental group",
                   `habitat_rearing` = 'Habitat of rearing',
                     `habitat_hatch` = 'Habitat of hatching',
                   `time_category` = 'Time of the day',
                   `scale(n_sibs2, scale = F)` = 'Number of siblings',
                   `scale(julian_hatch, scale = F)` = "Hatching date"),
                 estimate_fun = ~ style_number(.x, digits = 3)) 

## add features
weight_d6_table <- weight_d6_table00 %>% 
  add_global_p(anova_fun = drop1_output) %>% 
  bold_p(t = 0.05) %>% 
  bold_labels() %>%
  italicize_levels() %>% 
  modify_table_body(fun = function(.){
    output <- left_join(x = .,
                        y = drop1_output(x=mass_d6_model) %>% 
                          dplyr::select(variable = term, Chisq=statistic, df),
                        by = "variable")
    output$df <- ifelse(output$row_type == "label",  output$df, NA)
    output$Chisq <- ifelse(output$row_type == "label",  output$Chisq, NA)
    return(output)
  }) %>% 
  modify_fmt_fun(c(Chisq) ~ function(x) style_number(x, digits = 2)) %>%
  modify_fmt_fun(c(std.error) ~ function(x) style_number(x, digits = 3)) %>%
  modify_fmt_fun(c(p.value) ~ function(x) style_number(x, digits = 3)) %>%
  modify_table_body(~.x %>% dplyr::relocate(p.value, .after = df)) %>% 
  modify_header(label ~ "**Fixed effect**") %>% 
  modify_header(std.error ~ "**SE**") %>%
  modify_header(estimate ~ "**Estimate**") %>%
  modify_header(df ~ "**df**") %>% 
  modify_header(Chisq ~ html("<b>&chi;<sup>2</sup></b>")) %>% 
  as_gt() %>% 
  opt_footnote_marks(marks = "LETTERS")


##
## save table
gtsave(weight_d6_table, "./tables/TABLE S8.html")

#####

