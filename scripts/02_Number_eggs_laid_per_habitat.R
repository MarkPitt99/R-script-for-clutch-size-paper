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
data <- read.xlsx("./data/Pitt_et_all_database.xlsx",
                  colNames=T,
                  sheet = 1)

# retain data appropriate for this analysis (see reasons in table)
data <- data %>% 
  filter(analysis_total_number_eggs == 'YES')

# data formatting
data$LOCATION <- as.factor(data$LOCATION)
data$NESTBOX <- as.factor(data$NESTBOX)
data$EXPERIMENTAL_GROUP <- as.factor(data$EXPERIMENTAL_GROUP)
data$julian_1st_egg <- as.numeric(data$julian_1st_egg)
data$julian_hatch_date <- as.numeric(data$julian_hatch_date)

# double check that no NA (i.e., 'missing data') in relevant columns for analysis
table(is.na(data$NUMBER_EGGS_LAID))
table(is.na(data$EXPERIMENTAL_GROUP))
table(is.na(data$HABITAT))
table(is.na(data$julian_1st_egg))

#####

##
##
##### Data analysis 1 : total number of eggs laid in the forest #####
##
##
data_forest <- data %>% 
  filter(HABITAT == 'forest')

##
## Model 1: model forest
number_eggs_model_forest_cmpoisson <- glmmTMB(NUMBER_EGGS_LAID ~ 
                                                EXPERIMENTAL_GROUP +
                                                scale(julian_1st_egg, scale = F),
                                              family = compois(link = "log"),
                                              data = data_forest)
# simulate residual
# simulate residuals (all tests passed)
number_eggs_model_forest_cmpoisson_residuals <- simulateResiduals(number_eggs_model_forest_cmpoisson, 
                                                                 n = 1000, 
                                                                 plot = T)
# LRT and summary
drop1(number_eggs_model_forest_cmpoisson, test = 'Chisq')
summary(number_eggs_model_forest_cmpoisson)

##
## table of results

## base table
number_eggs_model_forest00 <- number_eggs_model_forest_cmpoisson %>%
  tbl_regression(intercept = T,
                 label = list(
                   `(Intercept)` = "Intercept",
                   `EXPERIMENTAL_GROUP` = "Experimental group",
                   `scale(julian_1st_egg, scale = F)` = "First egg laying date"),
                 estimate_fun = ~ style_number(.x, digits = 3)) 

## add features
number_eggs_model_forest_table <- number_eggs_model_forest00 %>% 
  add_global_p(anova_fun = drop1_output) %>% 
  bold_p(t = 0.05) %>% 
  bold_labels() %>%
  italicize_levels() %>% 
  modify_table_body(fun = function(.){
    output <- left_join(x = .,
                        y = drop1_output(x=number_eggs_model_forest_cmpoisson) %>% 
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
gtsave(number_eggs_model_forest_table, "./tables/TABLE S3.html")

#####

##
##
##### Data analysis 2 : total number of eggs laid in the city #####
##
##
data_urban <- data %>% 
  filter(HABITAT == 'urban')

##
## Model 2: Gaussian model city
number_eggs_model_urban_cmpoisson <- glmmTMB(NUMBER_EGGS_LAID ~ 
                                                EXPERIMENTAL_GROUP +
                                                scale(julian_1st_egg, scale = F),
                                              family = compois(link = "log"),
                                              data = data_urban)
# simulate residual
# simulate residuals (all tests passed)
number_eggs_model_urban_cmpoisson_residuals <- simulateResiduals(number_eggs_model_urban_cmpoisson, 
                                                                  n = 1000, 
                                                                  plot = T)

# LRT and summary
drop1(number_eggs_model_urban_cmpoisson, test = 'Chisq')
summary(number_eggs_model_urban_cmpoisson)

##
## table of results

## base table
number_eggs_model_urban00 <- number_eggs_model_urban_cmpoisson %>%
  tbl_regression(intercept = T,
                 label = list(
                   `(Intercept)` = "Intercept",
                   `EXPERIMENTAL_GROUP` = "Experimental group",
                   `scale(julian_1st_egg, scale = F)` = "First egg laying date"),
                 estimate_fun = ~ style_number(.x, digits = 3)) 

## add features
number_eggs_model_urban_table <- number_eggs_model_urban00 %>% 
  add_global_p(anova_fun = drop1_output) %>% 
  bold_p(t = 0.05) %>% 
  bold_labels() %>%
  italicize_levels() %>% 
  modify_table_body(fun = function(.){
    output <- left_join(x = .,
                        y = drop1_output(x=number_eggs_model_urban_cmpoisson) %>% 
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
gtsave(number_eggs_model_urban_table, "./tables/TABLE S4.html")

#####


