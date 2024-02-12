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
##### Initial assessment of biases in control and experimental clutches per habitats #####
##
##

##
## laying dates
laying_date_model <- lm(julian_1st_egg ~ 
                         EXPERIMENTAL_GROUP +
                         HABITAT, 
                       data = data)
summary(laying_date_model)
drop1(laying_date_model, test = 'F')

##
## hatching dates
hatching_date_model <- lm(julian_hatch_date ~ 
                          EXPERIMENTAL_GROUP +
                          HABITAT, 
                        data = data)
summary(hatching_date_model)
drop1(hatching_date_model, test = 'F')

##
## no differences in laying or hatching dates across experimental groups within habitats
##

#####

##
##
##### Data analysis 1 : total number of eggs laid using Gaussian distribution #####
##
##
hist(data$NUMBER_EGGS_LAID)
summary(data$NUMBER_EGGS_LAID)

table(data$HABITAT, data$EXPERIMENTAL_GROUP)

# summary data
data %>% 
  group_by(HABITAT, EXPERIMENTAL_GROUP) %>% 
  summarise(mean_eggs = mean(NUMBER_EGGS_LAID),
            se_eggs = sd(NUMBER_EGGS_LAID)/sqrt(n()),
            n_eggs = n())
##
## Model 1: all interactions included using 'normal' Poisson error distibution
number_eggs_model_full_poisson <- glmmTMB(NUMBER_EGGS_LAID ~ 
                                             EXPERIMENTAL_GROUP : HABITAT + 
                                             EXPERIMENTAL_GROUP +
                                             HABITAT +
                                             poly(julian_1st_egg, 2)[,1] +
                                             poly(julian_1st_egg, 2)[,2],
                                           family = poisson('log'),
                                           data = data)
# simulate residuals 
number_eggs_model_full_poisson_residuals <- simulateResiduals(number_eggs_model_full_poisson, n = 1000, plot = T)

##
## very poor fit, underdispersion and deviation from normal residuals
##

##
## Model 2: all interactions included using Conway-Maxwell Poisson error distibution
number_eggs_model_full_cmpoisson <- glmmTMB(NUMBER_EGGS_LAID ~ 
                                            EXPERIMENTAL_GROUP : HABITAT + 
                                            EXPERIMENTAL_GROUP +
                                            HABITAT +
                                            poly(julian_1st_egg, 2)[,1] +
                                            poly(julian_1st_egg, 2)[,2],
                                          family = compois(link = "log"),
                                          data = data)
# simulate residuals 
number_eggs_model_full_cmpoisson_residuals <- simulateResiduals(number_eggs_model_full_cmpoisson, n = 1000, plot = T)

##
## good fit!!
##

# model results
summary(number_eggs_model_full_cmpoisson)
drop1(number_eggs_model_full_cmpoisson, test = 'Chisq')

##
## Model 3: quadratic date removed
number_eggs_model_cmpoisson <- glmmTMB(NUMBER_EGGS_LAID ~ 
                                              EXPERIMENTAL_GROUP : HABITAT + 
                                              EXPERIMENTAL_GROUP +
                                              HABITAT +
                                              scale(julian_1st_egg, scale = F),
                                            family = compois(link = "log"),
                                            data = data)
# model results
summary(number_eggs_model_cmpoisson)
drop1(number_eggs_model_cmpoisson, test = 'Chisq')

#####

##
##
##### Table of results #####
##
##

## base table
number_eggs_model00 <- number_eggs_model_cmpoisson %>%
  tbl_regression(intercept = T,
                 label = list(
                   `(Intercept)` = "Intercept",
                   `EXPERIMENTAL_GROUP` = "Experimental group",
                   `HABITAT` = "Habitat",
                   `scale(julian_1st_egg, scale = F)` = "First egg laying date"),
                 estimate_fun = ~ style_number(.x, digits = 3)) 

## add features
number_eggs_model_table <- number_eggs_model00 %>% 
  add_global_p(anova_fun = drop1_output) %>% 
  bold_p(t = 0.05) %>% 
  bold_labels() %>%
  italicize_levels() %>% 
  modify_table_body(fun = function(.){
    output <- left_join(x = .,
                        y = drop1_output(x=number_eggs_model_cmpoisson) %>% 
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
gtsave(number_eggs_model_table, "./tables/TABLE S2.html")

#####

##
##
##### Plotting model results #####
##
##

# df to calculate predictions
df_predict <-expand.grid(
  EXPERIMENTAL_GROUP=unique(data$EXPERIMENTAL_GROUP),
  HABITAT=unique(data$HABITAT)
)
df_predict$julian_1st_egg <- ifelse(df_predict$HABITAT == 'urban', 
                                    mean(data$julian_1st_egg[data$HABITAT == 'urban']),
                                    mean(data$julian_1st_egg[data$HABITAT == 'urban']))


# calculate predicted values and associated 95% confidence intervals
df_predict$NUMBER_EGGS_LAID <- {
  predict(number_eggs_model_cmpoisson, df_predict, type = 'response', se.fit = T)
  }$fit

# associated standard errors
df_predict$se <- { 
  predict(number_eggs_model_cmpoisson, df_predict, type = 'response', se.fit = T)
  }$se.fit 

##
## plot with model predictions
level_order <- c('urban', 'forest') 

plot_figure2 <- ggplot(data = data, 
                       aes(x = factor(HABITAT, level = level_order), 
                                         y = NUMBER_EGGS_LAID, 
                                         fill = EXPERIMENTAL_GROUP)) +
  geom_point(position = position_jitterdodge(dodge.width = 0.70,
                                             jitter.width = 0.13),
             size = 3.5,
             shape = 21,
             color = "white",
             alpha = 0.5) +
  geom_errorbar(data = df_predict, 
                 aes(y = NUMBER_EGGS_LAID, 
                     ymin =NUMBER_EGGS_LAID - se, 
                     ymax = NUMBER_EGGS_LAID + se),
                 position = position_dodge(width = 0.70),
                 width = 0) +
  geom_point(data = df_predict, 
             aes(x = factor(HABITAT, level = level_order), 
                 y = NUMBER_EGGS_LAID, 
                 fill = EXPERIMENTAL_GROUP),
             position = position_dodge(width = 0.70),
             color = "black", 
             size = 5,
             shape = 21) +
  scale_x_discrete(labels = c("Urban", "Forest")) +
  scale_y_continuous(breaks=c(4,6,8,10,12,14), limits=c(4,14))+
  theme_classic() +  
  theme(axis.title=element_text(size=20),
        axis.text=element_text(size=20)) +
  theme(legend.position = "top",
        legend.title=element_blank(),
        legend.text=element_text(size=13)) +
  scale_fill_manual(name = "Experimental Group", values = c("#67a9cf", "pink")) +
  scale_color_manual(name = "Experimental Group", values = c("#67a9cf", "pink")) +
  labs(x = "Habitat", y = "Number of eggs laid ± SE")

# to save plot
ggsave(filename = "./plots/Figure2.png",
       plot = plot_figure2, 
       device = "png", 
       width = 100, 
       height = 180, 
       units = "mm")

#####

##
## effect of outlier in forest

# Model 1 without outlier 
number_eggs_model_cmpoisson_outlier <- glmmTMB(NUMBER_EGGS_LAID ~ 
                                                EXPERIMENTAL_GROUP : HABITAT + 
                                                EXPERIMENTAL_GROUP +
                                                HABITAT +
                                                julian_1st_egg,
                                              family = compois(link = "log"),
                                              data = data %>% 
                                                mutate(remove = ifelse(HABITAT == 'forest' & NUMBER_EGGS_LAID < 7, 1, 0)) %>% 
                                                filter(remove == 0))
summary(number_eggs_model_cmpoisson_outlier)
