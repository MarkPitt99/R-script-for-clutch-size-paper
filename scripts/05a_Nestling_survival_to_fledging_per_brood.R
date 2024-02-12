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
data <- read.xlsx("./data/Pitt_et_all_database.xlsx",
                  colNames=T,
                  sheet = 1)

# retain data appropriate for this analysis (see reasons in table)
data <- data %>% 
  filter(analysis_nestling_survival == 'YES')

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
##### model number of nestlings alive at a given time point #####
##
##

## reformat data for analysis
df_model <- data %>% 
  select(IN_NEST_CLUTCH_SIZE, julian_hatch_date, NESTBOX, 
         HABITAT, EXPERIMENTAL_GROUP,
         BROODSIZE_DAY3_POSTMANIPULATION,
         BROODSIZE_DAY7,
         BROODSIZE_DAY13,
         NUMBER_FLEDGLINGS) %>% 
  pivot_longer(cols = c(BROODSIZE_DAY3_POSTMANIPULATION,
                        BROODSIZE_DAY7,
                        BROODSIZE_DAY13,
                        NUMBER_FLEDGLINGS), 
               values_to = 'number_chicks_alive', 
               names_to = 'time_point') %>% 
  filter(!is.na(julian_hatch_date)) %>% 
  mutate(failed_nestlings = IN_NEST_CLUTCH_SIZE - number_chicks_alive) %>% 
  mutate(time_point =  factor(case_match(time_point, 
                                  'BROODSIZE_DAY3_POSTMANIPULATION' ~ 'Day2', 
                                  'BROODSIZE_DAY7' ~ 'Day6',
                                  'BROODSIZE_DAY13' ~ 'Day13',
                                  'NUMBER_FLEDGLINGS' ~ 'Fledged'))) %>% 
  mutate(time_point = factor(time_point, levels = c('Day2', 'Day6', 'Day13', 'Fledged')))

# model
nestling_count_model_full <- glmmTMB(cbind(number_chicks_alive, failed_nestlings) ~
                                       EXPERIMENTAL_GROUP:HABITAT: time_point +
                                       HABITAT: time_point +
                                       EXPERIMENTAL_GROUP: time_point +
                                       EXPERIMENTAL_GROUP:HABITAT+
                                       
                                       EXPERIMENTAL_GROUP +
                                       HABITAT +
                                       time_point +
                                       
                                       poly(julian_hatch_date, 2)[,2] +
                                       poly(julian_hatch_date, 2)[,1] +
                                       (1|NESTBOX),
                                     family = binomial,
                                     data = df_model)

# simulate residuals 
nestling_count_model_full_residuals <- simulateResiduals(nestling_count_model_full, n = 1000, plot = T)

drop1(nestling_count_model_full, test = 'Chisq')
summary(nestling_count_model_full)

#####

##
##
##### model without date interaction #####
##
##
nestling_count_model <- glmmTMB(cbind(number_chicks_alive, failed_nestlings) ~
                                     #EXPERIMENTAL_GROUP:HABITAT:time_point+
                                     HABITAT:time_point+
                                     EXPERIMENTAL_GROUP:time_point+
                                     EXPERIMENTAL_GROUP:HABITAT+
                                     
                                     EXPERIMENTAL_GROUP +
                                     HABITAT +
                                     time_point +
                                     
                                     scale(julian_hatch_date, scale = F) +
                                     (1|NESTBOX),
                                   family = binomial,
                                   data = df_model)

drop1(nestling_count_model, test = 'Chisq')
summary(nestling_count_model)

#####

##
##
##### model without (non sign) 3-way interaction #####
##
##
nestling_count_model01 <- glmmTMB(cbind(number_chicks_alive, failed_nestlings) ~
                                  #EXPERIMENTAL_GROUP:HABITAT:time_point+
                                  HABITAT:time_point+
                                  EXPERIMENTAL_GROUP:time_point+
                                  EXPERIMENTAL_GROUP:HABITAT+
                                  
                                  EXPERIMENTAL_GROUP +
                                  HABITAT +
                                  time_point +
                                  
                                  scale(julian_hatch_date, scale = F) +
                                  (1|NESTBOX),
                                family = binomial,
                                data = df_model)

drop1(nestling_count_model01, test = 'Chisq')
summary(nestling_count_model01)

#####

##
##
##### model without (non sign) 2-way interaction #####
##
##
nestling_count_model02 <- glmmTMB(cbind(number_chicks_alive, failed_nestlings) ~
                                    #EXPERIMENTAL_GROUP:HABITAT:time_point+
                                    HABITAT:time_point+
                                    EXPERIMENTAL_GROUP:time_point+
                                    #EXPERIMENTAL_GROUP:HABITAT+
                                    
                                    EXPERIMENTAL_GROUP +
                                    HABITAT +
                                    time_point +
                                    
                                    scale(julian_hatch_date, scale = F) +
                                    (1|NESTBOX),
                                  family = binomial,
                                  data = df_model)

drop1(nestling_count_model02, test = 'Chisq')
summary(nestling_count_model02)

#####

##
##
##### Table of results #####
##
##

## base table
nestling_count_table00 <- nestling_count_model02 %>%
  tbl_regression(intercept = T,
                 label = list(
                   `(Intercept)` = "Intercept",
                   `EXPERIMENTAL_GROUP` = "Experimental group",
                   `HABITAT` = 'Habitat',
                   `time_point` = 'Time point',
                   `scale(julian_hatch_date, scale = F)` = "Hatching date"),
                 estimate_fun = ~ style_number(.x, digits = 3)) 

## add features
nestling_count_table <- nestling_count_table00 %>% 
  add_global_p(anova_fun = drop1_output) %>% 
  bold_p(t = 0.05) %>% 
  bold_labels() %>%
  italicize_levels() %>% 
  modify_table_body(fun = function(.){
    output <- left_join(x = .,
                        y = drop1_output(x=nestling_count_model) %>% 
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
gtsave(nestling_count_table, "./tables/TABLE S9.html")

#####

##
##
##### Plotting model results #####
##
##

# df to calculate predictions
df_predict <-expand.grid(
  time_point = unique(df_model$time_point),
  EXPERIMENTAL_GROUP = unique(df_model$EXPERIMENTAL_GROUP),
  HABITAT = unique(df_model$HABITAT),
  NESTBOX = unique(df_model$NESTBOX)[1]
)
df_predict$julian_hatch_date <- ifelse(df_predict$HABITAT == 'urban', 
                                    mean(df_model$julian_hatch_date[df_model$HABITAT == 'urban']),
                                    mean(df_model$julian_hatch_date[df_model$HABITAT == 'forest']))


# calculate predicted values and associated 95% confidence intervals
df_predict$number_chicks_alive_prop <- {
  predict(nestling_count_model, df_predict, re.form = NA, se.fit = T, type = 'response')
}$fit

# associated standard errors
df_predict$ci <- { 
  predict(nestling_count_model, df_predict, type = 'response', se.fit = T)
}$se.fit


df_means <- df_model %>% 
  group_by(HABITAT, EXPERIMENTAL_GROUP, time_point) %>% 
  summarise(IN_NEST_CLUTCH_SIZE = mean(IN_NEST_CLUTCH_SIZE))

df_predict <- left_join(x = df_predict,
                        y = df_means,
                        by = c('HABITAT', 'EXPERIMENTAL_GROUP', 'time_point')) %>% 
  mutate(number_chicks_alive = number_chicks_alive_prop * IN_NEST_CLUTCH_SIZE,
         number_chicks_alive_low = (number_chicks_alive_prop-ci) * IN_NEST_CLUTCH_SIZE,
         number_chicks_alive_high = (number_chicks_alive_prop+ci) * IN_NEST_CLUTCH_SIZE)


##
## plot with model predictions
# adjust name of habitat factors for plot
df_model$HABITAT2 <- ifelse(df_model$HABITAT == 'urban', 'Urban', 'Forest')
df_predict$HABITAT2 <- ifelse(df_predict$HABITAT == 'urban', 'Urban', 'Forest')
level_order <- c('Urban', 'Forest') 

nestling_count <- ggplot(data = df_model, 
       aes(x = time_point, 
           y = number_chicks_alive, 
           fill = EXPERIMENTAL_GROUP)) +
  facet_grid(~factor(HABITAT2, level = level_order)) +
  geom_line(data = df_predict, 
            aes(x = time_point, 
                y = number_chicks_alive, 
                group = EXPERIMENTAL_GROUP,
                color = EXPERIMENTAL_GROUP),
            position = position_dodge(width = 0.50)) +
  scale_color_manual(name = "Treatment group", values = c("#67a9cf", "pink")) +
  geom_point(position = position_jitterdodge(jitter.width = 0.15, 
                                             dodge.width = 0.5),
             color= 'white',
             shape = 21,
             alpha = 0.5, 
             size=3) +
  geom_errorbar(data = df_predict, 
                 aes(y = number_chicks_alive, 
                     ymin = number_chicks_alive_low, 
                     ymax = number_chicks_alive_high),
                 position = position_dodge(width = 0.50),
                 width = 0) +
  geom_point(data = df_predict, 
             aes(x = time_point, 
                 y = number_chicks_alive, 
                 fill = EXPERIMENTAL_GROUP),
             position = position_dodge(width = 0.50),
             color = "black", 
             size = 8,
             shape = 21) +
  theme_classic() + 
  theme(axis.title=element_text(size=20),
        axis.text=element_text(size=15)) +
  scale_x_discrete(labels= c("Day 2", "Day 6", "Day 12", "Fledged"))+
  scale_y_continuous(limits=c(0,15), breaks=c(0,3,6,9,12,15))+
  theme(legend.position = "top",
        legend.title=element_text(size=20),
        legend.text=element_text(size=20),
        strip.text = element_text(size=20)) +
  scale_fill_manual(name = "Treatment group", values = c("#67a9cf", "pink")) +
  labs(x = "Age (days after hatching)", y = "Number of nestlings alive")


## saving plot
saveRDS(object = nestling_count, file = './plots/Figure4a.RDS')

ggsave(filename = "./plots/Figure4a.png",
       plot = nestling_count, 
       device = "png", 
       width = 200, 
       height = 150, 
       units = "mm")
