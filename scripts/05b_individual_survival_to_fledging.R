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

# add weight on day 2

## data weights
data_weights <- read.xlsx("./data/Pitt_et_all_database.xlsx",
                          colNames=T,
                          sheet = 4) %>% 
  filter(AGE == 2) %>% 
  select(RING_NUMBER, weight_2 = WEIGHT)
head(data_weights)

# combine data
data <- left_join(x = data,
                  y = data_weights,
                  by = 'RING_NUMBER')
table(is.na(data$weight_2))

data <- data %>% 
  filter(!is.na(weight_2)) %>% # remove obs with no weight data
  filter(!is.na(fate)) # remove NA in survival

# data formatting
data$julian_hatch <- as.numeric(data$julian_hatch)
data$survived <- ifelse(data$fate == 'DEAD', 0, 1)


table(data$survived)
length(unique(data$nestbox_hatch))
length(unique(data$nestbox_rearing))

#####

##
##
##### data summaries #####
##
##
head(data)
data %>% 
  group_by(habitat_rearing) %>% 
  summarise(mean_survival = mean(survived, na.rm = T))

#####

##
##
##### model number of nestlings alive at a given time point #####
##
##
# model
nestling_survival_model_full <- glmmTMB(survived ~
                                          EXPERIMENTAL_GROUP:habitat_rearing +
                                          
                                          EXPERIMENTAL_GROUP +
                                          habitat_hatch +
                                          habitat_rearing +
                                          weight_2 +
                                          
                                          poly(julian_hatch, 2)[,2] +
                                          poly(julian_hatch, 2)[,1] +
                                          (1|nestbox_hatch) +
                                          (1|nestbox_rearing),
                                        family = binomial,
                                        data = data)

# simulate residuals 
nestling_survival_model_residuals <- simulateResiduals(nestling_survival_model_full, n = 1000, plot = T)

drop1(nestling_survival_model_full, test = 'Chisq')
summary(nestling_survival_model_full)

#####

##
##
##### model without date interaction #####
##
##
nestling_survival_model <- glmmTMB(survived ~
                                     EXPERIMENTAL_GROUP : habitat_rearing +

                                     EXPERIMENTAL_GROUP +
                                     habitat_hatch +
                                     habitat_rearing +
                                     weight_2 +
                                     
                                     scale(julian_hatch, scale = F) +
                                     (1|nestbox_hatch) +
                                     (1|nestbox_rearing),
                                   family = binomial,
                                   data = data)

drop1(nestling_survival_model, test = 'Chisq')
summary(nestling_survival_model)

#####

##
##
##### model without non-sign interaction #####
##
##
nestling_survival_model_final <- glmmTMB(survived ~ 

                                           EXPERIMENTAL_GROUP +
                                           habitat_hatch +
                                           habitat_rearing +
                                           weight_2 +
                                           
                                           scale(julian_hatch, scale = F) +
                                           (1|nestbox_hatch) +
                                           (1|nestbox_rearing),
                                         family = binomial,
                                         data = data)

drop1(nestling_survival_model_final, test = 'Chisq')
summary(nestling_survival_model_final)

#####
##
##
##### Table of results #####
##
##

## base table
nestling_survival_table00 <- nestling_survival_model_final %>%
  tbl_regression(intercept = T,
                 label = list(
                   `(Intercept)` = "Intercept",
                   `EXPERIMENTAL_GROUP` = "Experimental group",
                   `habitat_hatch` = 'Habitat of hatching',
                   `habitat_rearing` = 'Habitat of rearing',
                   `weight_2` = 'Hatching weight',
                   `scale(julian_hatch, scale = F)` = "Hatching date"),
                 estimate_fun = ~ style_number(.x, digits = 3)) 

## add features
nestling_survival_table <- nestling_survival_table00 %>% 
  add_global_p(anova_fun = drop1_output) %>% 
  bold_p(t = 0.05) %>% 
  bold_labels() %>%
  italicize_levels() %>% 
  modify_table_body(fun = function(.){
    output <- left_join(x = .,
                        y = drop1_output(x=nestling_survival_model_final) %>% 
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
gtsave(nestling_survival_table, "./tables/TABLE S10.html")

#####

##
##
##### Plotting model results #####
##
##

# df to calculate predictions
df_predict <- expand.grid(
  weight_2 = mean(data$weight_2),
  EXPERIMENTAL_GROUP = unique(data$EXPERIMENTAL_GROUP),
  habitat_hatch = unique(data$habitat_hatch),
  habitat_rearing = unique(data$habitat_rearing),
  nestbox_hatch = unique(data$nestbox_hatch)[1],
  nestbox_rearing = unique(data$nestbox_rearing)[1]
)

df_predict$julian_hatch <- ifelse(df_predict$habitat_hatch == 'urban', 
                                       mean(data$julian_hatch[data$habitat_hatch == 'urban']),
                                       mean(data$julian_hatch[data$habitat_hatch == 'forest']))


# calculate predicted values and associated SE
df_predict$nestling_survival <- {
  predict(nestling_survival_model_final, df_predict, re.form = NA, se.fit = T, type = 'response')
}$fit

# associated standard errors
df_predict$se <- { 
  predict(nestling_survival_model_final, df_predict, type = 'response', se.fit = T)
}$se.fit


df_predict <- df_predict %>% 
  group_by(EXPERIMENTAL_GROUP, habitat_rearing) %>% 
  summarise(nestling_survival = mean(nestling_survival),
            nestling_survival_low = mean(nestling_survival - se),
            nestling_survival_high = mean(nestling_survival + se))

##
## plot with model predictions
# adjust name of habitat factors for plot
data$habitat_rearing2 <- ifelse(data$habitat_rearing == 'urban', 'Urban', 'Forest')
df_predict$habitat_rearing2 <- ifelse(df_predict$habitat_rearing == 'urban', 'Urban', 'Forest')
level_order <- c('Urban', 'Forest') 

nestling_survival <- ggplot(data = data, 
                         aes(x = EXPERIMENTAL_GROUP, 
                             y = survived, 
                             fill = EXPERIMENTAL_GROUP)) +
  facet_grid(~factor(habitat_rearing2, level = level_order)) +
  geom_point(position = position_jitter(width = 0.2, height = 0),
             color= 'white',
             shape = 21,
             alpha = 0.5, 
             size=3) +
  geom_errorbar(data = df_predict, 
                width = 0,
              aes(y = nestling_survival, 
                  ymin = nestling_survival_low, 
                  ymax = nestling_survival_high,
                  color = EXPERIMENTAL_GROUP,
                  fill = EXPERIMENTAL_GROUP)) +
  geom_point(data = df_predict, 
             size = 8,
             shape = 21, 
             color = 'black',
            aes(x = EXPERIMENTAL_GROUP, 
                y = nestling_survival, 
                color = EXPERIMENTAL_GROUP,
                fill = EXPERIMENTAL_GROUP)) 
  
  
  
  
  
  theme_classic() + 
  theme(axis.title=element_text(size=20),
        axis.text=element_text(size=15)) +
  scale_x_discrete(labels= c("Day 2", "Day 6", "Day 12", "Fledged"))+
  scale_y_continuous(limits=c(0,15), breaks=c(0,3,6,9,12,15))+
  theme(legend.position = "top",
        legend.title=element_text(size=20),
        legend.text=element_text(size=20),
        strip.text = element_text(size=20)) +
  scale_color_manual(name = "Treatment group", values = c("#67a9cf", "pink")) +
  
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
