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

# read clutch dataset to filter for those clutches that can be included in this analysis
data_clutches <- read.xlsx("./data/Pitt_et_all_database.xlsx",
                           colNames=T,
                           sheet = 1)

data_clutches <- data_clutches %>% 
  filter(analysis_nestling_survival == 'YES') %>% 
  select(analysis_nestling_survival, NESTBOX, EXPERIMENTAL_GROUP, NUMBER_HATCHED_EGGS) %>% 
  mutate(NESTBOX = as.factor(NESTBOX))

# nestling weights data
data <- read.xlsx("./data/Pitt_et_all_database.xlsx",
                  colNames=T,
                  sheet = 4)

# select eggs from clutches that should be included in this analysis
data <- data %>% 
  filter(AGE == 2) %>% 
  filter(NESTBOX %in% data_clutches$NESTBOX) %>% 
  left_join(., 
            y = data_clutches %>% select(NESTBOX, EXPERIMENTAL_GROUP, NUMBER_HATCHED_EGGS), 
            by = 'NESTBOX') %>% 
  mutate(julian_hatch = as.numeric(julian_date) - 2)

# calculate the number of siblings on day 2
data_sibs <- data %>% 
  group_by(NESTBOX) %>% 
  summarise(n_sibs = n())

# merge to data set
data <- left_join(x = data, 
                  y = data_sibs, 
                  by = 'NESTBOX')

table(data$n_sibs)

head(data)

##
##
##### analysis of mass day 2 #####
##
##
table(is.na(data$WEIGHT))
length(unique(data$NESTBOX))
sort(table(data$RING_NUMBER))
length(unique(data$RING_NUMBER))



# Model 1: with hatch date fitted as a quadratic
mass_d2_model_full <-lmer(WEIGHT~
                            HABITAT:EXPERIMENTAL_GROUP +
                            EXPERIMENTAL_GROUP +
                            HABITAT +
                            time_category +
                            scale(n_sibs, scale=F) +
                            poly(julian_hatch, 2)[,2] +
                            poly(julian_hatch, 2)[,1]+
                            (1|NESTBOX),
                          data = data)
# simulate residuals 
weight_d2_model_full_residuals <- simulateResiduals(mass_d2_model_full, n = 1000, plot = T) # not too bad

# model results
summary(mass_d2_model_full)
drop1(mass_d2_model_full, test = 'Chisq')

# Model 2: without interactions
mass_d2_model <-lmer(WEIGHT~
                       EXPERIMENTAL_GROUP +
                       HABITAT +
                       time_category +
                       scale(n_sibs, scale=F) +
                       scale(julian_hatch, scale=F) +
                     (1|NESTBOX),
                     data = data)
# model results
summary(mass_d2_model)
drop1(mass_d2_model, test = 'Chisq')

##
##
##### Table of results #####
##
##

## base table
weight_d2_table00 <- mass_d2_model %>%
  tbl_regression(intercept = T,
                 label = list(
                   `(Intercept)` = "Intercept",
                   `EXPERIMENTAL_GROUP` = "Experimental group",
                   `HABITAT` = "Habitat",
                   `time_category` = 'Time of the day',
                   `scale(n_sibs, scale = F)` = 'Number of siblings',
                   `scale(julian_hatch, scale = F)` = "Hatching date"),
                 estimate_fun = ~ style_number(.x, digits = 3)) 

## add features
weight_d2_table <- weight_d2_table00 %>% 
  add_global_p(anova_fun = drop1_output) %>% 
  bold_p(t = 0.05) %>% 
  bold_labels() %>%
  italicize_levels() %>% 
  modify_table_body(fun = function(.){
    output <- left_join(x = .,
                        y = drop1_output(x=mass_d2_model) %>% 
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
gtsave(weight_d2_table, "./tables/TABLE S6.html")

#####

##
##
##### Plotting model results #####
##
##

# df to calculate predictions
df_predict <- expand.grid(
  EXPERIMENTAL_GROUP = unique(data$EXPERIMENTAL_GROUP),
  HABITAT = unique(data$HABITAT),
  time_category = unique(data$time_category),
  julian_hatch = mean(as.numeric(data$julian_hatch), na.rm = T)
)

df_predict$julian_hatch <- ifelse(df_predict$HABITAT == 'urban', 
                                            mean(data$julian_hatch[data$HABITAT == 'urban']),
                                            mean(data$julian_hatch[data$HABITAT == 'urban']))

df_predict$NUMBER_HATCHED_EGGS <- ifelse(df_predict$HABITAT == 'urban', 
                                      mean(data$NUMBER_HATCHED_EGGS[data$HABITAT == 'urban']),
                                      mean(data$NUMBER_HATCHED_EGGS[data$HABITAT == 'urban']))

# getting the predicted values
df_predict$WEIGHT <- predict(mass_d2_model, df_predict, re.form=NA)
mm <- model.matrix(terms(mass_d2_model), df_predict)
pvar1 <- diag(mm %*% tcrossprod(vcov(mass_d2_model),mm))
cmult <- 1.96
df_predict <-data.frame(df_predict,
                    plo = df_predict$WEIGHT-cmult*sqrt(pvar1),
                    phi = df_predict$WEIGHT+cmult*sqrt(pvar1))

# averaging across times of the day
df_predict <- df_predict %>% 
  group_by(EXPERIMENTAL_GROUP, HABITAT) %>% 
  summarise(WEIGHT = mean(WEIGHT),
            plo = mean(plo),
            phi = mean(phi)) %>% 
  mutate(EXPERIMENTAL_GROUP = as.character(EXPERIMENTAL_GROUP))

# plot
PREDICTED_PLOT<- ggplot(data = data,
                        aes(x = HABITAT, 
                            y = WEIGHT, 
                            fill = EXPERIMENTAL_GROUP)) +
  geom_point(position = position_jitterdodge(dodge.width = 0.60,
                                             jitter.width = 0.18),
             size = 3.5,
             shape = 21,
             color = "white",
             alpha = 0.5) +
  geom_errorbar(data = df_predict, 
                 aes(y = WEIGHT, 
                     ymin = plo, 
                     ymax = phi),
                 position = position_dodge(width = 0.60),
                 width = 0) +
  geom_point(data = df_predict, 
             aes(x = HABITAT, 
                 y = WEIGHT, 
                 fill = EXPERIMENTAL_GROUP),
             position = position_dodge(width = 0.60),
             color = "black", 
             size = 5,
             shape = 21) +
  scale_x_discrete(labels = c("Urban", "Forest")) +
  scale_y_continuous(limits=c(0.5,3.5), breaks=c(0.5,1,1.5,2,2.5,3,3.5))+
  theme_classic() +
  theme(axis.title=element_text(size=25),
        axis.text=element_text(size=25))+
  theme(legend.position = "top",
        legend.title=element_text(size=25),
        legend.text=element_text(size=25)) +
  scale_fill_manual(name = "Treatment Group", values = c("#67a9cf", "pink")) +
  scale_color_manual(name = "Treatment Group", values = c("#67a9cf", "pink")) +
  labs(x = "Habitat", y = "Body mass (g)")

