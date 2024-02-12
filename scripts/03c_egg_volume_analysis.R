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
##### data #####
##
##
data <- readRDS(file = './data/individual_egg_data.RDS')

head(data)
length(unique(data$eggID)) # number of eggs
length(unique(data$NESTBOX))     # number of nest-boxes

summary(data$LATEST_LAYING_ORDER - data$EARLIEST_LAYING_ORDER)
table(data$LATEST_LAYING_ORDER - data$EARLIEST_LAYING_ORDER, data$accurate)

#####

##
##
##### Initial egg volume model #####
##
##
egg_volume_model_full <- lmer(volume ~
                                LATEST_LAYING_ORDER:EXPERIMENTAL_GROUP:HABITAT +
                                LATEST_LAYING_ORDER:EXPERIMENTAL_GROUP +
                                LATEST_LAYING_ORDER:HABITAT +
                                EXPERIMENTAL_GROUP:HABITAT +
                                LATEST_LAYING_ORDER +
                                EXPERIMENTAL_GROUP +
                                HABITAT +
                                scale(NUMBER_EGGS_LAID, scale = F) +
                                poly(julian_latest_lay_date, 2)[,2] +
                                poly(julian_latest_lay_date, 2)[,1]+
                                (1|NESTBOX) +
                                (1|eggID),
                              data = data)

table(is.na(data$NUMBER_EGGS_LAID))

# simulate residuals 
egg_volume_model_full_residuals <- simulateResiduals(egg_volume_model_full, n = 1000, plot = T)
hist(residuals(egg_volume_model_full)) # residuals do not look bad at all

summary(egg_volume_model_full)
drop1(egg_volume_model_full, test = 'Chisq')

#####

##
##
##### Model without non-significant interactions #####
##
##
egg_volume_model <- lmer(volume ~
                           LATEST_LAYING_ORDER:EXPERIMENTAL_GROUP:HABITAT +
                           LATEST_LAYING_ORDER:EXPERIMENTAL_GROUP +
                           LATEST_LAYING_ORDER:HABITAT +
                           EXPERIMENTAL_GROUP:HABITAT +
                           LATEST_LAYING_ORDER +
                           EXPERIMENTAL_GROUP +
                           HABITAT +
                           scale(NUMBER_EGGS_LAID, scale = F) +
                           scale(julian_latest_lay_date, scale = F) +
                           (1|NESTBOX) +
                           (1|eggID),
                         data = data)
summary(egg_volume_model)
drop1(egg_volume_model, test = 'Chisq')

#####

##
##
##### Table of results #####
##
##

## base table
egg_volume_model00 <- egg_volume_model %>%
  tbl_regression(intercept = T,
                 label = list(
                   `(Intercept)` = "Intercept",
                   `EXPERIMENTAL_GROUP` = "Experimental group",
                   `HABITAT` = "Habitat",
                   `scale(julian_latest_lay_date, scale = F)` = "Egg laying date",
                   `LATEST_LAYING_ORDER` = 'Laying order',
                   `scale(NUMBER_EGGS_LAID, scale = F)` = 'Total number of eggs laid'),
                 estimate_fun = ~ style_number(.x, digits = 3)) 

## add features
egg_volume_table <- egg_volume_model00 %>% 
  add_global_p(anova_fun = drop1_output) %>% 
  bold_p(t = 0.05) %>% 
  bold_labels() %>%
  italicize_levels() %>% 
  modify_table_body(fun = function(.){
    output <- left_join(x = .,
                        y = drop1_output(x=egg_volume_model) %>% 
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
gtsave(egg_volume_table, "./tables/TABLE S5.html")

#####

##
##
##### Egg volume model predictions #####
##
##

# data frame to create predictions
df_predict <- expand.grid(
  LATEST_LAYING_ORDER=c(1:9),
  HABITAT=unique(data$HABITAT),
  EXPERIMENTAL_GROUP=unique(data$EXPERIMENTAL_GROUP)
)

df_predict$julian_latest_lay_date <- ifelse(df_predict$HABITAT == 'urban', 
                                            mean(data$julian_latest_lay_date[data$HABITAT == 'urban']),
                                            mean(data$julian_latest_lay_date[data$HABITAT == 'urban']))

df_predict$NUMBER_EGGS_LAID <- ifelse(df_predict$HABITAT == 'urban', 
                                      mean(data$NUMBER_EGGS_LAID[data$HABITAT == 'urban']),
                                      mean(data$NUMBER_EGGS_LAID[data$HABITAT == 'urban']))

# predictions
df_predict$volume<-predict(egg_volume_model, df_predict, re.form=NA)

# calculation 95%CI for predictions
mm<-model.matrix(terms(egg_volume_model),df_predict)
pvar1 <- diag(mm %*% tcrossprod(vcov(egg_volume_model),mm))
cmult <- 1.96
df_predict <-data.frame(df_predict,
                        plo = df_predict$volume-cmult*sqrt(pvar1),
                        phi = df_predict$volume+cmult*sqrt(pvar1))

# getting the means to put on the plot
df_summary_volume <- data %>%
  group_by(HABITAT, LATEST_LAYING_ORDER, EXPERIMENTAL_GROUP) %>% 
  summarise(mean_volume = mean(volume, na.rm = T),
            se_volume= sd(volume)/sqrt(n()))

# adjust name of habitat factors for plot
data$HABITAT <- ifelse(data$HABITAT == 'urban', 'Urban', 'Forest')
df_summary_volume$HABITAT <- ifelse(df_summary_volume$HABITAT == 'urban', 'Urban', 'Forest')
df_predict$HABITAT <- ifelse(df_predict$HABITAT == 'urban', 'Urban', 'Forest')

# plotting the predicted values onto a ggplot
egg_volume_plot <- ggplot(data = data,
             aes(x = LATEST_LAYING_ORDER, 
                 y = volume, 
                 fill = EXPERIMENTAL_GROUP,
                 colour = EXPERIMENTAL_GROUP)) +
  facet_wrap(~factor(HABITAT, levels = c('Urban', 'Forest'))) +
  geom_point(position = position_jitter(width = 0.15),
             alpha = 0.25) +
  geom_errorbar(data = df_summary_volume, 
                aes(y = mean_volume, 
                    ymin = mean_volume-se_volume, 
                    ymax = mean_volume+se_volume),
                position = position_dodge(width = 0.60),
                width = 0,
                size = 1.5,
                alpha=1) +
  geom_point(data = df_summary_volume, 
             aes(x = LATEST_LAYING_ORDER, 
                 y = mean_volume, 
                 fill = EXPERIMENTAL_GROUP),
             position = position_dodge(width = 0.60),
             color = "black", 
             size = 7,
             shape = 21,
             alpha=1) +
  geom_line(data = df_predict, 
            aes(x = LATEST_LAYING_ORDER, 
                y = volume,
                color = EXPERIMENTAL_GROUP),
            size=1.5) +
  geom_ribbon(data = df_predict, 
              aes(y=NULL, 
                  ymin = plo, 
                  ymax = phi, 
                  color=NULL, 
                  fill = EXPERIMENTAL_GROUP),
              alpha = .25) +
  theme_classic() + 
  theme(axis.title=element_text(size=25),
        axis.text=element_text(size=25))+
  theme(legend.position = "top",
        legend.title=element_text(size=20),
        legend.text=element_text(size=20),
        strip.text = element_text(size=25)) +
  scale_x_continuous(breaks=c(1,2,3,4,5,6,7,8,9))+
  scale_y_continuous(limits=c(800,1900), breaks=c(900, 1100,1300,1500,1700,1900))+
  scale_fill_manual(name = "Treatment group", values = c("#67a9cf", "pink")) +
  scale_color_manual(name = "Treatment group", values = c("#67a9cf", "pink")) +
  labs(x = "Position in laying sequence", y = bquote("Egg volume" (mm^3)))

ggsave(filename = "./plots/Figure3.png",
       plot = egg_volume_plot, 
       device = "png", 
       width = 210, 
       height = 210, 
       units = "mm")

