

# Hello Mark

####Script Aim: To analyse the number of eggs laid in both treatment groups (experimental or control) in the urban and forest habitat#########

#########installing the necessary packages#####
install.packages("lubridate")
install.packages("nlme")
install.packages("lme4")
install.packages("lmerTest")
install.packages("performance")
install.packages("openxlsx")
install.packages("lubridate")
install.packages("lmtest")
install.packages ("gridExtra")


## libraries###########
library(drop1)
library(dplyr)
library(ggplot2)
library(wesanderson)
library(lme4)
library(nlme)
library(lmerTest)
library(performance) 
library(openxlsx)
library(lmtest)
library(gridExtra)

##
## clean environment
rm(list=ls())

##
##
##### Read data#####
##
##
CSM_ <- read.xlsx("./data/CSM_18.07.2022.xlsx",
                  colNames=T,
                  sheet = 1)


## data formatting#########
CSM_$LOCATION<-as.factor(CSM_$LOCATION)
CSM_$NESTBOX<-as.factor(CSM_$NESTBOX)
CSM_$EXPERIMENTAL_GROUP<-as.factor(CSM_$EXPERIMENTAL_GROUP)
CSM_$julian_1st_egg<-as.numeric(CSM_$julian_1st_egg)
CSM_$julian_hatch_date<-as.numeric(CSM_$julian_hatch_date)
CSM_$NUMBER_EGGS_LAID<-as.numeric(CSM_$NUMBER_EGGS_LAID)


#################filtering the dataset so it only includes Kelvingrove (the urban site) and SCENE (the forest site)#################
  clutch.size<-CSM_%>%
    filter(LOCATION %in% c("SCENE", "KELVINGROVE"))%>%
    filter(!is.na(NUMBER_EGGS_LAID))%>%
  filter(!NESTBOX%in%c(540, 558, 560))#removing nests where females abandoned before clutch completion
clutch.size<-as.data.frame(clutch.size)

##### Statistical Models######
#########Model 1: With lay date fitted as a quadratic term########
m0 <- lm(NUMBER_EGGS_LAID ~ 
           EXPERIMENTAL_GROUP : LOCATION + 
           EXPERIMENTAL_GROUP +
           LOCATION + 
           poly(julian_1st_egg, 2)[,2] +
           poly(julian_1st_egg, 2)[,1],
         data = clutch.size)
summary(m0)
check_model(m0, panel=F)

#######Model 2: With lay date fitted as a linear term#######
m1<- lm(NUMBER_EGGS_LAID ~ 
          EXPERIMENTAL_GROUP*LOCATION+
        scale(julian_1st_egg, scale = F),
        data = clutch.size)
summary(m1)
check_model(m1, panel=F)
confint.lm(m1)
lrtest.default(m1,m0)#Gives the lrtest value for lay date fitted as a quadratic term

##Getting the likelihood ratio test results for all of the fixed effects in this model#########
###Model 3: model with no interaction included
m2<- lm(NUMBER_EGGS_LAID ~ 
          EXPERIMENTAL_GROUP+LOCATION+
          scale(julian_1st_egg, scale = F),
        data = clutch.size)
lm1<-lrtest(m1, m2)#gives the likelihood ratio test for the location*treatment group interaction 

###Model 4: model with no experimental group included
m3<- lm(NUMBER_EGGS_LAID ~ 
          LOCATION+
          scale(julian_1st_egg, scale = F),
        data = clutch.size)
lm2<-lrtest(m3, m2)#gives likelihood ratio test for experimental group

###Model 5: model with no location effect
m4<- lm(NUMBER_EGGS_LAID ~ 
          EXPERIMENTAL_GROUP+
          scale(julian_1st_egg, scale = F),
        data = clutch.size)
lm3<-lrtest(m4, m2)#gives likelihood ratio for Location

###Model 6: model with no lay date
m5<- lm(NUMBER_EGGS_LAID ~ 
          EXPERIMENTAL_GROUP*LOCATION,
        data = clutch.size)
lm4<-lrtest(m5, m2)#gives likelihood ratio for Lay date

#######Plotting the model predictions from Model 2#########
newdat<-expand.grid(
  EXPERIMENTAL_GROUP=unique(clutch.size$EXPERIMENTAL_GROUP),
  LOCATION=unique(clutch.size$LOCATION),
  julian_1st_egg=mean(as.numeric(clutch.size$julian_1st_egg), na.rm = T),
  NUMBER_EGGS_LAID=0
)

##the new predicted values and associated 95% confidence intervals
newdat$NUMBER_EGGS_LAID<-{predict(m1, newdat, se.fit = T)}$fit###predicted value from your linear model
newdat$se <- {predict(m1, newdat, se.fit = T)}$se.fit####the associated standard errors
newdatci<-{predict(m1, newdat, interval="confidence")}#extracting 95% confidence intervals
newdatci<-as.data.frame(newdatci)
newdatci$NUMBER_EGGS_LAID<-newdatci$fit
newdat<-left_join(x=newdat, y=newdatci%>%
                      select(lwr, upr, NUMBER_EGGS_LAID),
                    by="NUMBER_EGGS_LAID")

#####plotting the predictions with the confidence intervals onto a ggplot##########
PREDICTED_PLOT<- ggplot(data = clutch.size, aes(x = LOCATION, 
                                                 y = NUMBER_EGGS_LAID, 
                                                 fill = EXPERIMENTAL_GROUP)) +
  geom_point(position = position_jitterdodge(dodge.width = 0.70,
                                             jitter.width = 0.13),
             size = 3.5,
             shape = 21,
             color = "white",
             alpha = 0.5) +
  geom_linerange(data = newdat, 
                 aes(y = NUMBER_EGGS_LAID, 
                     ymin =lwr, 
                     ymax = upr),
                 position = position_dodge(width = 0.60),
                 width = 0) +
  geom_point(data = newdat, 
             aes(x = LOCATION, y = NUMBER_EGGS_LAID, 
                 fill = EXPERIMENTAL_GROUP),
             position = position_dodge(width = 0.60),
             color = "black", 
             size = 5,
             shape = 21) +
  scale_x_discrete(labels = c("Urban", "Forest")) +
  scale_y_continuous(breaks=c(4,6,8,10,12,14), limits=c(4,14))+
  theme_classic() +  
  theme(axis.title=element_text(size=20),
                           axis.text=element_text(size=20)) +
  theme(legend.position = "top",
        legend.title=element_text(size=13),
        legend.text=element_text(size=13)) +
  scale_fill_manual(name = "Experimental Group", values = c("#67a9cf", "pink")) +
  scale_color_manual(name = "Experimental Group", values = c("#67a9cf", "pink")) +
  labs(x = "Habitat", y = "Number of eggs laid")

# to save plot
ggsave(filename = "./plots/predicted_number_eggs_control+experimental.png",
       plot = PREDICTED_PLOT, 
       device = "png", 
       width = 180, 
       height = 180, 
       units = "mm",  )

###Part 2: Within habitat modelling --> running separate models for the Urban and Forest site######
##########Model 7: Number of eggs laid in the Forest --> with lay date fitted as a quadratic###########
m1a.1 <- lm(NUMBER_EGGS_LAID ~ 
            EXPERIMENTAL_GROUP +poly(julian_1st_egg, 2)[,2] +
            poly(julian_1st_egg, 2)[,1],
          data = clutch.size%>% filter(LOCATION == "SCENE"))
check_model(m1a.1, panel =F)

##########Model 8: Number of eggs laid in the Forest --> with lay date fitted as a linear term###########
m1a <- lm(NUMBER_EGGS_LAID ~ 
            EXPERIMENTAL_GROUP +
            scale(julian_1st_egg, scale = F),
          data = clutch.size%>% filter(LOCATION == "SCENE"))
summary(m1a)
check_model(m1a, panel=F)
lrtest.default(m1a.1,m1a)#tests egg laying date when it is fitted as a quadratic
confint(m1a)

##Models for Likelihood ratio tests#########
##Model 9: model with no lay date
m2a <- lm(NUMBER_EGGS_LAID ~ 
            EXPERIMENTAL_GROUP,
          data = clutch.size%>% filter(LOCATION == "SCENE"))
lm2a<-lrtest(m2a,m1a)#gives likelihood ratio for lay date

###Model 10: model with no experimental group
m3a <- lm(NUMBER_EGGS_LAID ~ 
            scale(julian_1st_egg, scale = F),
          data = clutch.size%>% filter(LOCATION == "SCENE"))
lm3a<-lrtest(m3a,m1a)#gives likelihood ratio for the treatment group

############Model 11: Number of eggs laid in the City --> with lay date fitted as a quadratic###########
m1b.1 <- lm(NUMBER_EGGS_LAID ~ 
              EXPERIMENTAL_GROUP +poly(julian_1st_egg, 2)[,2] +
              poly(julian_1st_egg, 2)[,1],
            data = clutch.size%>% filter(LOCATION == "KELVINGROVE"))
check_model(m1b.1, panel =F)

###Model 12: Number of eggs laid in the City --> with lay date fitted as a linear term###########
m1b <- lm(NUMBER_EGGS_LAID ~ 
            EXPERIMENTAL_GROUP +
            scale(julian_1st_egg, scale = F),
          data = clutch.size%>% filter(LOCATION == "KELVINGROVE"))
lrtest.default(m1b.1,m1b)
summary(m1b)
check_model(m1b, panel =F)

##getting the likelihood stats for the city model#########

###Model 13: model with no lay date
m2b <- lm(NUMBER_EGGS_LAID ~ 
            EXPERIMENTAL_GROUP,
          data = clutch.size%>% filter(LOCATION == "KELVINGROVE"))
lm2b<-lrtest(m2b,m1b)#gives likelihood ratio for lay date

###Model 14: model with no experimental group
m3b <- lm(NUMBER_EGGS_LAID ~ 
            scale(julian_1st_egg, scale = F),
          data = clutch.size%>% filter(LOCATION == "KELVINGROVE"))
lm3b<-lrtest(m3b,m1b)#gives likelihood ratio for treatment


###############Part 3: Differences in lay date and hatch date between treatment groups############
##Model 15: Differences in first egg date between treatment groups and Locations
m6 <- lm(julian_1st_egg ~ 
             EXPERIMENTAL_GROUP +
             LOCATION, 
           data = clutch.size)
summary(m6)
check_model(m6, panel=F)

###Getting the likelihood ratio tests###
#Model 16: Model with no location
m7<-lm(julian_1st_egg ~ 
            EXPERIMENTAL_GROUP,
          data = clutch.size)
lrtest.default(m7,m6)#Gives the lrtest for location

#Model 17: Model with no experimental group
m8<-lm(julian_1st_egg ~ 
           LOCATION,
         data = clutch.size)
lrtest.default(m8,m6)#gives lrtest values for any differences between treatment groups in lay date


############Differences in hatch date between treatment groups and locations################
#Model 18:Differences in hatch date between treatment groups and Locations 
m9 <- lm(julian_hatch_date ~ 
             EXPERIMENTAL_GROUP +
             LOCATION, 
           data = clutch.size)
summary(m9)
check_model(m9, panel =F)

##Getting the likelihood ratio test results
#Model 19: Model with no Location
m10<-lm(julian_hatch_date ~ 
            EXPERIMENTAL_GROUP,
          data = clutch.size)
lrtest.default(m10,m9)

#Model 20: Model with no treatment group
m11<-lm(julian_hatch_date ~ 
           LOCATION,
         data = clutch.size)
lrtest.default(m11,m9)

