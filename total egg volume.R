##
##
## Script to calculate total egg volume produced by a female##############
##
##Libraries#####
library(lubridate)
library(dplyr)
library(ggplot2)
library(wesanderson)
library(lme4)
library(nlme)
library(lmerTest)
library(performance) 
library(openxlsx)
library(tidyr)
library(rptR)
library(sjPlot)
library(sjmisc)
library(ggplot2)
##
## clean environment
rm(list = ls())

##
##
##### data #####
##
##
data<- read.xlsx("./data/CSM_18.07.2022.xlsx",
                 colNames=T,
                 sheet = 7)

#changing data types#############
data$LOCATION<-as.factor(data$LOCATION)
data$NESTBOX<-as.factor(data$NESTBOX)
data$EXPERIMENTAL_GROUP<-as.factor(data$EXPERIMENTAL_GROUP)
data$julian_1st_egg<-as.numeric(data$julian_1st_egg)
data$weight_g<-as.numeric(data$weight_g)
data$yolk_weight_g<-as.numeric(data$yolk_weight_g)
data$LATEST_LAYING_ORDER<-as.factor(data$LATEST_LAYING_ORDER)
##
## selecting only the columns that are needed for the analysis#####################
data <- data %>%
  group_by(NESTBOX, EGG_LABEL, EXPERIMENTAL_GROUP) %>% 
  mutate(eggID=cur_group_id())%>%
  dplyr::select(eggID, NESTBOX,EXPERIMENTAL_GROUP, EGG_LABEL, julian_early, julian_latest, LOCATION, LATEST_LAYING_ORDER, 
                length, width, volume, photo_id, no_eggs_photo, weight_g, yolk_weight_g, rep, accurate)

allbroods<-read.xlsx("./data/CSM_18.07.2022.xlsx",
                     colNames=T,
                     sheet = 1)

################performing a left join to combine the two datasets into one
data<-left_join(x=data, y=allbroods%>%
                   select(IN_NEST_CLUTCH_SIZE, NESTBOX, julian_1st_egg, NUMBER_EGGS_LAID), by='NESTBOX')


#MEAN VOLUMES###########################
data2<-data%>%
  group_by(NESTBOX, EGG_LABEL, EXPERIMENTAL_GROUP)%>%
  summarise_at(vars(volume), 
               list(name=mean))%>%
  mutate(mean_volume=name)
meandata<-left_join(data2, data, by=c("NESTBOX", "EGG_LABEL", "EXPERIMENTAL_GROUP"))
meandata$mean_volume<-as.numeric(meandata$mean_volume)

#sorting out data so there is only one mean value per egg#######################
meandata<-meandata%>%
  filter(!is.na(mean_volume))%>%
  group_by(mean_volume, EGG_LABEL, EXPERIMENTAL_GROUP)%>%
  distinct(NESTBOX, EGG_LABEL, mean_volume,.keep_all = TRUE)%>%
  filter(mean_volume!=0)%>%
  filter(!NESTBOX%in%c(540, 558, 560))


########including clutch size in the models#############
allbroods<-read.xlsx("./data/CSM_18.07.2022.xlsx",
                     colNames=T,
                     sheet = 1)
allbroods$NESTBOX<-as.factor(allbroods$NESTBOX)
meandata<-left_join(x=meandata, y=allbroods%>%
                      select(NESTBOX, IN_NEST_CLUTCH_SIZE, NUMBER_EGGS_LAID),
                    by="NESTBOX")
meandata2<-meandata%>%
  filter(!is.na(mean_volume))###filters out nests where the females abandoned their investment before clutch completion()

meandata2<-meandata2%>%
  group_by(NESTBOX)%>%
  summarise_at(vars(volume), 
               list(name=mean))%>%
  mutate(mean_nest_egg=name)

meandata<-left_join(meandata2, data, by=c("NESTBOX"))
meandata$total_investment<-(meandata$mean_nest_egg*meandata$NUMBER_EGGS_LAID)#total investment represents the total volume of egg material produced by each female
meandata<-meandata%>%
  filter(!is.na(total_investment))%>%
  distinct(NESTBOX,.keep_all = TRUE)%>%
  filter(!NESTBOX%in%c(540, 558, 560, 19, 20, 55, 529, 539, 571))#filtered nests where we only photographed a couple eggs or females abandoned

##################STATISTICAL MODELS#########################

##############total volume of egg produced models##################
meandata$julian_1st_egg<-as.numeric(meandata$julian_1st_egg)

#Model 1: with lay date fitted as a quadratic###
m0.q<-lm(total_investment~
         LOCATION*EXPERIMENTAL_GROUP+
           poly(julian_1st_egg, 2)[,2] +
           poly(julian_1st_egg, 2)[,1],
       data=meandata)
check_model(m0.q, panel=F)

#Model 2: with lay date fitted linearly
m0<-lm(total_investment~
           LOCATION*EXPERIMENTAL_GROUP+
           scale(as.numeric(julian_1st_egg), scale = F),
         data=meandata)
summary (m0)
lrtest.default(m0.q,m0)#gives the likelihood ratio for date fitted as a quadratic term
check_model(m0, panel=F)
confint.lm(m0)

##Model 3: model with no interaction (minimal model)
m1 <- lm(total_investment~
             LOCATION+EXPERIMENTAL_GROUP+
             scale(as.numeric(julian_1st_egg), scale = F),
           data=meandata)
summary(m1)
check_model(m1, panel=F)
confint.lm(m1)
lm1<-lrtest.default(m1,m0)#gives likelihood ratio for the location*experimental group interaction

###Model 4:model with no experimental group
m2 <- lm(total_investment~
           LOCATION+
           scale(as.numeric(julian_1st_egg), scale = F),
         data=meandata)
lm2<-lrtest.default(m2,m1)#gives likelihood ratio for exp group

###Model 5: model with no location
m3 <- lm(total_investment~
           EXPERIMENTAL_GROUP+
           scale(as.numeric(julian_1st_egg), scale = F),
         data=meandata)
lm3<-lrtest.default(m3,m1)#gives likelihood ratio for location

###Model 6: model with no lay date
m4 <- lm(total_investment~
           LOCATION+EXPERIMENTAL_GROUP,
         data=meandata, REML=F)
lm4<-lrtest.default(m4,m1)#gives likelihood ratio for lay date


#######Plotting the model predictions (from Model 2)#########
newdat<-expand.grid(
  EXPERIMENTAL_GROUP=unique(meandata$EXPERIMENTAL_GROUP),
  LOCATION=unique(meandata$LOCATION),
  julian_1st_egg=mean(as.numeric(meandata$julian_1st_egg), na.rm = T),
  total_investment=0
)

##the new predicted values and associated standard errors for the raw data
newdat$total_investment<-{predict(m1, newdat, se.fit = T)}$fit###predicted value from your linear model
newdat$se <- {predict(m1, newdat, se.fit = T)}$se.fit####the associated standard errors

#####plotting the predictions with the standard errors onto a ggplot##########
PREDICTED_PLOT<- ggplot(data = meandata, aes(x = LOCATION, #this is so you can add your raw data points
                                                y = total_investment, 
                                                fill = EXPERIMENTAL_GROUP)) +
  geom_point(position = position_jitterdodge(dodge.width = 0.60,
                                             jitter.width = 0.18),
             size = 3.5,
             shape = 21,
             color = "white",
             alpha = 0.5) +
  geom_linerange(data = newdat, ###this is where you include your dummy dataset prediction points and se
                 aes(y = total_investment, 
                     ymin = total_investment - se, 
                     ymax = total_investment + se),
                 position = position_dodge(width = 0.60),
                 width = 0) +
  geom_point(data = newdat, 
             aes(x = LOCATION, y = total_investment, 
                 fill = EXPERIMENTAL_GROUP),
             position = position_dodge(width = 0.60),
             color = "black", 
             size = 5,
             shape = 21) +
  scale_x_discrete(labels = c("City", "Forest")) +
  theme_classic() +  theme(axis.title=element_text(size=15),
                           axis.text=element_text(size=12)) +
  theme(legend.position = "top",
        legend.title=element_text(size=12),
        legend.text=element_text(size=12)) +
  scale_fill_manual(name = "Treatment Group", values = c("#67a9cf", "pink")) +
  scale_color_manual(name = "Treatment Group", values = c("#67a9cf", "pink")) +
  labs(x = "Habitat", y = (bquote("Total volume of egg produced"(mm^3))))

# to save plot
ggsave(filename = "./plots/total investment.png",
       plot = PREDICTED_PLOT, 
       device = "png", 
       width = 140, 
       height = 150, 
       units = "mm",  )


####Total Investment for each location (running seperate models for the Urban and Forest site)#########

#Model 7: Total investment in the city --> with lay date fitted as a quadratic term
mkgp <- lm(total_investment~EXPERIMENTAL_GROUP+
             poly(julian_1st_egg, 2)[,2] +
             poly(julian_1st_egg, 2)[,1],
           data=meandata%>%
             filter(LOCATION=="Kelvingrove_park"))
check_model(mkgp, panel=F)

#Model 8: Total investment in the city --> with lay date fitted as a linear term
mkgp1 <- lm(total_investment~EXPERIMENTAL_GROUP+
           scale(as.numeric(julian_1st_egg), scale = F),
         data=meandata%>%
           filter(LOCATION=="Kelvingrove_park"))
lrtest.default(mkgp,mkgp1)#gives the likelihood ratio test for lay date (fitted as a quadratic term)
summary(mkgp1)
confint.lm(mkgp1)

#Model 9: Total investment in the city --> without exp group
mkgp.1 <- lm(total_investment~
             scale(as.numeric(julian_1st_egg), scale = F),
           data=meandata%>%
             filter(LOCATION=="Kelvingrove_park"))
lrtest.1<-lrtest.default(mkgp.1, mkgp1)#gives the lrtest for experimental group in the city

#Model 10: Total investment in the city -->without lay date
mkgp.2 <- lm(total_investment~EXPERIMENTAL_GROUP,
             data=meandata%>%
               filter(LOCATION=="Kelvingrove_park"))
lrtest.2<-lrtest.default(mkgp.2, mkgp1)#Gives the lrtest for lay date in the city


#Model 11: Total investment in the forest --> with lay date fitted as a quadratic term
mscene1 <- lm(total_investment~EXPERIMENTAL_GROUP+
             poly(julian_1st_egg, 2)[,2] +
             poly(julian_1st_egg, 2)[,1],
           data=meandata%>%
             filter(LOCATION=="SCENE"))
check_model(mscene1, panel=F)

#Model 12: Total investment in the forest --> with lay date fitted as a linear term
mscene <- lm(total_investment~EXPERIMENTAL_GROUP+
             scale(as.numeric(julian_1st_egg), scale = F),
           data=meandata%>%
             filter(LOCATION=="SCENE"))
check_model(mscene, panel=F)
lrtest.default(mscene1,mscene)#gives the lrtest for lay date (fitted as a quadratic term)
summary(mscene)
confint.lm(mscene)

#Model 13:Total investment in the forest --> without exp group
mscene.1 <- lm(total_investment~
               scale(as.numeric(julian_1st_egg), scale = F),
             data=meandata%>%
               filter(LOCATION=="SCENE"))
lrtest.5<-lrtest.default(mscene.1, mscene)

#Model 14 --> Total investment in the forest --> without lay date
mscene.2 <- lm(total_investment~EXPERIMENTAL_GROUP,
             data=meandata%>%
               filter(LOCATION=="SCENE"))
lrtest.6<-lrtest.default(mscene.2, mscene)