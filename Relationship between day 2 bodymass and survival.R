##
##
##### SCRIPT AIM #####
##
##
#' Script to determine if a nestling body mass 2 days after hatching predicts survival until fledging
#These models are presented in the supplemntary materials, not in the main manuscript
##
##### libraries #####
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
library(dcast)
library(VGAM)
rm(list=ls())


#IMPORTING DATA#############
#########average body mass in the city/forest and experimental and control broods#########
allnestlings<- read.xlsx("./data/CSM_18.07.2022.xlsx",
                         colNames=T,
                         sheet = 4)###this sheet has information on the fate of each nestling

allnestlingweights<-read.xlsx("./data/CSM_18.07.2022.xlsx",
                              colNames=T,
                              sheet = 5)###this sheet has information on the bodyweights of each nestling
allbroods<-read.xlsx("./data/CSM_18.07.2022.xlsx",
                     colNames=T,
                     sheet = 1)#this sheet has information on the in nest clutch size, first egg date etc.

####refining the datasets and joining them together#################
colnames(allbroods)[colnames(allbroods)%in%("NESTBOX")] <- ("nestbox_hatch")#making sure columns have the same name before the left join#
allbroods$nestbox_hatch<-as.factor(allbroods$nestbox_hatch)

#######performing left join using dplyr#######
data1<-left_join(x=allnestlings, y=allnestlingweights%>%
                   select(LOCATION, RING_NUMBER, exp_period, DATE, TIME, time_category, AGE, WEIGHT, TARSUS, WING, BLOOD_ID, SAMPLE_ID), by='RING_NUMBER')
data1$nestbox_hatch<-as.factor(data1$nestbox_hatch)
data0<-left_join(x=data1, y=allbroods%>%
                   select(nestbox_hatch, EXPERIMENTAL_GROUP, NUMBER_EGGS_LAID, NUMBER_HATCHED_EGGS, IN_NEST_CLUTCH_SIZE)%>%
                   rename(EXPERIMENTAL_GROUP_HATCH=EXPERIMENTAL_GROUP)%>%
                   rename(NUMBER_EGGS_LAID_HATCH=NUMBER_EGGS_LAID)%>%
                   rename(NUMBER_SIBLINGS_HATCH=NUMBER_HATCHED_EGGS)%>%
                   rename(IN_NEST_CLUTCH_SIZE_HATCH=IN_NEST_CLUTCH_SIZE),
                 by="nestbox_hatch")

#renaming columns in all broods again so i can include number of hatched eggs, experimental group and in nest clutch size of the nestbox of rearing
colnames(allbroods)[colnames(allbroods)%in%("nestbox_hatch")] <- ("nestbox_rearing")#need to run this code AFTER you have joined data by nestbox hatching
allbroods$nestbox_rearing<-as.factor(allbroods$nestbox_rearing)
data0$nestbox_rearing<-as.factor(data0$nestbox_rearing)
data<-left_join(x=data0, y=allbroods%>%
                  select(nestbox_rearing, EXPERIMENTAL_GROUP, NUMBER_EGGS_LAID, NUMBER_HATCHED_EGGS, IN_NEST_CLUTCH_SIZE)%>%
                  rename(EXPERIMENTAL_GROUP_REARING=EXPERIMENTAL_GROUP)%>%
                  rename(NUMBER_EGGS_LAID_REARING=NUMBER_EGGS_LAID)%>%
                  rename(NUMBER_SIBLINGS_REARING=NUMBER_HATCHED_EGGS)%>%
                  rename(IN_NEST_CLUTCH_SIZE_REARING=IN_NEST_CLUTCH_SIZE),
                by="nestbox_rearing")
unique(data$fate)

#organising columNS so the data is of the correct type
data$LOCATION<-as.factor(data$site_hatch)
data$NESTBOX<-as.factor(data$site_rearing)
data$EXPERIMENTAL_GROUP_HATCH<-as.factor(data$EXPERIMENTAL_GROUP_HATCH)
data$EXPERIMENTAL_GROUP_REARING<-as.factor(data$EXPERIMENTAL_GROUP_REARING)
data$nestbox_rearing<-as.factor(data$nestbox_rearing)
data$nestbox_hatch<-as.factor(data$nestbox_hatch)
data$CROSS_FOSTER_GROUP<-as.factor(data$CROSS_FOSTER_GROUP)
data$julian_hatch<-as.numeric(data$julian_hatch)
data$julian_fate<-as.numeric(data$julian_fate)
data$AGE<-as.factor(data$AGE)
data$WEIGHT<-as.numeric(data$WEIGHT)  
data$TARSUS<-as.numeric(data$TARSUS) 
data$TARSUS<-as.numeric(data$TARSUS) 
data$julian_hatch<-as.numeric(data$julian_hatch)
data$julian_fate<-as.numeric(data$julian_fate)
data$TIME<-as.numeric(data$TIME)
data$fate<-as.factor(data$fate)
data$fate<-as.numeric(data$fate)

#renaming kelvingrove and GCU to "city" and scene to "Forest
(data["site_hatch"][data["site_hatch"]=="GCU"]<-"Urban")
(data["site_hatch"][data["site_hatch"]=="Kelvingrove_park"]<-"Urban")
(data["site_hatch"][data["site_hatch"]=="SCENE"]<-"Forest")
(data["site_rearing"][data["site_rearing"]=="GCU"]<-"Urban")
(data["site_rearing"][data["site_rearing"]=="Kelvingrove_park"]<-"Urban")
(data["site_rearing"][data["site_rearing"]=="SCENE"]<-"Forest")
data$site_rearing<-factor(data$site_rearing, levels=c("Urban", "Forest"))
data$site_hatch<-factor(data$site_hatch, levels=c("Urban", "Forest"))



######################STATISTICAL MODELS#####################
##Coding survival as the response variable (needed for the binomial models)
death_data1<-data%>%
  mutate(fate=case_when(fate==2~1,
                        fate==1~0))
           
##Filtering to only include body mass at day 2
death_data1<-death_data1%>%
  filter(!is.na(fate))
death_data1$resid<-as.factor(1:length(death_data1$RING_NUMBER))
death_data1$time_category <- factor(death_data1$time_category, levels=c("morning", "midday", "afternoon"))
death_data<-death_data1%>%
    filter(!is.na(fate))%>%
    filter(AGE==2)
death_data$fate<-as.factor(death_data$fate)


######################STATISTICAL MODELS#####################
death_data$fate<-as.numeric(death_data$fate)
death_data$WEIGHT<-as.numeric(death_data$WEIGHT)
death_data<-death_data%>%
  mutate(fate=case_when(fate==2~1,
                        fate==1~0))

###############statistical model 1: does day 2 body mass predict nestling survival?###############

##Model 1: Where hatch date is fitted as a quadratic
ma.1<-glmer(fate~
              WEIGHT*site_rearing*EXPERIMENTAL_GROUP_REARING+
              scale(NUMBER_SIBLINGS_REARING, scale=F)+
              time_category+
              poly(julian_hatch, 2)[,2] +
              poly(julian_hatch, 2)[,1]+
              (1|nestbox_rearing)+
              (1|nestbox_hatch),
            data = death_data, family=binomial(link="logit"))
####tryig a different optimiser because of issues with model fit
ss <- getME(ma.1,c("theta","fixef"))
ma.1<- update(ma.1,start=ss,control=glmerControl(optimizer="bobyqa",
                                               optCtrl=list(maxfun=2e5)))
summary(ma.1)

##Model 2: Where hatch date is fitted linearly#####
ma.2<-glmer(fate~
            WEIGHT*site_rearing*EXPERIMENTAL_GROUP_REARING+
            scale(NUMBER_SIBLINGS_REARING, scale=F)+
            time_category+
            scale(julian_hatch, scale = F)+
            (1|nestbox_rearing)+
            (1|nestbox_hatch),
          data = death_data, family=binomial(link="logit"))

####trying a different optimiser because of issues with model fit
ss <- getME(ma.2,c("theta","fixef"))
ma<- update(ma.2,start=ss,control=glmerControl(optimizer="bobyqa",
                                                 optCtrl=list(maxfun=2e5)))
summary(ma)
lrtest.default(ma,ma.1)###This gives the likelihood ratio values for the quadratic hatch date term

##confidence intervals####
confint(ma,parm="beta_",method="Wald",nsim=500)#Gives the confidence intervals for the model with two way interactions
check_model(ma, panel=F)


#####Likelihood ratio tests for model predictors######

#Model 3: Model with three way interaction effect removed
m1<-glmer(fate~
              WEIGHT*site_rearing+EXPERIMENTAL_GROUP_REARING*WEIGHT+site_rearing*EXPERIMENTAL_GROUP_REARING+
              scale(NUMBER_SIBLINGS_REARING, scale=F)+
              time_category+
              scale(julian_hatch, scale = F)+
              (1|nestbox_rearing)+
              (1|nestbox_hatch),
            data = death_data, family=binomial(link="logit"))

####trying a different optimiser because of issues with model fit
ss <- getME(m1,c("theta","fixef"))
m1<- update(m1,start=ss,control=glmerControl(optimizer="bobyqa",
                                               optCtrl=list(maxfun=2e5)))
lrtest.default(m1,ma)#tests three way interaction effect for the global model

###Model 4: Model with the Number of siblings removed###
m2<-glmer(fate~
            WEIGHT*site_rearing*EXPERIMENTAL_GROUP_REARING+
            time_category+
            scale(julian_hatch, scale = F)+
            (1|nestbox_rearing)+
            (1|nestbox_hatch),
          data = death_data%>%
            filter(!is.na(NUMBER_SIBLINGS_REARING)), family=binomial(link="logit"))

####trying a different optimiser because of issues with model fit
ss <- getME(m2,c("theta","fixef"))
m2<- update(m2,start=ss,control=glmerControl(optimizer="bobyqa",
                                             optCtrl=list(maxfun=2e5)))
lrtest.default(m2,ma)#tests number sibs

#Model 5: Model with the time of day removed
m3<-glmer(fate~
              WEIGHT*site_rearing*EXPERIMENTAL_GROUP_REARING+
              scale(NUMBER_SIBLINGS_REARING, scale=F)+
              scale(julian_hatch, scale = F)+
              (1|nestbox_rearing)+
              (1|nestbox_hatch),
            data = death_data, family=binomial(link="logit"))

####trying a different optimiser because of issues with model fit
ss <- getME(m3,c("theta","fixef"))
m3<- update(m3,start=ss,control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))
lrtest.default(m3,ma)

#Model 6: Model with hatch date removed####
m4<-glmer(fate~
              WEIGHT*site_rearing*EXPERIMENTAL_GROUP_REARING+
              scale(NUMBER_SIBLINGS_REARING, scale=F)+
              time_category+
              (1|nestbox_rearing)+
              (1|nestbox_hatch),
            data = death_data, family=binomial(link="logit"))

####trying a different optimiser because of issues with model fit
ss <- getME(m4,c("theta","fixef"))
m4<- update(m4,start=ss,control=glmerControl(optimizer="bobyqa",
                                               optCtrl=list(maxfun=2e5)))
lrtest.default(m4,ma)#tests hatch date

##############simplifying the model even further, removing higher order interactions######

#Model 7: Model without the habitat*treatment group interaction
m5<-glmer(fate~
            WEIGHT*site_rearing+EXPERIMENTAL_GROUP_REARING*WEIGHT+
            scale(NUMBER_SIBLINGS_REARING, scale=F)+
            time_category+
            scale(julian_hatch, scale = F)+
            (1|nestbox_rearing)+
            (1|nestbox_hatch),
          data = death_data, family=binomial(link="logit"))

####trying a different optimiser because of issues with model fit
ss <- getME(m5,c("theta","fixef"))
m5<- update(m5,start=ss,control=glmerControl(optimizer="bobyqa",
                                             optCtrl=list(maxfun=2e5)))

#lrtests for two way interactions
lrtest.default(m5,m1)#LRtest for habitat*treatment group-->remove this one first

###Model 8: Model without weight*treatment group interaction###
m1.2<-glmer(fate~
              site_rearing*WEIGHT+EXPERIMENTAL_GROUP_REARING+
              scale(NUMBER_SIBLINGS_REARING, scale=F)+
              time_category+
              scale(julian_hatch, scale = F)+
              (1|nestbox_rearing)+
              (1|nestbox_hatch),
            data = death_data, family=binomial(link="logit"))
ss <- getME(m1.2,c("theta","fixef"))
m1.2<- update(m1.2,start=ss,control=glmerControl(optimizer="bobyqa",
                                                 optCtrl=list(maxfun=2e5)))
                                                                                                       
##lrtest for treatment group*weight interaction
lrtest.default(m1.2,m5)

###Model 9: The Minimal Model with NO interaction effects####
m1.5<-glmer(fate~EXPERIMENTAL_GROUP_REARING+WEIGHT+site_rearing+
              scale(NUMBER_SIBLINGS_REARING, scale=F)+
              time_category+
              scale(julian_hatch, scale = F)+
              (1|nestbox_rearing)+
              (1|nestbox_hatch),
            data = death_data, family=binomial(link="logit"))
ss <- getME(m1.5,c("theta","fixef"))
m1.5<- update(m1.5,start=ss,control=glmerControl(optimizer="bobyqa",
                                                 optCtrl=list(maxfun=2e5)))                                                       
summary(m1.5)

###lrtest for habitat*weight interaction
lrtest.default(m1.4,m1.5)

##getting the confidence intervals
confint(m1.5,parm="beta_",method="Wald",nsim=500)#Getting confidence intervals for the minimal model

##Lrtests for the minimal model##############

#Model 10: removing experimental group from the minimal model
m1.5.5<-glmer(fate~WEIGHT+site_rearing+
              scale(NUMBER_SIBLINGS_REARING, scale=F)+
              time_category+
              scale(julian_hatch, scale = F)+
              (1|nestbox_rearing)+
              (1|nestbox_hatch),
            data = death_data, family=binomial(link="logit"))
ss <- getME(m1.5.5,c("theta","fixef"))
m1.5.5<- update(m1.5.5,start=ss,control=glmerControl(optimizer="bobyqa",
                                                 optCtrl=list(maxfun=2e5))) 
lrtest.default(m1.5.5,m1.5)#lrtest for experimental group

#Model 11: removing location from the minimal model
m1.6<-glmer(fate~EXPERIMENTAL_GROUP_REARING+WEIGHT+
              scale(NUMBER_SIBLINGS_REARING, scale=F)+
              time_category+
              scale(julian_hatch, scale = F)+
              (1|nestbox_rearing)+
              (1|nestbox_hatch),
            data = death_data, family=binomial(link="logit"))
ss <- getME(m1.6,c("theta","fixef"))
m1.6<- update(m1.6,start=ss,control=glmerControl(optimizer="bobyqa",
                                                 optCtrl=list(maxfun=2e5)))
lrtest.default(m1.6,m1.5)#tests the effect of habitat on survival

#Model 12: removing mass two days after hatching from the minimal model
m1.7<-glmer(fate~EXPERIMENTAL_GROUP_REARING+site_rearing+
              scale(NUMBER_SIBLINGS_REARING, scale=F)+
              time_category+
              scale(julian_hatch, scale = F)+
              (1|nestbox_rearing)+
              (1|nestbox_hatch),
            data = death_data, family=binomial(link="logit"))
ss <- getME(m1.7,c("theta","fixef"))
m1.7<- update(m1.7,start=ss,control=glmerControl(optimizer="bobyqa",
                                                 optCtrl=list(maxfun=2e5)))
lrtest.default(m1.7,m1.5)#tests body mass two days after hatching

#Model 13: removing the number of siblings from the minimal model####
m1.8<-glmer(fate~EXPERIMENTAL_GROUP_REARING+WEIGHT+site_rearing+
              time_category+
              scale(julian_hatch, scale = F)+
              (1|nestbox_rearing)+
              (1|nestbox_hatch),
            data = death_data%>%
              filter(!is.na(NUMBER_SIBLINGS_REARING)), family=binomial(link="logit"))
ss <- getME(m1.8,c("theta","fixef"))
m1.8<- update(m1.8,start=ss,control=glmerControl(optimizer="bobyqa",
                                                 optCtrl=list(maxfun=2e5))) 
lrtest.default(m1.8,m1.5)#tests the effect of the number of siblings in the minimal model

#Model 14: removing time of day from the minimal model###
m1.9<-glmer(fate~EXPERIMENTAL_GROUP_REARING+WEIGHT+site_rearing+
              scale(NUMBER_SIBLINGS_REARING, scale=F)+
              scale(julian_hatch, scale = F)+
              (1|nestbox_rearing)+
              (1|nestbox_hatch),
            data = death_data, family=binomial(link="logit"))
ss <- getME(m1.9,c("theta","fixef"))
m1.9<- update(m1.9,start=ss,control=glmerControl(optimizer="bobyqa",
                                                 optCtrl=list(maxfun=2e5)))  
lrtest.default(m1.9,m1.5)#tests the effect of time of day

#Model 15: removing hatch date from the minimal model
m1.10<-glmer(fate~EXPERIMENTAL_GROUP_REARING+WEIGHT+site_rearing+
              scale(NUMBER_SIBLINGS_REARING, scale=F)+
              time_category+
              (1|nestbox_rearing)+
              (1|nestbox_hatch),
            data = death_data, family=binomial(link="logit"))
ss <- getME(m1.10,c("theta","fixef"))
m1.10<- update(m1.10,start=ss,control=glmerControl(optimizer="bobyqa",
                                                 optCtrl=list(maxfun=2e5)))  
lrtest.default(m1.10,m1.5)#tests hatch date

#####plotting the MINIMAL model predictions (Model 9)################
newdat<-expand.grid(
  site_rearing=unique(death_data$site_rearing),
  EXPERIMENTAL_GROUP_REARING=unique(death_data$EXPERIMENTAL_GROUP_REARING),
  NUMBER_SIBLINGS_REARING=mean(as.numeric(death_data$NUMBER_SIBLINGS_REARING), na.rm = T),
  julian_hatch=mean(as.numeric(death_data$julian_hatch), na.rm = T),
  WEIGHT=unique(death_data$WEIGHT),
  time_category=c("morning", "afternoon"),
  fate=0
)

##the new predicted values and associated confidence intervals
newdat$fate<-predict(m1.5,newdat, type="response", re.form=NA)
newdat$prediction<-predict(m1.5,newdat, re.form=NA)
mm<-model.matrix(terms(m1.5),newdat)
mm<-plogis(mm)
cmult <- 1.96
pvar1 <- diag(mm %*% tcrossprod(vcov(m1.5),mm))
tvar1 <- pvar1+VarCorr(ma)$Subject
newdat2 <-data.frame(newdat,
                     plo = newdat$prediction-cmult*sqrt(pvar1),
                     phi = newdat$prediction+cmult*sqrt(pvar1))

#Changing data from the log scale for ease of plotting purposes
newdat2$prediction<-exp(newdat2$prediction)
newdat2$lower<-exp(newdat2$plo)
newdat2$upper<-exp(newdat2$phi)
newdat2$prediction<-newdat2$prediction/(1+newdat2$prediction)
newdat2$lower<-newdat2$lower/(1+newdat2$lower)
newdat2$upper<-newdat2$upper/(1+newdat2$upper)

#Selecting which factors to consider when plotting
newdat1<-newdat2%>%
  group_by(EXPERIMENTAL_GROUP_REARING, site_rearing,WEIGHT)%>%
  summarise_at(vars(prediction), 
list(name=mean))%>%
  mutate(fate=name)

newdat3<-newdat2%>%
  group_by(EXPERIMENTAL_GROUP_REARING, site_rearing, WEIGHT)%>%
  summarise_at(vars(lower), 
               list(name=mean))%>%
  mutate(mean_plo=name)

newdat4<-newdat2%>%
  group_by(EXPERIMENTAL_GROUP_REARING, site_rearing, WEIGHT)%>%
  summarise_at(vars(upper), 
               list(name=mean))%>%
  mutate(mean_phi=name)

newdat<-left_join(x=newdat3, y=newdat4%>%
                  select(EXPERIMENTAL_GROUP_REARING, site_rearing, mean_phi, WEIGHT), by=c("EXPERIMENTAL_GROUP_REARING","site_rearing", "WEIGHT"))
newdat2<- left_join(x=newdat, y=newdat1%>%
                      select(EXPERIMENTAL_GROUP_REARING, WEIGHT, site_rearing, fate), by=c("EXPERIMENTAL_GROUP_REARING", "site_rearing", "WEIGHT"))

#plotting the probability of survival from the binomial model (Model 9)
g1 <- ggplot(data = death_data,
             aes(x = WEIGHT, 
                 y = fate, 
                 colour = EXPERIMENTAL_GROUP_REARING))+
  geom_count(aes(WEIGHT, fate), death_data, alpha=0.50)+
  geom_ribbon(data = newdat2, aes(y=NULL, ymin = mean_plo, ymax = mean_phi, 
                                  color=NULL, fill = EXPERIMENTAL_GROUP_REARING),
              alpha = .15)+
  geom_line(data = newdat2, 
            aes(y=fate),
            size=1)+
  theme_classic()+
  theme(axis.title=element_text(size=15),
        axis.text=element_text(size=12)) +
  facet_wrap(~site_rearing)+
  theme(legend.position = "top",
        legend.title=element_text(size=12),
        legend.text=element_text(size=12)) +
  scale_y_continuous(name="Probability of surviving until fledging", limits=c(0,1))+
  scale_fill_manual(name = "Treatment group", values = c("#67a9cf", "pink")) +
  scale_color_manual(name = "Treatment group", values = c("#67a9cf", "pink")) +
  labs(x = "Body mass(g) two days after hatching", y = "Fate")

####saving the plot############
ggsave(filename = "./plots/fate_plot_and_day_2_bodyweight.png",
       plot = g1, 
       device = "png", 
       width = 180, 
       height = 160, 
       units = "mm",  )
