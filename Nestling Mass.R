##
##
##### SCRIPT AIM #####
##
##
#' Script to analyse nestling mass at days 2, 6, and 12
#' 
##
##
##
##
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
library(broom)
library(car)
library(rptR)
library(lmtest)
rm(list=ls())


#######importing the three separate data frames for nestlings and nestling weights so they can be joined######
allnestlings<- read.xlsx("./data/CSM_18.07.2022.xlsx",
                 colNames=T,
                 sheet = 4)

allnestlingweights<-read.xlsx("./data/CSM_18.07.2022.xlsx",
                              colNames=T,
                              sheet = 5)
allbroods<-read.xlsx("./data/CSM_18.07.2022.xlsx",
                          colNames=T,
                          sheet = 1)
colnames(allbroods)[colnames(allbroods)%in%("NESTBOX")] <- ("nestbox_hatch")#making sure that I can join the nestbox columns
  
allbroods$nestbox_hatch<-as.factor(allbroods$nestbox_hatch)

#######performing left join using dplyr#######
data1<-left_join(x=allnestlings, y=allnestlingweights%>%
                   select(LOCATION, time_category, RING_NUMBER,exp_period, DATE, TIME, AGE, WEIGHT, TARSUS, WING, BLOOD_ID, SAMPLE_ID), by='RING_NUMBER')
data1$nestbox_hatch<-as.factor(data1$nestbox_hatch)
data0<-left_join(x=data1, y=allbroods%>%
                  select(nestbox_hatch, EXPERIMENTAL_GROUP,CROSS_FOSTER_GROUP ,IN_NEST_CLUTCH_SIZE, NUMBER_EGGS_LAID, NUMBER_HATCHED_EGGS)%>%
                  rename(EXPERIMENTAL_GROUP_HATCH=EXPERIMENTAL_GROUP)%>%
                  rename(IN_NEST_CLUTCH_SIZE_HATCH=IN_NEST_CLUTCH_SIZE)%>%
                  rename(NUMBER_HATCHED_EGGS_HATCH=NUMBER_HATCHED_EGGS)%>%
                  rename(NUMBER_EGGS_LAID_HATCH=NUMBER_EGGS_LAID),
                by="nestbox_hatch")

#renaming columns
colnames(allbroods)[colnames(allbroods)%in%("nestbox_hatch")] <- ("nestbox_rearing")
allbroods$nestbox_rearing<-as.factor(allbroods$nestbox_rearing)
data0$nestbox_rearing<-as.factor(data0$nestbox_rearing)

###creating the final dataset############
data<-left_join(x=data0, y=allbroods%>%
                  select(nestbox_rearing,CROSS_FOSTER_GROUP ,EXPERIMENTAL_GROUP, IN_NEST_CLUTCH_SIZE,NUMBER_EGGS_LAID, NUMBER_HATCHED_EGGS)%>%
                  rename(EXPERIMENTAL_GROUP_REARING=EXPERIMENTAL_GROUP)%>%
                  rename(IN_NEST_CLUTCH_SIZE_REARING=IN_NEST_CLUTCH_SIZE)%>%
                  rename(NUMBER_HATCHED_EGGS_REARING=NUMBER_HATCHED_EGGS)%>%
                  rename(NUMBER_EGGS_LAID_REARING=NUMBER_EGGS_LAID),
                by="nestbox_rearing")

length(unique(data$RING_NUMBER))#making sure that all the ring numbers chicks have are unique
#data is now grouped, so that each nestling has been assigned it's weight, fate. cross-foster group and experimental group
#filtering data to remove measurements that were taken on days 1 and 4 (these are individual replicates)
data<-data%>%
  filter(!AGE%in%c(1, 4))

#organising columnS so the data is of the correct type
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
data$time_category <- factor(data$time_category, levels=c("morning","afternoon"))
data$AGE <- factor(data$AGE, levels=c("2", "6", "12"))

#renaming kelvingrove and GCU to "urban" and SCENE to "Forest
(data["site_hatch"][data["site_hatch"]=="GCU"]<-"Urban")
(data["site_hatch"][data["site_hatch"]=="Kelvingrove_park"]<-"Urban")
(data["site_hatch"][data["site_hatch"]=="SCENE"]<-"Forest")
(data["site_rearing"][data["site_rearing"]=="GCU"]<-"Urban")
(data["site_rearing"][data["site_rearing"]=="Kelvingrove_park"]<-"Urban")
(data["site_rearing"][data["site_rearing"]=="SCENE"]<-"Forest")


####################STATISTICAL MODELS############################

##Part 1: Nestling mass at Day 2##########

#Filtering down to day 2
data2<-data%>%
  filter(AGE==2)%>%
  mutate(WEIGHT=round(WEIGHT, 1))


#########. Models for nestling body mass at 2 days old######
data$WEIGHT<-as.numeric(data$WEIGHT)

###Model 1: with hatch date fitted as a quadratic#####
m0 <-lmer(WEIGHT~site_hatch*EXPERIMENTAL_GROUP_HATCH+time_category+
            scale(NUMBER_HATCHED_EGGS_HATCH, scale=F)+
            poly(julian_hatch, 2)[,2] +
            poly(julian_hatch, 2)[,1]+
            (1|nestbox_hatch),
          data = data2%>%
            filter(AGE==2), REML=F)
check_model(m0, panel =F)

#Model 2: With hatch date fitted linearly
ma <-lmer(WEIGHT~site_hatch*EXPERIMENTAL_GROUP_HATCH+time_category+
           scale(NUMBER_HATCHED_EGGS_HATCH, scale=F)+
           scale(julian_hatch, scale = F)+
           (1|nestbox_hatch),
         data = data2%>%
           filter(AGE==2), REML=F)
summary(ma)
check_model(ma, panel=F)
lrtest.default(m0,ma)#lrtest for hatch date (fitted as a quadratic)

###doing bootstrapping to get the confidence intervals for the model
FUN <- function(fit) {return(fixef(fit))} 
merBoot<-bootMer(ma, FUN, nsim=500)
CI.lower = apply(merBoot$t, 2, function(x) as.numeric(quantile(x, probs=.025, na.rm=TRUE)))
CI.upper = apply(merBoot$t, 2, function(x) as.numeric(quantile(x, probs=.95, na.rm=TRUE)))
confint.merMod(ma)

####Getting the likelihood ratio values for all other terms in the model#########

###Model 3: model with no interactions (which is also the minimal model as there were no significant interaction effects)
m1 <- lmer(WEIGHT~site_hatch+EXPERIMENTAL_GROUP_HATCH+time_category+
             scale(NUMBER_HATCHED_EGGS_HATCH, scale=F)+
             scale(julian_hatch, scale = F)+
             (1|nestbox_hatch),
           data = data2%>%
             filter(AGE==2), REML=F)
summary(m1)
check_model(m1, panel=F)
lm1<-lrtest.default(m1,ma)#gives likelihood ratio for two way interaction between location*treatment group

#calculates the confidence intervals using parametric bootstrapping
FUN <- function(fit) {return(fixef(fit))} 
merBoot<-bootMer(m1, FUN, nsim=500)
CI.lower = apply(merBoot$t, 2, function(x) as.numeric(quantile(x, probs=.025, na.rm=TRUE)))
CI.upper = apply(merBoot$t, 2, function(x) as.numeric(quantile(x, probs=.95, na.rm=TRUE)))

###Model 4: model with no hatch date
m3 <- lmer(WEIGHT~site_hatch*EXPERIMENTAL_GROUP_HATCH+time_category+
             scale(NUMBER_HATCHED_EGGS_HATCH, scale=F)+
             (1|nestbox_hatch),
           data = data2%>%
             filter(AGE==2), REML=F)
lm3<-lrtest.default(m3,ma)#gives likelihood ratio for hatch date


###Model 5: model with no number of siblings
m4 <- lmer(WEIGHT~site_hatch*EXPERIMENTAL_GROUP_HATCH+time_category+
             scale(julian_hatch, scale = F)+
             (1|nestbox_hatch),
           data = data2%>%
             filter(AGE==2), REML=F)

lm4<-lrtest.default(m4,ma)#gives likelihood ratio for number of siblings in the nest

###Model 6: model with no time of day
m5 <- lmer(WEIGHT~site_hatch*EXPERIMENTAL_GROUP_HATCH+
             scale(NUMBER_HATCHED_EGGS_HATCH, scale=F)+
             scale(julian_hatch, scale = F)+
             (1|nestbox_hatch),
           data = data2%>%
             filter(AGE==2), REML=F)
lm4<-lrtest.default(m5,ma)#gives likelihood ratio for time of day

####Model 7: Minimal model with no habitat###
m6<- lmer(WEIGHT~EXPERIMENTAL_GROUP_HATCH+time_category+
              scale(NUMBER_HATCHED_EGGS_HATCH, scale=F)+
              scale(julian_hatch, scale = F)+
              (1|nestbox_hatch),
            data = data2%>%
              filter(AGE==2), REML=F)
lrtest.default(m6,m1)#lrtest for habitat

####Model 8: Minimal model with no experimental group
m7<- lmer(WEIGHT~site_hatch+time_category+
              scale(NUMBER_HATCHED_EGGS_HATCH, scale=F)+
              scale(julian_hatch, scale = F)+
              (1|nestbox_hatch),
            data = data2%>%
              filter(AGE==2), REML=F)
lrtest.default(m7,m1)#lr test for treatment group

#Model 9: Minimal model with no time of weighing
m8<- lmer(WEIGHT~site_hatch+EXPERIMENTAL_GROUP_HATCH+
              scale(NUMBER_HATCHED_EGGS_HATCH, scale=F)+
              scale(julian_hatch, scale = F)+
              (1|nestbox_hatch),
            data = data2%>%
              filter(AGE==2), REML=F)
lrtest.default(m8,m1)#lrtest for time category

#Model 10: Minimal model with no number sibs
m9<- lmer(WEIGHT~site_hatch+EXPERIMENTAL_GROUP_HATCH+time_category+
              scale(julian_hatch, scale = F)+
              (1|nestbox_hatch),
            data = data2%>%
              filter(AGE==2), REML=F)
lrtest.default(m9,m1)#lrtest for number sibs

#Model 11: Minimal model with no hatch date
m10<- lmer(WEIGHT~site_hatch+EXPERIMENTAL_GROUP_HATCH+time_category+
              scale(NUMBER_HATCHED_EGGS_HATCH, scale=F)+
              (1|nestbox_hatch),
            data = data2%>%
              filter(AGE==2), REML=F)
lrtest.default(m10,m1)#lrtest for hatch date

#####Plotting the predictions for day 2 body mass (from model 2)##############
#creating a dummy dataset for day 2 only
newdat<-expand.grid(
  EXPERIMENTAL_GROUP_HATCH=unique(data2$EXPERIMENTAL_GROUP_HATCH),
  NUMBER_HATCHED_EGGS_HATCH=mean(as.numeric(data2$NUMBER_HATCHED_EGGS_HATCH), na.rm = T),
  site_hatch=unique(data2$site_hatch),
  time_category=unique(data2$time_category),
  julian_hatch=mean(as.numeric(data2$julian_hatch), na.rm = T),
  WEIGHT=0
)

#getting the predicted values
newdat$WEIGHT<-predict(m1, newdat, re.form=NA)
mm<-model.matrix(terms(m1),newdat)
pvar1 <- diag(mm %*% tcrossprod(vcov(m1),mm))
tvar1 <- pvar1+VarCorr(m1)$Subject[1]  
cmult <- 1.96
newdat <-data.frame(newdat,
                     plo = newdat$WEIGHT-cmult*sqrt(pvar1),
                     phi = newdat$WEIGHT+cmult*sqrt(pvar1))

#selecting only the variables to plot for the figure
newdat1<-newdat%>%
  group_by(EXPERIMENTAL_GROUP_HATCH, site_hatch)%>%
  summarise_at(vars(WEIGHT), 
               list(name=mean))%>%
  mutate(WEIGHT=name)

newdat3<-newdat%>%
  group_by(EXPERIMENTAL_GROUP_HATCH, site_hatch)%>% 
  summarise_at(vars(plo), 
               list(name=mean))%>%
  mutate(plo=name)

newdat4<-newdat%>%
  group_by(EXPERIMENTAL_GROUP_HATCH, site_hatch)%>%
  summarise_at(vars(phi), 
               list(name=mean))%>%
  mutate(phi=name)

newdat<-left_join(x=newdat3, y=newdat4%>%
                    select(EXPERIMENTAL_GROUP_HATCH, site_hatch, phi), by=c("EXPERIMENTAL_GROUP_HATCH", "site_hatch"))
newdat2<- left_join(x=newdat, y=newdat1%>%
                      select(EXPERIMENTAL_GROUP_HATCH,site_hatch, WEIGHT), by=c("EXPERIMENTAL_GROUP_HATCH","site_hatch"))

##############ggplotting the predicted values for nestling body mass 2 days after hatching##########
PREDICTED_PLOT<- ggplot(data = data2%>%
                          filter(AGE==2)%>%
                          mutate(round(WEIGHT==WEIGHT,1)), aes(x = site_hatch, 
                                                y = WEIGHT, 
                                                fill = EXPERIMENTAL_GROUP_HATCH)) +
  geom_point(position = position_jitterdodge(dodge.width = 0.60,
                                             jitter.width = 0.18),
             size = 3.5,
             shape = 21,
             color = "white",
             alpha = 0.5) +
  geom_linerange(data = newdat2, 
                 aes(y = WEIGHT, 
                     ymin = plo, 
                     ymax = phi),
                 position = position_dodge(width = 0.60),
                 width = 0) +
  geom_point(data = newdat2, 
             aes(x = site_hatch, y = WEIGHT, 
                 fill = EXPERIMENTAL_GROUP_HATCH),
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

# to save plot
  ggsave(filename = "./plots/predicted_DAY2_CHICKWEIGHT.png",
         plot = PREDICTED_PLOT, 
         device = "png", 
         width = 250, 
         height = 200, 
         units = "mm",  )

##Part 2: Nestling Body Mass at day 6 after hatching######
#filtering out nestling mass on days 2 and 12
  dat<-data%>%
    filter(AGE!=(2))%>%
    filter(AGE!=(12))

##Model 12: model with hatch date fitted as a quadratic
m11<-lmer(WEIGHT~
              site_hatch+
              EXPERIMENTAL_GROUP_HATCH+
              scale(NUMBER_HATCHED_EGGS_REARING, scale=F)+
              EXPERIMENTAL_GROUP_REARING*site_rearing+
              poly(julian_hatch, 2)[,2] +
              poly(julian_hatch, 2)[,1]+
              time_category+
              (1|nestbox_hatch)+
              (1|nestbox_rearing),
            data = dat,REML=F)
summary(m11)
check_model(m11, panel=F)  

#Model 13: with hatch date fitted linearly
  m12<-lmer(WEIGHT~
             site_hatch+
             EXPERIMENTAL_GROUP_HATCH+
             scale(NUMBER_HATCHED_EGGS_REARING, scale=F)+
             EXPERIMENTAL_GROUP_REARING*site_rearing+
             scale(julian_hatch, scale = F)+
             time_category+
             (1|nestbox_hatch)+
             (1|nestbox_rearing),
           data = dat,REML=F)
  lrtest.default(m12,m11)#lrtest for hatch date when fitted as a quadratic
  summary(m12)####drop three way interaction as it's not significant
  check_model(m12, panel=F)

##getting the confidence intervals for the model with the three way interaction (m12)
###doing bootsrapping to get the confidence intervals
  FUN <- function(fit) {return(fixef(fit))} 
  merBoot<-bootMer(m12, FUN, nsim=500)
  CI.lower = apply(merBoot$t, 2, function(x) as.numeric(quantile(x, probs=.025, na.rm=TRUE)))
  CI.upper = apply(merBoot$t, 2, function(x) as.numeric(quantile(x, probs=.95, na.rm=TRUE)))
  
  
#getting the likelihood ratio test values for the nestling body mass at day 6 model#####

#Model 14: model without site of hatching
  m13<-lmer(WEIGHT~
             EXPERIMENTAL_GROUP_HATCH+
             scale(NUMBER_HATCHED_EGGS_REARING, scale=F)+
             EXPERIMENTAL_GROUP_REARING*site_rearing+
             scale(julian_hatch, scale = F)+
             time_category+
             (1|nestbox_hatch)+
             (1|nestbox_rearing),
           data = dat,REML=F)
  lrtest.default(m12,m13)#lrtest for site of hatching
  
#Model 15: model without treatment group in nest of hatching
  m14<-lmer(WEIGHT~
             site_hatch+
             #EXPERIMENTAL_GROUP_HATCH+
             scale(NUMBER_HATCHED_EGGS_REARING, scale=F)+
             EXPERIMENTAL_GROUP_REARING*site_rearing+
             scale(julian_hatch, scale = F)+
             time_category+
             (1|nestbox_hatch)+
             (1|nestbox_rearing),
           data = dat,REML=F)
  lrtest.default(m14,m12)#lrtest for treatment group in the nest of hatching
  
#Model 16: model without number of sibs
  m15<-lmer(WEIGHT~
             site_hatch+
             EXPERIMENTAL_GROUP_HATCH+
             EXPERIMENTAL_GROUP_REARING*site_rearing+
             scale(julian_hatch, scale = F)+
             time_category+
             (1|nestbox_hatch)+
             (1|nestbox_rearing),
           data = dat%>%
             filter(!is.na(NUMBER_HATCHED_EGGS_REARING)), REML=F)
  lrtest.default(m15,m12)#lr test for number of siblings
  
#Model 17: model without time of day
  m16<-lmer(WEIGHT~
             site_hatch+
             EXPERIMENTAL_GROUP_HATCH+
             scale(NUMBER_HATCHED_EGGS_REARING, scale=F)+
             EXPERIMENTAL_GROUP_REARING*site_rearing+
             scale(julian_hatch, scale = F)+
             (1|nestbox_hatch)+
             (1|nestbox_rearing),
           data = dat, REML=F)
  lrtest.default(m16,m12)#lrtest for time of day the nenstling was weighed
  
#Model 18: model with no hatch date
  m17<-lmer(WEIGHT~
             site_hatch+
             EXPERIMENTAL_GROUP_HATCH+
             scale(NUMBER_HATCHED_EGGS_REARING, scale=F)+
             time_category+
             EXPERIMENTAL_GROUP_REARING*site_rearing+
             (1|nestbox_hatch)+
             (1|nestbox_rearing),
           data = dat, REML=F)
  lrtest.default(m17,m12)#
  
#Model 18: The minimal model with no interaction effect
  m17<-lmer(WEIGHT~
             site_hatch+
             EXPERIMENTAL_GROUP_HATCH+
             scale(NUMBER_HATCHED_EGGS_REARING, scale=F)+
             time_category+
             scale(julian_hatch, scale = F)+
             EXPERIMENTAL_GROUP_REARING+site_rearing+
             (1|nestbox_hatch)+
             (1|nestbox_rearing),
           data = dat, REML=F)
  lrtest.default(m17,m12)#Gives the lrtest value for the three way interaction
  summary(m17)
  check_model(m17, panel=F)
  
###doing bootstrapping to get the confidence intervals for the minimal model
    FUN <- function(fit) {return(fixef(fit))} 
    merBoot<-bootMer(m17, FUN, nsim=500)
    CI.lower = apply(merBoot$t, 2, function(x) as.numeric(quantile(x, probs=.025, na.rm=TRUE)))
    CI.upper = apply(merBoot$t, 2, function(x) as.numeric(quantile(x, probs=.95, na.rm=TRUE)))
    
####Geting the lrtest values for the minimal model coefficients (minimal model is m17)######

##Model 19: Minimal model without the site of hatching##
m18<-lmer(WEIGHT~
               #site_hatch+
               EXPERIMENTAL_GROUP_HATCH+
               scale(NUMBER_HATCHED_EGGS_REARING, scale=F)+
               time_category+
               scale(julian_hatch, scale = F)+
               EXPERIMENTAL_GROUP_REARING+site_rearing+
               (1|nestbox_hatch)+
               (1|nestbox_rearing),
             data = dat, REML=F)
lrtest.default(m18,m17)#for site of hatching
  
##Model 20: Minimal model without the site of rearing
m19<-lmer(WEIGHT~
               site_hatch+
               EXPERIMENTAL_GROUP_HATCH+
               scale(NUMBER_HATCHED_EGGS_REARING, scale=F)+
               time_category+
               scale(julian_hatch, scale = F)+
               EXPERIMENTAL_GROUP_REARING+#site_rearing+
               (1|nestbox_hatch)+
               (1|nestbox_rearing),
             data = dat, REML=F)
lrtest.default(m19,m17)#for site of rearing
    
#Model 21: Minimal model without treatment group in the nest of hatching
    m20<-lmer(WEIGHT~
               site_hatch+
               #EXPERIMENTAL_GROUP_HATCH+
               scale(NUMBER_HATCHED_EGGS_REARING, scale=F)+
               time_category+
               scale(julian_hatch, scale = F)+
               EXPERIMENTAL_GROUP_REARING+
                site_rearing+
               (1|nestbox_hatch)+
               (1|nestbox_rearing),
             data = dat, REML=F)
    lrtest.default(m20,m17)#for treatment group of hatching

#Model 22: Minimal model without treatment group in the nest of rearing
    m21<-lmer(WEIGHT~
                site_hatch+
                EXPERIMENTAL_GROUP_HATCH+
                scale(NUMBER_HATCHED_EGGS_REARING, scale=F)+
                time_category+
                scale(julian_hatch, scale = F)+
                #EXPERIMENTAL_GROUP_REARING
                +site_rearing+
                (1|nestbox_hatch)+
                (1|nestbox_rearing),
              data = dat, REML=F)
    lrtest.default(m21,m17)#treatment group for the nest of rearing

##Model 23: Minimal Model without hatch date
    m22<-lmer(WEIGHT~
                site_hatch+
                EXPERIMENTAL_GROUP_HATCH+
                scale(NUMBER_HATCHED_EGGS_REARING, scale=F)+
                time_category+
               # scale(julian_hatch, scale = F)+
                EXPERIMENTAL_GROUP_REARING
                +site_rearing+
                (1|nestbox_hatch)+
                (1|nestbox_rearing),
              data = dat, REML=F)
    lrtest.default(m22,m17)#for hatch date
    
#Model 24: Minimal Model without number of siblings
    m23<-lmer(WEIGHT~
                site_hatch+
                EXPERIMENTAL_GROUP_HATCH+
                time_category+
                scale(julian_hatch, scale = F)+
                EXPERIMENTAL_GROUP_REARING
              +site_rearing+
                (1|nestbox_hatch)+
                (1|nestbox_rearing),
              data = dat%>%
                filter(!is.na(NUMBER_HATCHED_EGGS_REARING)), REML=F)
    lrtest.default(m23,m17)#for the number of siblings
    
#Model 25: Minimal Model without time of day
    m24<-lmer(WEIGHT~
                site_hatch+
                EXPERIMENTAL_GROUP_HATCH+
                scale(NUMBER_HATCHED_EGGS_REARING, scale=F)+
                #time_category+
                scale(julian_hatch, scale = F)+
                EXPERIMENTAL_GROUP_REARING
              +site_rearing+
                (1|nestbox_hatch)+
                (1|nestbox_rearing),
              data = dat, REML=F)
    lrtest.default(m24,m17)#for hatch date
    
    
###Part 3: Body mass at day 12 model####
    
#filtering out day 2 and 6 body mass
    dat2<-data%>%
      filter(AGE!=(2))%>%
      filter(AGE!=(6))
    
    
##Model 26: Day 12 body mass with hatch date fitted as a quadratic
m25<-lmer(WEIGHT~
                site_hatch+
                EXPERIMENTAL_GROUP_HATCH+
                scale(NUMBER_HATCHED_EGGS_REARING, scale=F)+
                EXPERIMENTAL_GROUP_REARING*site_rearing+
                poly(julian_hatch, 2)[,2] +
                poly(julian_hatch, 2)[,1]+
                time_category+
                (1|nestbox_hatch)+
                (1|nestbox_rearing),
              data = dat2,REML=F)
    summary(m25)
    check_model (m25,panel=F)

#Model 27: Day 12 body mass with hatch date fitted linearly####
    m26<-lmer(WEIGHT~
               site_hatch+
               EXPERIMENTAL_GROUP_HATCH+
               scale(NUMBER_HATCHED_EGGS_REARING, scale=F)+
               EXPERIMENTAL_GROUP_REARING*site_rearing+
               scale(julian_hatch, scale = F)+
               time_category+
               (1|nestbox_hatch)+
               (1|nestbox_rearing),
             data = dat2,REML=F)
lrtest.default(m26,m25)#lrtest for hatch date when fitted as a quadratic
summary(m26)
check_model(m26, panel=F)
    
 ##getting the confidence intervals for m26 (the model with the three-way interaction)
###doing bootstrapping to get the confidence intervals
    FUN <- function(fit) {return(fixef(fit))} 
    merBoot<-bootMer(m26, FUN, nsim=500)
    CI.lower = apply(merBoot$t, 2, function(x) as.numeric(quantile(x, probs=.025, na.rm=TRUE)))
    CI.upper = apply(merBoot$t, 2, function(x) as.numeric(quantile(x, probs=.95, na.rm=TRUE)))
    
#####likelihood ratio test values for the nestling body mass at day 12 Global model#####

#Model 28: model without the site of hatching
m27<-lmer(WEIGHT~
               EXPERIMENTAL_GROUP_HATCH+
               scale(NUMBER_HATCHED_EGGS_REARING, scale=F)+
               EXPERIMENTAL_GROUP_REARING*site_rearing+
               scale(julian_hatch, scale = F)+
               time_category+
               (1|nestbox_hatch)+
               (1|nestbox_rearing),
             data = dat2,REML=F)
lrtest.default(m27,m25)#lrtest for site of hatching
    
#Model 29: model without treatment group hatching
    m30<-lmer(WEIGHT~
               site_hatch+
               #EXPERIMENTAL_GROUP_HATCH+
               scale(NUMBER_HATCHED_EGGS_REARING, scale=F)+
               EXPERIMENTAL_GROUP_REARING*site_rearing+
               scale(julian_hatch, scale = F)+
               time_category+
               (1|nestbox_hatch)+
               (1|nestbox_rearing),
             data = dat2,REML=F)
lrtest.default(m30,m25)#lrtest for treatment group in the nest of hatching
    
#Model 30: model without number of sibs
    m29<-lmer(WEIGHT~
               site_hatch+
               EXPERIMENTAL_GROUP_HATCH+
               EXPERIMENTAL_GROUP_REARING*site_rearing+
               scale(julian_hatch, scale = F)+
               time_category+
               (1|nestbox_hatch)+
               (1|nestbox_rearing),
             data = dat2%>%
               filter(!is.na(NUMBER_HATCHED_EGGS_REARING)), REML=F)
lrtest.default(m29,m25)#lr test for number of siblings
    
    
#Model 31: model without time of day
    m30<-lmer(WEIGHT~
               site_hatch+
               EXPERIMENTAL_GROUP_HATCH+
               scale(NUMBER_HATCHED_EGGS_REARING, scale=F)+
               EXPERIMENTAL_GROUP_REARING*site_rearing+
               scale(julian_hatch, scale = F)+
               (1|nestbox_hatch)+
               (1|nestbox_rearing),
             data = dat2, REML=F)
lrtest.default(m30,m25)#lrtest for time of day the nenstling was weighed
    
#Model 32: model with no hatch date
    m31<-lmer(WEIGHT~
               site_hatch+
               EXPERIMENTAL_GROUP_HATCH+
               scale(NUMBER_HATCHED_EGGS_REARING, scale=F)+
               time_category+
               EXPERIMENTAL_GROUP_REARING*site_rearing+
               (1|nestbox_hatch)+
               (1|nestbox_rearing),
             data = dat2, REML=F)
lrtest.default(m31,m25)#
    
#Model 33: The minimal model with no interaction effect#####
m32<-lmer(WEIGHT~
               site_hatch+
               EXPERIMENTAL_GROUP_HATCH+
               scale(NUMBER_HATCHED_EGGS_REARING, scale=F)+
               time_category+
               scale(julian_hatch, scale = F)+
               EXPERIMENTAL_GROUP_REARING+site_rearing+
               (1|nestbox_hatch)+
               (1|nestbox_rearing),
             data = dat2, REML=F)
lrtest.default(m32,m25)#lrtest value for the interaction effect for the day 12 bdyweight model
  

###Part 4: Are there differences in the weight of cross-fostered nestlings?######
datacross<-data2 %>%
  mutate(crossfoster = case_when(
    nestbox_hatch==nestbox_rearing ~ "No",
    nestbox_hatch!=nestbox_rearing ~ "Yes"
  ))
#model testing if there are differences in bodyweight between cross-fostered siblings
mcross<-lmer(WEIGHT~site_hatch*crossfoster+
               scale(NUMBER_HATCHED_EGGS_HATCH, scale=F)+
               scale(julian_hatch, scale = F)+
               (1|nestbox_hatch), data = datacross)
summary(mcross)#model with interaction fitted

mcrosss<-lmer(WEIGHT~site_hatch+crossfoster+
                scale(NUMBER_HATCHED_EGGS_HATCH, scale=F)+
                scale(julian_hatch, scale = F)+
                (1|nestbox_hatch), data = dataa)
summary(mcrosss)#model without interaction
lrtest.default(mcross, mcrosss)#lrtest values for cross-fostered sibling bodymass


