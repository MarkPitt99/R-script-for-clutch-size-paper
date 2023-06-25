###############script aim: To analyse how both the treatment group and habitat affected the number of offspring alive and the proportion of nestlings alive##########

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
library(forcats)
library(lmtest)
library(emmeans)
library (survival)
library(survminer)

#clearing workspace####
rm(list=ls())

#IMPORTING DATASETS#############

data<- read.xlsx("./data/CSM_18.07.2022.xlsx",
                 colNames=T,
                 sheet = 1)


#converting from wide-format to long format#######
survival1<-gather(data, Day, Number_chicks_alive,BROODSIZE_DAY3_PREMANIPULATION, BROODSIZE_DAY7, BROODSIZE_DAY13,NUMBER_FLEDGLINGS)

####filtering out cells with NA and only including SCENE (the forest site) and Kelvingrove (the city site)
survival<- survival1%>%
  filter(LOCATION%in% c("SCENE", "KELVINGROVE"))%>%
  filter(!is.na(Number_chicks_alive))%>%
  filter(!is.na(IN_NEST_CLUTCH_SIZE))%>%
  filter(!is.na(NUMBER_HATCHED_EGGS))

###Organising the data so that it is of the correct type for analysis###########
survival$LOCATION<-as.factor(survival$LOCATION)
survival$NESTBOX<-as.factor(survival$NESTBOX)
survival$EXPERIMENTAL_GROUP<-as.factor(survival$EXPERIMENTAL_GROUP)
survival$julian_1st_egg<-as.numeric(survival$julian_1st_egg)
survival$julian_hatch_date<-as.numeric(survival$julian_hatch_date)
survival$Day<-as.factor(survival$Day)
survival$Day <- factor(survival$Day, levels=c("BROODSIZE_DAY3_PREMANIPULATION", "BROODSIZE_DAY7", "BROODSIZE_DAY13", "NUMBER_FLEDGLINGS"))#Making sure the nestlings age is in the right order, for both analysis and plotting purposes

###########changing the names of Kelvingrove and SCENE to urban and forest###############
survival<- survival %>%
  mutate(LOCATION = fct_recode(LOCATION,
                               "Urban" = "KELVINGROVE",
                               "Forest" = "SCENE"))

#########STATISTICAL MODELS##########

###PART 1: Number of chicks alive model (expressed as a number of nestlings in the nest at each stage, with in nest clutch size included as a fixed effect).######

####Model 1: The global Model where hatch date is fitted as a quadratic term####
m0.q <- lmer(Number_chicks_alive~
             scale(IN_NEST_CLUTCH_SIZE, scale=F)+
             EXPERIMENTAL_GROUP*LOCATION*Day+
             poly(julian_hatch_date, 2)[,2] +
             poly(julian_hatch_date, 2)[,1]+
             (1|NESTBOX),
           data = survival, REML = F)
check_model(m0.q, panel = F)
summary(m0.q)

#Model 2: The global model where hatch date is fitted as a linear term########
m0 <- lmer(Number_chicks_alive~
               scale(IN_NEST_CLUTCH_SIZE, scale=F)+
               EXPERIMENTAL_GROUP*LOCATION*Day+
               scale(julian_hatch_date, scale = F)+
               (1|NESTBOX),
             data = survival, REML = F)
summary(m0)
check_model(m0, panel = F)
lrtest.default(m0,m0.q)#lrtest for hatch date fitted as a quadratic term

##Bootstrapping for confidence intervals####
FUN <- function(fit) {return(fixef(fit))} 
merBoot<-bootMer(m0, FUN, nsim=500)
CI.lower = apply(merBoot$t, 2, function(x) as.numeric(quantile(x, probs=.025, na.rm=TRUE)))
CI.upper = apply(merBoot$t, 2, function(x) as.numeric(quantile(x, probs=.95, na.rm=TRUE)))

######Getting the likelikood ratio values for every term in the model####
###Model 3: model with two way interactions only --> to get the likelihood ratio values for the three way interaction (location*treatment group*day)
m1 <- lmer(Number_chicks_alive~
             scale(IN_NEST_CLUTCH_SIZE, scale=F)+
             EXPERIMENTAL_GROUP*LOCATION+LOCATION*Day+
             EXPERIMENTAL_GROUP*Day+
             scale(julian_hatch_date, scale = F)+
             (1|NESTBOX),
           data = survival, REML = F)
lm1<-lrtest.default(m0,m1)#gives likelihood ratio for three way interaction between location*day*treatment group

###Model 4: Model with no clutch size
m2 <- lmer(Number_chicks_alive~
             EXPERIMENTAL_GROUP*LOCATION*Day+
             scale(julian_hatch_date, scale = F)+
             (1|NESTBOX),
           data = survival, REML = F)
lm2<-lrtest.default(m2,m0)#gives likelihood ratio for clutch size

###Model 5: Model with no hatch date
m3 <- lmer(Number_chicks_alive~
             scale(IN_NEST_CLUTCH_SIZE, scale=F)+
             EXPERIMENTAL_GROUP*LOCATION*Day+
             (1|NESTBOX),
           data = survival, REML = F)
lm3<-lrtest.default(m3,m0)#gives likelihood ratio for hatch date


######plotting the predicted values from model 2###################
newdat<-expand.grid(
  EXPERIMENTAL_GROUP=unique(survival$EXPERIMENTAL_GROUP),
  Day=unique(survival$Day),
  LOCATION=unique(survival$LOCATION),
  julian_hatch_date=mean(as.numeric(survival$julian_hatch_date), na.rm = T),
  Number_chicks_alive=0
)

##
## because clutch size is so different across the groups that you want to plot and so influential in the prediction
## you need to adjust the predictions to the mean clutch size of each habitat and group (rather than using the same clutch size value
## to predict across all groups)

# mean clutch size of controls in the city
newdat$IN_NEST_CLUTCH_SIZE[newdat$EXPERIMENTAL_GROUP == "CONTROL" & newdat$LOCATION == "Urban"] <- 
  mean(survival$IN_NEST_CLUTCH_SIZE[survival$EXPERIMENTAL_GROUP == "CONTROL" & survival$LOCATION == "Urban"])

# mean clutch size of controls in the forest
newdat$IN_NEST_CLUTCH_SIZE[newdat$EXPERIMENTAL_GROUP == "CONTROL" & newdat$LOCATION == "Forest"] <- 
  mean(survival$IN_NEST_CLUTCH_SIZE[survival$EXPERIMENTAL_GROUP == "CONTROL" & survival$LOCATION == "Forest"])

# mean clutch size of experimental nests in the City
newdat$IN_NEST_CLUTCH_SIZE[newdat$EXPERIMENTAL_GROUP == "EXPERIMENTAL" & newdat$LOCATION == "Urban"] <- 
  mean(survival$IN_NEST_CLUTCH_SIZE[survival$EXPERIMENTAL_GROUP == "EXPERIMENTAL" & survival$LOCATION == "Urban"])

# mean clutch size of experimental nests in the Forest
newdat$IN_NEST_CLUTCH_SIZE[newdat$EXPERIMENTAL_GROUP == "EXPERIMENTAL" & newdat$LOCATION == "Forest"] <- 
  mean(survival$IN_NEST_CLUTCH_SIZE[survival$EXPERIMENTAL_GROUP == "EXPERIMENTAL" & survival$LOCATION == "Forest"])

# mean brood size of controls in the city
newdat$NUMBER_HATCHED_EGGS[newdat$EXPERIMENTAL_GROUP == "CONTROL" & newdat$LOCATION == "Urban"] <- 
  mean(survival$NUMBER_HATCHED_EGGS[survival$EXPERIMENTAL_GROUP == "CONTROL" & survival$LOCATION == "Urban"])

# mean BROOD size of cotrols in the forest
newdat$NUMBER_HATCHED_EGGS[newdat$EXPERIMENTAL_GROUP == "CONTROL" & newdat$LOCATION == "Forest"] <- 
  mean(survival$NUMBER_HATCHED_EGGS[survival$EXPERIMENTAL_GROUP == "CONTROL" & survival$LOCATION == "Forest"])

# mean BROOD size experimental nests in the city
newdat$NUMBER_HATCHED_EGGS[newdat$EXPERIMENTAL_GROUP == "EXPERIMENTAL" & newdat$LOCATION == "Urban"] <- 
  mean(survival$NUMBER_HATCHED_EGGS[survival$EXPERIMENTAL_GROUP == "EXPERIMENTAL" & survival$LOCATION == "Urban"])

# mean BROOD size of experimental nests in the city
newdat$NUMBER_HATCHED_EGGS[newdat$EXPERIMENTAL_GROUP == "EXPERIMENTAL" & newdat$LOCATION == "Forest"] <- 
  mean(survival$NUMBER_HATCHED_EGGS[survival$EXPERIMENTAL_GROUP == "EXPERIMENTAL" & survival$LOCATION == "Forest"])

##the new predicted values and associated confidence intervals##############
newdat$Number_chicks_alive<-predict(m0, newdat, re.form=NA)
mm<-model.matrix(terms(m0),newdat)
mm<-plogis(mm)
pvar1 <- diag(mm %*% tcrossprod(vcov(m0),mm))
tvar1 <- pvar1+VarCorr(m0)$Subject
cmult <- 1.96
newdat2 <-data.frame(newdat,
                     plo = newdat$Number_chicks_alive-cmult*sqrt(pvar1),
                     phi = newdat$Number_chicks_alive+cmult*sqrt(pvar1))####these values seeem waay too high


###plotting the predicted values from model 2######################
g0 <- ggplot(data = survival, 
             aes(x = Day, 
                 y = Number_chicks_alive, 
                 colour = EXPERIMENTAL_GROUP)) +
  geom_point(position = position_jitter(width = 0.15),
             alpha = 0.5, size=3)+
  geom_linerange(data = newdat2, 
                 aes(y = Number_chicks_alive, 
                     ymin = plo, 
                     ymax = phi),
                 position = position_dodge(width = 0.60),
                 width = 0)+
  geom_point(data = newdat, 
             aes(x = as.numeric(Day), y = Number_chicks_alive, 
                 fill = EXPERIMENTAL_GROUP),
             position = position_dodge(width = 0.60),
             color = "black", 
             size = 8,
             shape = 21) +
  geom_line(data = newdat, 
            aes(x = as.numeric(Day), y = Number_chicks_alive, 
                fill = EXPERIMENTAL_GROUP),
            position = position_dodge(width = 0.60))+
  theme_classic()+ 
  theme(axis.title=element_text(size=35),
        axis.text=element_text(size=35)) +
  scale_x_discrete(labels= c("Day 2", "Day 6", "Day 12", "Fledged"))+
  scale_y_continuous(limits=c(0,15), breaks=c(0,3,6,9,12,15))+
  facet_grid(~LOCATION)+
  theme(legend.position = "top",
        legend.title=element_text(size=25),
        legend.text=element_text(size=25),
        strip.text = element_text(size=25)) +
  scale_fill_manual(name = "Treatment group", values = c("#67a9cf", "pink")) +
  scale_color_manual(name = "Treatment group", values = c("#67a9cf", "pink")) +
  labs(x = "Age (days after hatching)", y = "Number of nestlings alive")


####saving the plot############
ggsave(filename = "./plots/predicted_number_offsrping_plot.png",
       plot = g0, 
       device = "png", 
       width = 400, 
       height = 350, 
       units = "mm",  )


###############Part 2: Proportion of nestlings alive model######################
#Creating the response variable
survival<-survival%>%
  mutate(dead_chicks= IN_NEST_CLUTCH_SIZE- Number_chicks_alive)%>%
  mutate(success=(cbind(Number_chicks_alive, dead_chicks)))

####Model 6: Where hatch date is fitted as a quadratic term######
msuccess1 <- glmer(success~ 
                     EXPERIMENTAL_GROUP*LOCATION*Day +
                     scale(NUMBER_HATCHED_EGGS, scale=F)+
                    poly(julian_hatch_date, 2)[,2] +
                    poly(julian_hatch_date, 2)[,1]+(1|NESTBOX),
                   family = binomial(link="logit"),
                   data = survival)
ss <- getME(msuccess1,c("theta","fixef"))
msuccess11<- update(msuccess1,start=ss,control=glmerControl(optimizer="bobyqa",
                                              optCtrl=list(maxfun=2e5)))

####Model 7: Where hatch date is fitted as a linear term######
msuccess <- glmer(success~
                    EXPERIMENTAL_GROUP*LOCATION*Day+
                    scale(NUMBER_HATCHED_EGGS, scale=F)+
                    scale(julian_hatch_date, scale=F)+(1|NESTBOX),
                  family = binomial(link="logit"),
                  data = survival)
ss <- getME(msuccess,c("theta","fixef"))
msuccess2<- update(msuccess,start=ss,control=glmerControl(optimizer="bobyqa",
                                                           optCtrl=list(maxfun=2e5)))
summary(msuccess2)
lrtest.default(msuccess2,msuccess11)#The model where hatch date is fitted linearly is a better fit
confint(msuccess2,parm="beta_",method="Wald",nsim=500)#Extracting confidence intervals for model 2


##########Getting the likelikood ratio values for every term in the model####
###Model 8: model with two way interactions
m4 <- glmer(success~ 
              EXPERIMENTAL_GROUP*LOCATION+ EXPERIMENTAL_GROUP*Day + LOCATION*Day+
              scale(julian_hatch_date, scale = F)+
              scale(NUMBER_HATCHED_EGGS, scale=F)
            +(1|NESTBOX),
            family = binomial(link="logit"),
            data = survival)
summary(m4)
ss <- getME(m4,c("theta","fixef"))
m1<- update(m4,start=ss,control=glmerControl(optimizer="bobyqa",
                                                          optCtrl=list(maxfun=2e5)))
lm1<-lrtest.default(m4,msuccess2)#gives the likelihood ratio for three way interaction between location*day*treatment group

###Model 9: model with no hatch date
m5 <- glmer(success~ 
              EXPERIMENTAL_GROUP*LOCATION*Day+
              scale(NUMBER_HATCHED_EGGS, scale=F) +(1|NESTBOX),
            family = binomial(link="logit"),
            data = survival)
ss <- getME(m5,c("theta","fixef"))
m5<- update(m5,start=ss,control=glmerControl(optimizer="bobyqa",
                                                          optCtrl=list(maxfun=2e5)))
lm2<-lrtest(m5,msuccess2)#gives thelikelihood ratio for hatch date

### Model 10: with no number of hatched eggs
m6 <- glmer(success~ 
              EXPERIMENTAL_GROUP*LOCATION*Day+scale(julian_hatch_date, scale = F)+(1|NESTBOX),
            family = binomial(link="logit"),
            data = survival)
ss <- getME(m6,c("theta","fixef"))
m4<- update(m6,start=ss,control=glmerControl(optimizer="bobyqa",
                                             optCtrl=list(maxfun=2e5)))
lm3<-lrtest(m6,msuccess2)#gives the likelihood ratio for the number of hatched eggs


############plotting the predicted values from the binomial survival model (Model 7) ############
newdat<-expand.grid(
  LOCATION=unique(survival$LOCATION),
  Day=unique(survival$Day),
  EXPERIMENTAL_GROUP=unique(survival$EXPERIMENTAL_GROUP),
#NUMBER_HATCHED_EGGS=mean(as.numeric(survival$NUMBER_HATCHED_EGGS), na.rm = T),
  julian_hatch_date=mean(as.numeric(survival$julian_hatch_date), na.rm = T),
  success=0
)

# mean brood size for control nests in the city
newdat$NUMBER_HATCHED_EGGS[newdat$EXPERIMENTAL_GROUP == "CONTROL" & newdat$LOCATION == "Urban"] <- 
  mean(survival$NUMBER_HATCHED_EGGS[survival$EXPERIMENTAL_GROUP == "CONTROL" & survival$LOCATION == "Urban"])

# mean BROOD size for control nests in the forest
newdat$NUMBER_HATCHED_EGGS[newdat$EXPERIMENTAL_GROUP == "CONTROL" & newdat$LOCATION == "Forest"] <- 
  mean(survival$NUMBER_HATCHED_EGGS[survival$EXPERIMENTAL_GROUP == "CONTROL" & survival$LOCATION == "Forest"])

# mean BROOD size for experimental nests in the city
newdat$NUMBER_HATCHED_EGGS[newdat$EXPERIMENTAL_GROUP == "EXPERIMENTAL" & newdat$LOCATION == "Urban"] <- 
  mean(survival$NUMBER_HATCHED_EGGS[survival$EXPERIMENTAL_GROUP == "EXPERIMENTAL" & survival$LOCATION == "Urban"])

# mean BROOD size for experimental nests in the forest
newdat$NUMBER_HATCHED_EGGS[newdat$EXPERIMENTAL_GROUP == "EXPERIMENTAL" & newdat$LOCATION == "Forest"] <- 
  mean(survival$NUMBER_HATCHED_EGGS[survival$EXPERIMENTAL_GROUP == "EXPERIMENTAL" & survival$LOCATION == "Forest"])

##the new predicted values and associated confidence intervals
newdat$fate<-predict(msuccess2,newdat, type="response", re.form=NA)
newdat$prediction<-predict(msuccess2,newdat, re.form=NA)
mm<-model.matrix(terms(msuccess2),newdat)
pvar1 <- diag(mm %*% tcrossprod(vcov(msuccess2),mm))
tvar1 <- pvar1+VarCorr(msuccess2)$Subject 
cmult <- 1.96
newdat2 <-data.frame(newdat,
                     plo = newdat$prediction-cmult*sqrt(pvar1),
                     phi = newdat$prediction+cmult*sqrt(pvar1))

newdat2$prediction<-plogis(newdat2$prediction)#use plogis to transform predictions to proportions
newdat2$lower<-plogis(newdat2$plo)
newdat2$upper<-plogis(newdat2$phi)

##########Plotting the model predictions##########
GSURVIVAL<-ggplot(data =newdat2,
                  aes(x = as.factor(Day), 
                      y = prediction, 
                      colour = EXPERIMENTAL_GROUP))+
  geom_linerange(data = newdat2, 
                 aes(y = prediction, 
                     ymin = lower, 
                     ymax = upper),
                 position = position_dodge(width = 0.60))+
  geom_point(data = newdat2, 
             aes(x = as.numeric(Day), y = prediction, 
                 fill = EXPERIMENTAL_GROUP),
             position = position_dodge(width = 0.60),
             color = "black", 
             size = 8,
             shape = 21) +
  geom_line(data = newdat2, 
            aes(x = as.numeric(Day), y = prediction, 
                fill = EXPERIMENTAL_GROUP),
            position = position_dodge(width = 0.60))+
  theme_classic()+
  theme(axis.title=element_text(size=25),
        axis.text=element_text(size=25))+
  facet_wrap(~LOCATION)+
  theme(legend.position = "top",
        legend.title=element_text(size=25),
        legend.text=element_text(size=25),
        strip.text = element_text(size=25)) +
  scale_x_discrete(labels= c("Day 2", "Day 6", "Day 12", "Fledged"))+
  scale_y_continuous(limits=c(0,1),breaks=c(0, 0.25, 0.5, 0.75, 1))+
  scale_fill_manual(name = "Treatment group", values = c("#67a9cf", "pink")) +
  scale_color_manual(name = "Treatment group", values = c("#67a9cf", "pink")) +
  labs(x = "Age (days after hatching)", y = "Probability of surviving")

####saving the plot############
ggsave(filename = "./plots/predicted_survival.png",
       plot = GSURVIVAL, 
       device = "png", 
       width = 300, 
       height = 295, 
       units = "mm",  )
