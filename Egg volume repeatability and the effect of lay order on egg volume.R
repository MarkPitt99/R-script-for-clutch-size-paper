##
##
## R Script to calculate the repeatability of the egg volume measurements and the effect of laying order on egg volume####
##
##
##### libraries #####
  library(lubridate)
  library(dplyr)
  library(ggplot2)
  library(wesanderson)
  library(lme4)
  library(nlme)
  library(lmtest)
  library(lmerTest)
  library(performance) 
  library(openxlsx)
  library(tidyr)
  library(rptR)
  library(ggplot2)

## clean environment
rm(list = ls())


##### Loading the data #####
data<- read.xlsx("./data/CSM_18.07.2022.xlsx",
                 colNames=T,
                 sheet = 7)

#changing data types for analysis#############
data$LOCATION<-as.factor(data$LOCATION)
data$NESTBOX<-as.factor(data$NESTBOX)
data$EXPERIMENTAL_GROUP<-as.factor(data$EXPERIMENTAL_GROUP)
data$julian_latest_date<-as.numeric(data$julian_latest)
data$weight_g<-as.numeric(data$weight_g)
data$yolk_weight_g<-as.numeric(data$yolk_weight_g)
data$LATEST_LAYING_ORDER<-as.factor(data$LATEST_LAYING_ORDER)
data$Group<-as.factor(data$Group)

## selecting only those columns that are needed for the analysis#####################
data <- data %>%
  group_by(NESTBOX, EGG_LABEL, EXPERIMENTAL_GROUP) %>% 
  mutate(eggID=cur_group_id())%>%
  dplyr::select(eggID, NESTBOX,EXPERIMENTAL_GROUP, EGG_LABEL, julian_early, julian_latest, LOCATION, LATEST_LAYING_ORDER, 
                length, width, volume, photo_id, no_eggs_photo, weight_g, yolk_weight_g, rep, accurate, Group)%>%
  filter(!is.na(volume))%>%
  filter(!NESTBOX%in%c(540, 558, 560))#removing nests where females abandoned

#changing data types of this new dataset#####
data$NESTBOX<-as.factor(data$NESTBOX)
data$LOCATION<-as.character(data$LOCATION)

########including in nest clutch size in the models#############
allbroods<-read.xlsx("./data/CSM_18.07.2022.xlsx",
                     colNames=T,
                     sheet = 1)
allbroods$NESTBOX<-as.factor(allbroods$NESTBOX)

data1<-left_join(x=data, y=allbroods%>%
                   select(NESTBOX, IN_NEST_CLUTCH_SIZE, NUMBER_EGGS_LAID),
                by="NESTBOX") #performing a left join and creating a single dataset

meandata<-data1%>%
  filter(!is.na(NUMBER_EGGS_LAID))%>%
  filter(LOCATION%in% c("SCENE", "Kelvingrove_park"))

#changing names of locations#
(meandata["LOCATION"][meandata["LOCATION"]=="Kelvingrove_park"]<-"Urban") #renaming sites: Kelvingrove = urban, SCENE = Forest
(meandata["LOCATION"][meandata["LOCATION"]=="SCENE"]<-"Forest")
meandata$LOCATION <- factor(meandata$LOCATION, levels=c("Urban", "Forest"))
meandata$LOCATION<-as.factor(meandata$LOCATION)

#####################Part 1: REPEATABILITY ANALYSIS##############################
## wide to long format (needed for the repeatability analysis)
data <- data %>% 
  pivot_longer(cols = c("volume", 
                        "width",
                        "length"), 
               names_to = "type", 
               values_to = c("measurement")) %>% 
  separate(type, into = c('trait'))%>%
  filter(!is.na(measurement))

##################REPEATABILITY MODEL#################
rep_volume_model <- rpt(measurement ~
                          LOCATION+
                          LATEST_LAYING_ORDER+
                          julian_latest+
                          EXPERIMENTAL_GROUP+
                          (1|NESTBOX)+
                          (1|eggID),
                        grname = c("NESTBOX", "eggID"), 
                        data = data %>% 
                          filter(trait == "volume"),
                        datatype = "Gaussian", 
                        nboot = 1000, 
                        npermut = 0)
summary(rep_volume_model)

###Egg weight and egg volume correlation (Only includes the removed eggs that were weighed)###########
weight<-meandata%>%
  filter(!is.na(volume))%>%
  group_by(volume, EGG_LABEL, EXPERIMENTAL_GROUP)%>%
  distinct(NESTBOX, EGG_LABEL,.keep_all = TRUE)%>%
  filter(volume!=0)%>%
  filter(!NESTBOX%in%c(540, 558, 560))%>%
  filter(!is.na(weight_g))%>%
  filter(accurate==1)

cor.test(weight$weight_g, weight$volume)#correlation test to see if egg weight and egg volume are correlated

################Part 2: EGG VOLUME ANALYSIS#######################
meandata$LATEST_LAYING_ORDER<-as.numeric(meandata$LATEST_LAYING_ORDER)
meandata$volume<-as.numeric(meandata$volume)
meandata$julian_latest<-as.numeric(meandata$julian_latest)
meandata$LATEST_LAYING_ORDER<-as.numeric(meandata$LATEST_LAYING_ORDER)

#MODEL 1: Volume model with lay date fitted as a quadratic term#############
m1<-lmer(volume~scale(NUMBER_EGGS_LAID, scale=F)+
          LATEST_LAYING_ORDER*EXPERIMENTAL_GROUP*LOCATION+
          poly(julian_latest, 2)[,2] +
          poly(julian_latest, 2)[,1]+
          (1|NESTBOX)+(1|eggID),
          data=meandata%>%
          filter(LATEST_LAYING_ORDER<=9)%>% ####filtering (so only analysing the volume of the first nine eggs)#
          filter(accurate==1), REML=F)
summary(m1)
check_model(m1, panel=F)

#MODEL 2: Maximal volume Model with lay date fitted linearly#########
m2<-lmer(volume~scale(NUMBER_EGGS_LAID, scale=F)+
         scale(LATEST_LAYING_ORDER, scale=F)*EXPERIMENTAL_GROUP*LOCATION+
         scale(as.numeric(julian_latest), scale = F)+
         (1|NESTBOX)+(1|eggID),
         data=meandata%>%
         filter(LATEST_LAYING_ORDER<=9)%>%
         filter(accurate==1), REML=F)
summary(m2)
check_model(m2, panel=F)
confint.merMod(m2)#sourcing confidence intervals for the maximal model

###likelihood ratio test for lay date (when fitted as a quadratic term)########## 
lrtest.default(volmod,volmod2)#lrtest for date when fitted as a quadratic


###likelihood ratio tests for every other term in the model#########

###2. Model 3: Volume model with two way interactions (removed three way interaction)#############
m3 <- lmer(volume~
             scale(NUMBER_EGGS_LAID, scale=F)+
             scale(LATEST_LAYING_ORDER, scale=F)*EXPERIMENTAL_GROUP+
             LOCATION*scale(LATEST_LAYING_ORDER, scale=F)+
             LOCATION*EXPERIMENTAL_GROUP+
             scale(as.numeric(julian_latest), scale = F)+
             (1|NESTBOX)+(1|eggID),
           data=meandata%>%
             filter(LATEST_LAYING_ORDER<=9)%>%
             filter(accurate==1), REML=F)
summary(m3)
lm1<-lrtest.default(m3,m2)#gives the likelihood ratio for three way interaction

###3. Model 4: Volume model with the number eggs laid excluded from the model#################
m4 <- lmer(volume~
             scale(LATEST_LAYING_ORDER, scale=F)*EXPERIMENTAL_GROUP*LOCATION+
             scale(as.numeric(julian_latest), scale = F)+
             (1|NESTBOX)+(1|eggID),
           data=meandata%>%
             filter(LATEST_LAYING_ORDER<=9)%>%
             filter(accurate==1), REML=F)
lm2<-lrtest.default(m4,m2) #gives likelihood ratio for number eggs laid

###4. Model 5 Volume model with lay date excluded
m5 <- lmer(volume~
           scale(NUMBER_EGGS_LAID, scale=F)+
           scale(LATEST_LAYING_ORDER, scale=F)*EXPERIMENTAL_GROUP*LOCATION+
           (1|NESTBOX)+(1|eggID),
           data=meandata%>%
           filter(LATEST_LAYING_ORDER<=9)%>%
           filter(accurate==1), REML=F)
lm3<-lrtest.default(m5,m2) #gives likelihood ratio for lay date


######################plotting the model predictions (from model m2)#############
#plotting volume only up to egg 9 (due to sample size issues beyond ten eggs)
#creating a new dataset for the predicted values###
newdat3<-expand.grid(
  LATEST_LAYING_ORDER=c(1:9),
  LOCATION=levels(meandata$LOCATION),
  EXPERIMENTAL_GROUP=levels(meandata$EXPERIMENTAL_GROUP),
  julian_latest=mean(as.numeric(meandata$julian_latest), na.rm = T),
  NESTBOX=levels(meandata$NESTBOX),
  NUMBER_EGGS_LAID=mean(as.numeric(meandata$NUMBER_EGGS_LAID), na.rm = T),
  volume=0
)

#extracting the predicted values from the model
newdat3$volume<-predict(m2, newdat3, re.form=NA)
mm<-model.matrix(terms(m2),newdat3)
pvar1 <- diag(mm %*% tcrossprod(vcov(m2),mm))
tvar1 <- pvar1+VarCorr(m2)$Subject 
cmult <- 1.96
newdat4 <-data.frame(newdat3,
                     plo = newdat3$volume-cmult*sqrt(pvar1),
                     phi = newdat3$volume+cmult*sqrt(pvar1))

###########getting the means to put on the plot (to show that linear model is not a perfect fit)
df_summary_volume <- meandata%>%
  filter(LATEST_LAYING_ORDER<=9)%>%
  filter(accurate==1)%>%
  filter(!is.na(volume))%>% 
  group_by(LOCATION, LATEST_LAYING_ORDER, EXPERIMENTAL_GROUP) %>% 
  summarise(mean = mean(volume, na.rm = T),
            se= sd(volume)/sqrt(n()))


###plotting the predicted values onto a ggplot######################
g2 <- ggplot(data = meandata%>%
               filter(LATEST_LAYING_ORDER<=9), 
             aes(x = LATEST_LAYING_ORDER, 
                 y = volume, 
                 colour = EXPERIMENTAL_GROUP)) +
  geom_point(position = position_jitter(width = 0.15),
             alpha = 0.25)+
  geom_linerange(data = df_summary_volume, 
                 aes(y = mean, 
                     ymin = mean-se, 
                     ymax = mean+se),
                 position = position_dodge(width = 0.60),
                 width = 0,
                 alpha=0.9)+
  geom_point(data = df_summary_volume, 
             aes(x = LATEST_LAYING_ORDER, y = mean, 
                 fill = EXPERIMENTAL_GROUP),
             position = position_dodge(width = 0.60),
             color = "black", 
             size = 7,
             shape = 21,
             alpha=0.9) +
  geom_line(data = newdat4, 
            aes(x = as.numeric(LATEST_LAYING_ORDER), y = volume, 
                fill = EXPERIMENTAL_GROUP),
            position = position_dodge(width = 0.9),
            alpha=1, size=1.5)+
  geom_ribbon(data = newdat4, aes(y=NULL, ymin = plo, ymax = phi, 
                                  color=NULL, fill = EXPERIMENTAL_GROUP),
              alpha = .25)+
  theme_classic()+ 
  theme(axis.title=element_text(size=25),
        axis.text=element_text(size=25))+
  facet_wrap(~LOCATION)+
  theme(legend.position = "top",
        legend.title=element_text(size=20),
        legend.text=element_text(size=20),
        strip.text = element_text(size=25)) +
  scale_x_continuous(breaks=c(1,2,3,4,5,6,7,8,9))+
  scale_y_continuous(limits=c(900,1900), breaks=c(900, 1100,1300,1500,1700,1900))+
  scale_fill_manual(name = "Treatment group", values = c("#67a9cf", "pink")) +
  scale_color_manual(name = "Treatment group", values = c("#67a9cf", "pink")) +
  labs(x = "Position in laying sequence", y = bquote("Egg volume" (mm^3)))

###############Saving the Plot########################
ggsave(filename = "./plots/mean_volume_eggs1-9_plot.png",
       plot = g2, 
       device = "png", 
       width = 210, 
       height = 210, 
       units = "mm",  )




####Egg volume analysis part 2: Grouping eggs into two groups (Group 1:eggs 1-3 or Group 2: eggs 4-9)#######
meandata$LATEST_LAYING_ORDER<-as.numeric(meandata$LATEST_LAYING_ORDER)
meandata$julian_latest<-as.numeric(meandata$julian_latest)
meandata$volume<-as.numeric(meandata$volume)
#Filtering eggs down into the two groups
meandata$NUMBER_EGGS_LAID<-as.numeric(meandata$NUMBER_EGGS_LAID)
  meandata<-meandata%>%
    filter(!Group==0)%>%
    filter(LATEST_LAYING_ORDER<=9)%>%
    filter(accurate==1)
  
###############statistical models for egg volume groups########################
#Model 1: Where date is fitted as a quadratic
mgrouped1_quadratic<-lmer(volume~
                 Group*LOCATION*EXPERIMENTAL_GROUP+
                 scale(NUMBER_EGGS_LAID, scale=F)+
                 poly(julian_latest, 2)[,2] +
                 poly(julian_latest, 2)[,1]+
                 (1|NESTBOX)+(1|eggID),
               data=meandata, REML=F)
  summary(mgrouped1_quadratic)
  check_model(mgrouped1_quadratic, panel = F)  
  
#Model 2: Where date is fitted linearly
mgrouped1<-lmer(volume~
                  Group*LOCATION*EXPERIMENTAL_GROUP+
                  scale(NUMBER_EGGS_LAID, scale=F)+
                  scale(as.numeric(julian_latest), scale = F)+
                  (1|NESTBOX)+(1|eggID),
                data=meandata, REML=F)
lrtest.default(mgrouped1,mgrouped1_quadratic)#lrtest for quadratic term
summary(mgrouped1)
check_model(mgrouped1, panel = F)

###########Getting confidence intervals for the model coefficients
FUN <- function(fit) {return(fixef(fit))} 
merBoot<-bootMer(mgrouped1, FUN, nsim=500)
CI.lower = apply(merBoot$t, 2, function(x) as.numeric(quantile(x, probs=.025, na.rm=TRUE)))
CI.upper = apply(merBoot$t, 2, function(x) as.numeric(quantile(x, probs=.975, na.rm=TRUE)))

######Simplifying the model and getting the lrtest values######
###2. model with two way interactions
mgrouped2 <- lmer(volume~
             Group*LOCATION+EXPERIMENTAL_GROUP*Group+LOCATION*EXPERIMENTAL_GROUP+
             scale(NUMBER_EGGS_LAID, scale=F)+
             scale(as.numeric(julian_latest), scale = F)+
             (1|NESTBOX)+(1|eggID),
           data=meandata%>%
             filter(LATEST_LAYING_ORDER<=9)%>%
             filter(accurate==1), REML=F)
summary(mgrouped2)
lm1<-lrtest(mgrouped2,mgrouped1)#gives likelihood ratio for three way interaction

###2. model with no number eggs laid
mgrouped3 <- lmer(volume~
             Group*LOCATION*EXPERIMENTAL_GROUP+
             scale(as.numeric(julian_latest), scale = F)+
             (1|NESTBOX)+(1|eggID),
           data=meandata%>%
             filter(LATEST_LAYING_ORDER<=9)%>%
             filter(accurate==1), REML=F)
lm2<-lrtest(mgrouped3,mgrouped1)#gives likelihood ratio for number eggs laid

###3. model with no lay date
mgrouped4 <- lmer(volume~
             Group*LOCATION*EXPERIMENTAL_GROUP+
             scale(NUMBER_EGGS_LAID, scale=F)+
             (1|NESTBOX)+(1|eggID),
           data=meandata%>%
             filter(LATEST_LAYING_ORDER<=9)%>%
             filter(accurate==1), REML=F)
lm3<-lrtest(mgrouped4,mgrouped1)#gives likelihood ratio for lay date

#model without experimental group*lay order interaction
m1.1 <- lmer(volume~
             Group*LOCATION+LOCATION*EXPERIMENTAL_GROUP+
             scale(NUMBER_EGGS_LAID, scale=F)+
             scale(as.numeric(julian_latest), scale = F)+
             (1|NESTBOX)+(1|eggID),
           data=meandata%>%
             filter(LATEST_LAYING_ORDER<=9)%>%
             filter(accurate==1), REML=F)
lrtest1<-lrtest.default(m1.1,mgrouped2)#gives the lrtest for the experimental group*lay order interaction

#model without experimetal group*location interaction
m1.2 <- lmer(volume~
               Group*LOCATION+EXPERIMENTAL_GROUP*Group+
               scale(NUMBER_EGGS_LAID, scale=F)+
               scale(as.numeric(julian_latest), scale = F)+
               (1|NESTBOX)+(1|eggID),
             data=meandata%>%
               filter(LATEST_LAYING_ORDER<=9)%>%
               filter(accurate==1), REML=F)
lrtest2<-lrtest.default(m1.2,mgrouped2)#for the experimental group*location--> remove this interaction from the model as it's the only one that's not significant

#model without location*lay order interaction
m1.3 <- lmer(volume~
               Group*EXPERIMENTAL_GROUP+LOCATION*EXPERIMENTAL_GROUP+
               scale(NUMBER_EGGS_LAID, scale=F)+
               scale(as.numeric(julian_latest), scale = F)+
               (1|NESTBOX)+(1|eggID),
             data=meandata%>%
               filter(LATEST_LAYING_ORDER<=9)%>%
               filter(accurate==1), REML=F)
lrtest3<-lrtest.default(m1.3,mgrouped2)


###m12 is the minimal model --> excluded the experimental group*location interaction###
minmod <- lmer(volume~
             Group*LOCATION+EXPERIMENTAL_GROUP+EXPERIMENTAL_GROUP*Group+
             scale(NUMBER_EGGS_LAID, scale=F)+
             scale(as.numeric(julian_latest), scale = F)+
             (1|NESTBOX)+(1|eggID),
           data=meandata%>%
             filter(LATEST_LAYING_ORDER<=9)%>%
             filter(accurate==1), REML=F)
summary(minmod)
check_model(minmod, panel = F)

#getting the confidence intervals for minmod
FUN <- function(fit) {return(fixef(fit))} 
merBoot<-bootMer(m12, FUN, nsim=500)
CI.lower = apply(merBoot$t, 2, function(x) as.numeric(quantile(x, probs=.025, na.rm=TRUE)))
CI.upper = apply(merBoot$t, 2, function(x) as.numeric(quantile(x, probs=.975, na.rm=TRUE)))

####getting the likelihood values for the other fixed effects in this minimal model
#model with no habitat lay order group interaction
minmod.1 <- lmer(volume~
              EXPERIMENTAL_GROUP*Group+LOCATION+
              scale(NUMBER_EGGS_LAID, scale=F)+
              scale(as.numeric(julian_latest), scale = F)+
              (1|NESTBOX)+(1|eggID),
            data=meandata%>%
              filter(LATEST_LAYING_ORDER<=9)%>%
              filter(accurate==1), REML=F)
lrtest.default(minmod.1,minmod)#for habitat*lay order group

#with no treatment and lay order group interaction
minmod.2 <- lmer(volume~
               Group*LOCATION+EXPERIMENTAL_GROUP+
               scale(NUMBER_EGGS_LAID, scale=F)+
               scale(as.numeric(julian_latest), scale = F)+
               (1|NESTBOX)+(1|eggID),
             data=meandata%>%
               filter(LATEST_LAYING_ORDER<=9)%>%
               filter(accurate==1), REML=F)
lrtest.default(minmod.2,minmod)#for treatment*lay order group

#model with no number eggs laid
minmod.3 <- lmer(volume~
              Group*LOCATION+EXPERIMENTAL_GROUP*Group+
              scale(as.numeric(julian_latest), scale = F)+
              (1|NESTBOX)+(1|eggID),
            data=meandata%>%
              filter(LATEST_LAYING_ORDER<=9)%>%
              filter(accurate==1), REML=F)
lrtest.default(minmod.3,minmod)#for number eggs laid

#MODEL WITH NO LAY DATE
minmod.4 <- lmer(volume~
              Group*LOCATION+EXPERIMENTAL_GROUP*Group+
              scale(NUMBER_EGGS_LAID, scale=F)+
              (1|NESTBOX)+(1|eggID),
            data=meandata%>%
              filter(LATEST_LAYING_ORDER<=9)%>%
              filter(accurate==1), REML=F)
lrtest.default(minmod.4,minmod)#for lay date


##########plotting the grouped model##################
newdat5<-expand.grid(
  LATEST_LAYING_ORDER=mean(as.numeric(meandata$LATEST_LAYING_ORDER), na.rm = T),
  LOCATION=levels(meandata$LOCATION),
  EXPERIMENTAL_GROUP=levels(meandata$EXPERIMENTAL_GROUP),
  julian_latest=mean(as.numeric(meandata$julian_latest), na.rm = T),
  NESTBOX=levels(meandata$NESTBOX),
  Group= levels(meandata$Group),
  NUMBER_EGGS_LAID=mean(as.numeric(meandata$NUMBER_EGGS_LAID), na.rm = T),
  mean_volume=0
)

#extracting the predicted values from the model
newdat5$mean_volume<-predict(m12, newdat5, re.form=NA)
mm<-model.matrix(terms(m12),newdat5)
pvar1 <- diag(mm %*% tcrossprod(vcov(m12),mm))
tvar1 <- pvar1+VarCorr(m12)$Subject  ## must be adapted for more complex models
cmult <- 1
newdat5 <-data.frame(newdat5,
                     plo = newdat5$mean_volume-cmult*sqrt(pvar1),
                     phi = newdat5$mean_volume+cmult*sqrt(pvar1))


###plotting the predicted values onto a ggplot (note there's no confidence intervals at the moment on the predicted values)######################
g3 <- ggplot(data = meandata%>%
               filter(LATEST_LAYING_ORDER<=9)%>%
               filter(accurate==1), 
             aes(x = Group, 
                 y = mean_volume, 
                 colour = EXPERIMENTAL_GROUP)) +
  geom_point(position = position_jitter(width = 0.15),
             alpha = 0.3)+
  geom_linerange(data = newdat5, 
                 aes(y = mean_volume, 
                     ymin = plo, 
                     ymax = phi),
                 position = position_dodge(width = 0.60),
                 width = 0)+
  geom_point(data = newdat5, 
             aes(x = Group, y = mean_volume, 
                 fill = EXPERIMENTAL_GROUP),
             position = position_dodge(width = 0.60),
             color = "black", 
             size = 4,
             shape = 21) +
  geom_line(data = newdat5, 
            aes(x = as.numeric(Group), y = mean_volume, 
                fill = EXPERIMENTAL_GROUP),
            position = position_dodge(width = 0.60))+
  theme_classic()+
  theme(axis.title=element_text(size=15),
        axis.text=element_text(size=12))+
  facet_grid(~LOCATION)+
  theme(legend.position = "top",
        legend.title=element_text(size=12),
        legend.text=element_text(size=12)) +
  scale_x_discrete(labels=c("Eggs 1-3", "Eggs 4-9"))+
  scale_fill_manual(name = "Treatment group", values = c("#67a9cf", "pink")) +
  scale_color_manual(name = "Treatment group", values = c("#67a9cf", "pink")) +
  labs(x = "Laying order group", y = bquote("Egg volume" (mm^3)))

#Saving the plot
ggsave(filename = "./plots/mean_group_eggs_2.png",
       plot = g3, 
       device = "png", 
       width = 150, 
       height = 120, 
       units = "mm",  )
