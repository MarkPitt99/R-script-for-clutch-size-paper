##
## Script to calculate average egg volume--> not accounting for lay order####
##
##
##
##
##### libraries #####
##
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
library(mgcv)
library(lmtest)

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
data$julian_latest<-as.numeric(data$julian_latest)
data$weight_g<-as.numeric(data$weight_g)
data$yolk_weight_g<-as.numeric(data$yolk_weight_g)
data$LATEST_LAYING_ORDER<-as.factor(data$LATEST_LAYING_ORDER)
data$Group<-as.factor(data$Group)


## selecting only the columns that are needed for the analysis#####################
data <- data %>%
  group_by(NESTBOX, EGG_LABEL, EXPERIMENTAL_GROUP) %>% 
  mutate(eggID=cur_group_id())%>%
  dplyr::select(eggID, NESTBOX,EXPERIMENTAL_GROUP, EGG_LABEL, julian_early, julian_latest, LOCATION, LATEST_LAYING_ORDER, 
                length, width, volume, photo_id, no_eggs_photo, weight_g, yolk_weight_g, rep, accurate, Group)%>%
  filter(volume!=0)%>%
  filter(!NESTBOX%in%c(540, 558, 560))
meandata$NESTBOX<-as.factor(meandata$NESTBOX)

########including in nest clutch size in the models#############
allbroods<-read.xlsx("./data/CSM_18.07.2022.xlsx",
                     colNames=T,
                     sheet = 1)
allbroods$NESTBOX<-as.factor(allbroods$NESTBOX)
meandata<-left_join(x=data, y=allbroods%>%
                      select(NESTBOX, IN_NEST_CLUTCH_SIZE, NUMBER_EGGS_LAID),
                    by="NESTBOX")
meandata<-meandata%>%
  filter(!is.na(NUMBER_EGGS_LAID))%>%
  filter(accurate==1)###filters out nests where the females abandoned their investment before clutch completion
meandata$volume<-as.numeric(meandata$volume)


##STATS MODELS#############
######MODEL 1: With lay date fitted as a quadratic term##############
m1<-lmer(volume~
         scale(NUMBER_EGGS_LAID, scale=F)+
         EXPERIMENTAL_GROUP*LOCATION+
         poly(julian_latest, 2)[,2] +
         poly(julian_latest, 2)[,1]+
         (1|NESTBOX)+(1|eggID),
         data=meandata, REML=F)
check_model(m1, panel=F)
summary (m1)
######MODEL 2:with lay date fitted Linearly##############
m2<-lmer(volume~
         scale(NUMBER_EGGS_LAID, scale=F)+
         EXPERIMENTAL_GROUP*LOCATION+
         scale(as.numeric(julian_latest), scale = F)+
         (1|NESTBOX)+(1|eggID),
         data=meandata, REML=F)
check_model(m2, panel=F)
lrtest.default(m1,m2)#gives the lrtest for lay date fitted as a quadratic term
summary(m2)###no effect of location or lay date on egg volume, strong effect of lay date (which declined later in the season)

#Gives the confidence intervals for the model  
FUN <- function(fit) {return(fixef(fit))} 
merBoot<-bootMer(m2, FUN, nsim=500)
CI.lower = apply(merBoot$t, 2, function(x) as.numeric(quantile(x, probs=.025, na.rm=TRUE)))
CI.upper = apply(merBoot$t, 2, function(x) as.numeric(quantile(x, probs=.975, na.rm=TRUE)))


###Model 3: Minimal model-->with no location* treatment group interaction
m3 <- lmer(volume~
           scale(NUMBER_EGGS_LAID, scale=F)+
           EXPERIMENTAL_GROUP+LOCATION+
           scale(as.numeric(julian_latest), scale = F)+
           (1|NESTBOX)+(1|eggID),
         data=meandata, REML=F)
summary(m3)
check_model(m3,panel=F)
lm1<-lrtest.default(m3,m1)#gives likelihood ratio for the interaction effect

###Model 4: model with no lay date included
m4 <- lmer(volume~
          scale(NUMBER_EGGS_LAID, scale=F)+
           EXPERIMENTAL_GROUP*LOCATION+
           (1|NESTBOX)+(1|eggID),
         data=meandata, REML=F)
lm2<-lrtest.default(m4,volmod)#gives the likelihood ratio  value for lay date

###Model 5: model with no number of eggs laid included
m5 <- lmer(volume~
             scale(as.numeric(julian_latest), scale = F)+
             EXPERIMENTAL_GROUP*LOCATION+
             (1|NESTBOX)+(1|eggID),
           data=meandata, REML=F)
lm3<-lrtest.default(m5,volmod)#'gives the likelihood ratio for the number of eggs laid

####Model 6: The minimal model (the one with no location*treatment group interaction)
m6 <- lmer(volume~
             scale(NUMBER_EGGS_LAID, scale=F)+
             EXPERIMENTAL_GROUP+LOCATION+
             scale(as.numeric(julian_latest), scale = F)+
             (1|NESTBOX)+(1|eggID),
           data=meandata, REML=F)
summary (m6)

#Getting the confidence intervals for the minimal model
FUN <- function(fit) {return(fixef(fit))} 
merBoot<-bootMer(m6, FUN, nsim=500)
CI.lower = apply(merBoot$t, 2, function(x) as.numeric(quantile(x, probs=.025, na.rm=TRUE)))
CI.upper = apply(merBoot$t, 2, function(x) as.numeric(quantile(x, probs=.975, na.rm=TRUE)))

##likelihood ratio values
###Model 7:Minimal model <- model with no location included
m7 <- lmer(volume~
             scale(NUMBER_EGGS_LAID, scale=F)+
             EXPERIMENTAL_GROUP+
             scale(as.numeric(julian_latest), scale = F)+
             (1|NESTBOX)+(1|eggID),
           data=meandata, REML=F)
lm4<-lrtest.default(m7,m6)#gives likelihood ratio for location

##Model 8: Minimal model --> with no treatment group included
m8<- lmer(volume~
             scale(NUMBER_EGGS_LAID, scale=F)+
             LOCATION+
             scale(as.numeric(julian_latest), scale = F)+
             (1|NESTBOX)+(1|eggID),
           data=meandata, REML=F)
lm5<-lrtest.default(m8,m6)#gives likelihood ratio for treatment group

###Model 9: Minimal model-->with no lay date
m9 <- lmer(volume~
             scale(NUMBER_EGGS_LAID, scale=F)+
             EXPERIMENTAL_GROUP+LOCATION+
             (1|NESTBOX)+(1|eggID),
           data=meandata, REML=F)
lm6<-lrtest.default(m9,m6)#gives the likelihood ratio  value for lay date

###Model 10: Minimal model-->with no number of eggs laid
m10 <- lmer(volume~
             scale(as.numeric(julian_latest), scale = F)+
             EXPERIMENTAL_GROUP+LOCATION+
             (1|NESTBOX)+(1|eggID),
           data=meandata, REML=F)
lm7<-lrtest.default(m10,m6)#gives the likelihood ratio for the number of eggs laid


#####Plotting the model predictions from Model 6 --> the minimal model#############
newdat3<-expand.grid(
  LOCATION=levels(meandata$LOCATION),
  EXPERIMENTAL_GROUP=levels(meandata$EXPERIMENTAL_GROUP),
  julian_latest=mean(as.numeric(meandata$julian_latest), na.rm = T),
  NESTBOX=levels(meandata$NESTBOX),
  NUMBER_EGGS_LAID=mean(as.numeric(meandata$NUMBER_EGGS_LAID), na.rm = T),
  volume=0
)

#extracting the predicted values from the model
newdat3$volume<-predict(m1, newdat3, re.form=NA)
mm<-model.matrix(terms(m1),newdat3)
pvar1 <- diag(mm %*% tcrossprod(vcov(m1),mm))
tvar1 <- pvar1+VarCorr(m1)$Subject 
cmult <- 1.96
newdat4 <-data.frame(newdat3,
                     plo = newdat3$volume-cmult*sqrt(pvar1),
                     phi = newdat3$volume+cmult*sqrt(pvar1))


#####plotting the predictions with confidence intervals onto a ggplot##########
PREDICTED_PLOT<- ggplot(data = meandata, aes(x = LOCATION, #this is so you can add your raw data points
                                                y = volume, 
                                                fill = EXPERIMENTAL_GROUP)) +
  geom_point(position = position_jitterdodge(dodge.width = 0.60,
                                             jitter.width = 0.18),
             size = 4,
             shape = 21,
             color = "white",
             alpha = 0.5) +
  geom_linerange(data = newdat4, ###this is where you include your dummy dataset prediction points and se
                 aes(y = volume, 
                     ymin = plo, 
                     ymax = phi),
                 position = position_dodge(width = 0.60),
                 width = 0) +
  geom_point(data = newdat4, 
             aes(x = LOCATION, y = volume, 
                 fill = EXPERIMENTAL_GROUP),
             position = position_dodge(width = 0.60),
             color = "black", 
             size = 6,
             shape = 21) +
  scale_x_discrete(labels = c("Urban", "Forest")) +
  scale_y_continuous(limits=c(900,1900), breaks=c(900, 1100,1300,1500,1700,1900))+
  theme_classic() +
  theme(axis.title=element_text(size=25),
        axis.text=element_text(size=25)) +
  theme(legend.position = "top",
        legend.title=element_text(size=20),
        legend.text=element_text(size=20)) +
  scale_fill_manual(name = "Treatment Group", values = c("#67a9cf", "pink")) +
  scale_color_manual(name = "Treatment Group", values = c("#67a9cf", "pink")) +
  labs(x = "Habitat", y = bquote("Egg volume" (mm^3)))

# to save plot
ggsave(filename = "./plots/mean_volume_plot.png",
       plot = PREDICTED_PLOT, 
       device = "png", 
       width = 210, 
       height = 210, 
       units = "mm",  )
