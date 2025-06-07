#############################################################################
####                                                                     ####
####                  ~~~~~~\o/~~~~~/\~~~~~                              ####
####                                                                     ####
####                   "NO MAN IS AN ISLAND"                             ####
####  Effects of social seclusion on REM sleep and social dream content  ####
####                                                                     ####
####          Tuominen, Olkoniemi, Revonsuo, & Valli (2021)              ####
####                  British Journal of Psychology                      ####
####                                                                     ####
#############################################################################

# 2021-6-7
# Henri Olkoniemi

# Needed libraries
library(lme4)
library(interactions)
library(effsize)
library(ggplot2)
library(sjPlot)

# Opening workdir
setwd("/")

############################################################
############################################################
####                                                    ####
####                                                    ####
####                        REM %                       ####
####                                                    ####
####                                                    ####
############################################################
############################################################

## Opening Zeo data
R<-read.table("sei_Zeo2.txt", header=T, na.strings = "NA") 

## Removing arrival and leaving nights from the data
RE<-R[with(R, Place != "S" & Place != "Sr"), ] 

# Setting 'Place' as factor and removing unnecessary levels
# Note that 'Place' is referred as "location" in the manuscript
RE$Place<-factor(RE$Place) 

# Removing outliers
REM<-RE[RE$REM > 0,] # Excluding 1 observations with 0min of REM sleep 
REM_GNS<-REM[REM$sleep_efficiency > 60,] # Excluding 1 observations with only 48 min of sleep and 6 min of REM sleep

# MEAN & SD FOR AMOUNT OF REM SLEEP (min) BETWEEN BASELINE AND ISOLATION
aggregate(REM~Place, data=REM_GNS, FUN=mean) 
aggregate(REM~Place, data=REM_GNS, FUN=sd)

# MEAN & SD FOR AMOUNT OF SLEEP (i.e. sleep efficiency; min) BETWEEN BASELINE AND ISOLATION
aggregate(sleep_efficiency~Place, data=REM_GNS, FUN=mean)
aggregate(sleep_efficiency~Place, data=REM_GNS, FUN=sd)

# MEAN & SD FOR REM% BETWEEN BASELINE AND ISOLATION
aggregate(REM_sleepEf~Place, data=REM_GNS, FUN=mean)
aggregate(REM_sleepEf~Place, data=REM_GNS, FUN=sd)

# Is the REM_SleepEf normally distributed
qqnorm(REM_GNS$REM_sleepEf) # Ok

#Treatment contrasts to Place 
contrasts(REM_GNS$Place) = contr.treatment(2) 

## Model 1: The amount of REM sleep
m_REM_total1<-lmer(REM_sleepEf~Place+(Place|ID), 
                   control=lmerControl(optCtrl=list(maxfun=40000)),
                   data=REM_GNS)

m_REM_total2<-lmer(REM_sleepEf~Place+(0+Place|ID), 
                   control=lmerControl(optCtrl=list(maxfun=40000)),
                   data=REM_GNS)

m_REM_total<-lmer(REM_sleepEf~Place+(1|ID), data=REM_GNS)

# model summaries
summary(m_REM_total)
confint(m_REM_total, method="Wald") # 95% CI FOR MODEL VALUES

# plot(s)
REM_perc <- cat_plot(m_REM_total, pred=Place, data=REM_GNS,
                x.label = "Location",
                y.label = "REM %")

REM_perc + theme_bw() + theme(panel.border = element_blank(), 
                              panel.grid.major = element_blank(),
                              panel.grid.minor = element_blank(), 
                              axis.line = element_line(colour = "black"),
                              axis.text.x = element_text(size = 20, colour = "black"),
                              axis.text.y = element_text(size = 20, colour = "black"),
                              axis.title.x = element_text(size = 20, colour = "black"),
                              axis.title.y = element_text(size = 20, colour = "black"))
# Plot(s)
ef_REM <- effects::effect("Place", m_REM_total)
summary(ef_REM)
x_REM<-as.data.frame(ef_REM)

Fig_REM<-ggplot(x_REM, aes(x=Place, y=fit)) +
  geom_line(size=2) +
  geom_point(size=5) +
  geom_errorbar(aes(ymin=lower, ymax=upper), width=0.5, size = 2) + 
  theme_bw(base_size=26) + 
  labs(x = "",
       y = "Amount of REM Sleep (%)") +
  scale_x_discrete(labels=c("B" = "Post-Seclusion", "E" = "Seclusion"), 
                   limits=c("E","B"))+
  scale_y_continuous(limits = c(0.25, 0.35), breaks=c(0.25, 0.3, 0.35)) +
  theme(panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line.y = element_line(colour = "black"),
        axis.line.y = element_line(colour = "black", hjust=0) 
Fig_REM

ggsave("figure3.pdf", width = 8, height = 8, units = "in", dpi=300)

############################################################
############################################################
####                                                    ####
####                    [(-_-)]zzz                      ####
####                                                    ####
####                DREAM INTERACTIONS                  ####
####                                                    ####
############################################################
############################################################

## Opening tables
INT<-read.table("seili_int_chars_u_cond.txt", header=T, na.strings = "NA") # Opening dream content data
teht<-read.table("tehtavat.txt", header=T, na.strings = "NA") # Opening task score data
teh<-teht[teht$alku==1,] # including only task scores from the baseline condition
INT2<-merge(INT,teh, by="ID") # merging data frames
##

## removing observations from the data in which the social isolation during retreat failed
INT2$excl<-ifelse(INT2$sos_int_prev_day == 1 & INT2$cond_broad == "I", 1,
                  ifelse(INT2$cond_broad == "E", 1, 
                         ifelse(INT2$cond_broad == "P", 1, 0)))
sum(INT2$excl) #just checking
IN<-INT2[INT2$excl == 1, ] 
##

## calculating sociality bias ratio -variable

# Converting the amount of dream interactions to same scale
# as the amount of wake time interactions was reported
IN$int_cat<-ifelse(IN$Interactions == 0, 1,
                     ifelse(IN$Interactions >= 1 & IN$Interactions <= 5, 2,
                            ifelse(IN$Interactions >= 6 & IN$Interactions <= 15, 3,
                                   ifelse(IN$Interactions >= 16 & IN$Interactions <= 25, 4,5))))

IN$dream_per_sos<-IN$int_cat/IN$sos_int_prev_day
hist(IN$dream_per_sos)
qqnorm(IN$dream_per_sos) #skewed
hist(log(IN$dream_per_sos))
qqnorm(log(IN$dream_per_sos)) #looks more normally distributed after log-transformed

################################################################################
##############        Figure on dream and wake interactions       ##############
################################################################################

IN$ID <- as.factor(IN$ID)
IN_long <- tidyr::gather(IN, condition, measurement, sos_int_prev_day, 
                         int_cat, factor_key=TRUE)
IN_long$measurement2<-IN_long$measurement-1
IN_long_m<-aggregate(measurement2~condition+cond_broad, data=IN_long, FUN=mean)
names(IN_long_m)[names(IN_long_m) == "measurement2"] <- "mean"
IN_long_sd<-aggregate(measurement2~condition+cond_broad, data=IN_long, FUN=sd)
names(IN_long_sd)[names(IN_long_sd) == "measurement2"] <- "sd"
IN_long_msd<-merge(IN_long_m, IN_long_sd, by=c('condition','cond_broad'))

Fig_Int<- ggplot(data=IN_long_msd, aes(x=cond_broad, 
                                       y=mean, 
                                       group=condition, 
                                       fill=condition)) + 
  geom_col(color="black", size=1,position = position_dodge(width=0.95)) +
  geom_errorbar(aes(ymin=mean-sd, ymax=mean+sd), 
                width=0.5, size = 1, position = position_dodge(width=0.95)) + 
  scale_x_discrete(labels=c("E" = "Pre-Seclusion", 
                            "I" = "Seclusion", 
                            "P"="Post-Seclusion"))+
  theme_bw(base_size=26) +
  labs(x = "Location",
       y = "Number of Social Interactions",
       fill="Interactions") +
  scale_fill_manual(labels = c("Wake", "Dream"), 
                    values = c("light grey", "dark grey")) +
  scale_y_continuous(limits = c(0, 4), breaks=c(0,1,2,3,4), 
                     labels = c("0", "1-5", "6-15", "16-25", ">25"))+
  theme(panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.ticks.x = element_blank(),
        axis.line = element_line(colour = "black", hjust=0))

Fig_Int

ggsave("figureINT.pdf", width = 16, height = 8, units = "in", dpi=300)
################################################################################

## Cronbachs alphas for the tasks
scores<-read.table("scoreps.txt", header=T, na.strings = "NA") # table containing necessary values
score<-scores[scores$alku.loppu==1,] #including only task scores from the baseline condition
psych::alpha(score[3:11]) # PHQ-9 alpha = 0.73, 95% CI [0.55, 0.90]
psych::alpha(score[12:21]) # NTB alpha = 0.86, 95% CI [0.77, 0.96]
psych::alpha(score[22:29], check.keys = T) # RFQ sum, alpha = 0.77, 95% CI [0.61, 0.93]
psych::alpha(score[32:37]) # RFQ-C alpha = 0.68, 95% CI [0.45, 0.91]
##

## Task descriptives & centering
range(teh$PSQ) #PSQ = PHQ
mean(teh$PSQ)
sd(teh$PSQ)

range(teh$RFQ_c)
mean(teh$RFQ_c)
sd(teh$RFQ_c)

range(teh$NtoB) #NtoB = NTB
mean(teh$NtoB)
sd(teh$NtoB)
##

# correlations between the predictors
cor.test(INT2$PSQ, INT2$RFQ_c) # r = -.43
cor.test(INT2$RFQ_c,INT2$NtoB) # r = -.21
cor.test(INT2$PSQ, INT2$NtoB) # r = .42     

# Centering the variables
IN$cPSQ<-scale(IN$PSQ, center=T)
IN$c_RFQ<-scale(IN$RFQ_c, center=T)
IN$cNtoB<-scale(IN$NtoB, center=T)
##

## Means and SD's // interaction measures per condition
aggregate(Interactions~cond_broad, data=IN, FUN=range)
aggregate(Interactions~cond_broad, data=IN, FUN=mean)
aggregate(Interactions~cond_broad, data=IN, FUN=sd)

aggregate(dream_per_sos~cond_broad, data=IN, FUN=range)
aggregate(dream_per_sos~cond_broad, data = IN, FUN=mean)
aggregate(dream_per_sos~cond_broad, data = IN, FUN=sd)
##

## Setting contrasts for cond_broad (location)
IN$cond_broad<-factor(IN$cond_broad) #cond_broad as factor (cond_broad is called location in the manuscript)
contrasts(IN$cond_broad) = MASS::contr.sdif(3) 
##

## Model 2: The amount of dream interactions
hist(IN$Interactions) #How Interactions is distributed

m_int<-glmer(Interactions~cond_broad+c_RFQ+cPSQ+cNtoB+(cond_broad|ID), data=IN, family="poisson")

# model summaries
summary(m_int)
confint(m_int, method="Wald") #95% CI FOR MODEL VALUES

# plot
ef_int <- effects::effect("cond_broad", m_int)
summary(ef_int)
x_int<-as.data.frame(ef_int)

Fig1<-ggplot(x_int, aes(x=cond_broad, y=fit)) +
  geom_line(size=2) +
  geom_point(size=5) +
  geom_errorbar(aes(ymin=lower, ymax=upper), width=0.5, size = 2) + 
  theme_bw(base_size=26) + 
  labs(x = "",
       y = "Number of Interctions in Dream") +
  scale_x_discrete(labels=c("E" = "Pre-Seclusion", "I" = "Seclusion", "P"="Post-Seclusion"))+
  scale_y_continuous(limits = c(0, 6)) +
  theme(panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black")) 
Fig1

## Model 3: The ratio between dream and wake social interactions 

m_DpS1<-lmer(log(dream_per_sos)~cond_broad+c_RFQ+cPSQ+cNtoB+(cond_broad|ID), 
            data=IN, control=lmerControl(optCtrl=list(maxfun=40000)))

m_DpS2<-lmer(log(dream_per_sos)~cond_broad+c_RFQ+cPSQ+cNtoB+(cond_broad||ID),
            control=lmerControl(optCtrl=list(maxfun=40000)),
            data=IN)

m_DpS<-lmer(log(dream_per_sos)~cond_broad+c_RFQ+cPSQ+cNtoB+(1|ID),
            control=lmerControl(optCtrl=list(maxfun=40000)),
            data=IN)

# model summaries
summary(m_DpS)
confint(m_DpS, method="Wald") #95% CI FOR MODEL VALUES

# plot(s)
L <- cat_plot(m_DpS, pred=cond_broad, data=IN, x.label = "Location")
L + theme_bw() + theme(panel.border = element_blank(), 
                              panel.grid.major = element_blank(),
                              panel.grid.minor = element_blank(), 
                              axis.line = element_line(colour = "black"),
                              axis.text.x = element_text(size = 20, colour = "black"),
                              axis.text.y = element_text(size = 20, colour = "black"),
                              axis.title.x = element_text(size = 20, colour = "black"),
                              axis.title.y = element_text(size = 20, colour = "black"))

plot_model(m_DpS, type = "pred", terms = c("c_RFQ"))

# model summaries
summary(m_Str3)
confint(m_Str3, method="Wald") #95% CI FOR MODEL VALUES

# plot(s)
ef_dps <- effects::effect("cond_broad", m_DpS)
summary(ef_dps)
x_dps<-as.data.frame(ef_dps)

x_dps$fit_b<-exp(x_dps$fit)
x_dps$lower_b<-exp(x_dps$lower)
x_dps$upper_b<-exp(x_dps$upper)

Fig1B<-ggplot(x_dps, aes(x=cond_broad, y=fit_b)) +
  geom_line(size=2) +
  geom_point(size=5) +
  geom_errorbar(aes(ymin=lower_b, ymax=upper_b), width=0.5, size = 2) + 
  theme_bw(base_size=26) + 
  labs(x = "",
       y = "Sociality Bias") +
  scale_x_discrete(labels=c("E" = "Pre-Seclusion", "I" = "Seclusion", "P"="Post-Seclusion"))+
  scale_y_continuous(limits = c(0, 2.5)) +
  theme(panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black")) 
Fig1B

## Model 4: The amount of strangers in dream interactions
ukno<-read.table("un_known.txt", header=T, na.strings = "NA") 
names(ukno)[names(ukno) == "uni"] <- "Dream"
IN2<-merge(IN, ukno, by="Dream")

aggregate(tuntemattomat ~cond_broad, data=IN2, FUN=mean)
aggregate(tuntemattomat ~cond_broad, data=IN2, FUN=sd)

m_Str1<-glmer(tuntemattomat~cond_broad+c_RFQ+cPSQ+cNtoB+(cond_broad|ID), 
                control=lmerControl(optCtrl=list(maxfun=40000)),
                data=IN2, family="binomial")

m_Str2<-glmer(tuntemattomat~cond_broad+c_RFQ+cPSQ+cNtoB+(cond_broad||ID), 
                control=glmerControl(optCtrl=list(maxfun=40000)),
                data=IN2, family="binomial")

m_Str3<-glmer(tuntemattomat~cond_broad+c_RFQ+cPSQ+cNtoB+(1|ID), data=IN2, family="binomial")

# model summaries
summary(m_Str3)
confint(m_Str3, method="Wald") #95% CI FOR MODEL VALUES

# plot(s)
ef_str <- effects::effect("cond_broad", m_Str3)
summary(ef_str)
x_str<-as.data.frame(ef_str)

x_str$fit_b<-exp(x_str$fit)
x_str$lower_b<-exp(x_str$lower)
x_str$upper_b<-exp(x_str$upper)

Fig2<-ggplot(x_str, aes(x=cond_broad, y=fit_b)) +
  geom_line(size=2) +
  geom_point(size=5) +
  geom_errorbar(aes(ymin=lower_b, ymax=upper_b), width=0.5, size = 2) + 
  theme_bw(base_size=26) + 
  labs(x = "",
       y = "Likelihood of Strangers (OR)") +
  scale_x_discrete(labels=c("E" = "Pre-Seclusion", "I" = "Seclusion", "P"="Post-Seclusion"))+
  scale_y_continuous(limits = c(1.3, 2)) +
  theme(panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black")) 
Fig2 

## Model 5: The amount of known individuals in dream interactions
aggregate(tutut ~cond_broad, data=IN2, FUN=mean)
aggregate(tutut ~cond_broad, data=IN2, FUN=sd)

m_kno1<-glmer(tutut~cond_broad+c_RFQ+cPSQ+cNtoB+(cond_broad|ID), 
                 control=glmerControl(optCtrl=list(maxfun=40000)),
                 data=IN2, family="binomial")

# model summaries
summary(m_kno1)
confint(m_kno1, method="Wald") # 95% CI FOR MODEL VALUES

# Plot(s)
ef_kno <- effects::effect("cond_broad", m_kno1)
summary(ef_kno)
x_kno<-as.data.frame(ef_kno)

x_kno$fit_b<-exp(x_kno$fit)
x_kno$lower_b<-exp(x_kno$lower)
x_kno$upper_b<-exp(x_kno$upper)

Fig3<-ggplot(x_kno, aes(x=cond_broad, y=fit_b)) +
  geom_line(size=2) +
  geom_point(size=5) +
  geom_errorbar(aes(ymin=lower_b, ymax=upper_b), width=0.5, size = 2) + 
  theme_bw(base_size=26) + 
  labs(x = "",
       y = "Likelihood of Known Characters (OR)") +
  scale_x_discrete(labels=c("E" = "Pre-Seclusion", "I" = "Seclusion", "P"="Post-Seclusion"))+
  scale_y_continuous(limits = c(1.3, 2)) +
  theme(panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black")) 
Fig3 

figure1 <- ggpubr::ggarrange(Fig1, Fig1B,
                            labels = c("A", "B"),
                            font.label = list(size = 20, 
                                              color = "black", 
                                              face = "bold"),
                            ncol = 2, nrow = 1)
figure1
ggsave("figure1.pdf", width = 20, height = 8, units = "in", dpi=300)

figure2 <- ggpubr::ggarrange(Fig2, Fig3, Fig4, Fig5,
                            labels = c("A", "B", "C", "D"),
                            font.label = list(size = 20, 
                                              color = "black", 
                                              face = "bold"),
                            ncol = 2, nrow = 2)
figure2
ggsave("figure2.pdf", width = 20, height = 20, units = "in", dpi=300)


###########################################################
###########################################################
####                                                   ####
####                                                   ####
####                                                   ####
####           DREAM NEGATIVITY/POSITIVITY             ####
####                                                   ####
###########################################################
###########################################################

# NEGATIVE INTERACTIONS
length(IN$NEG_int==0)
hist(IN$NEG_int)
aggregate(NEG_int~cond_broad, data=IN, FUN = mean)
aggregate(NEG_int~cond_broad, data=IN, FUN = sd)

# Model 6: the amount of negative interactions in dreams 
m_neg1<-glmer(NEG_int~cond_broad+c_RFQ+cPSQ+NtoB+(1+cond_broad|ID), 
              control=lmerControl(optCtrl=list(maxfun=40000)),
              data=IN, family="poisson")
m_neg2<-glmer(NEG_int~cond_broad+c_RFQ+cPSQ+NtoB+(0+cond_broad|ID), 
              control=lmerControl(optCtrl=list(maxfun=40000)),
              data=IN, family="poisson")

m_neg<-glmer(NEG_int~cond_broad+c_RFQ+cPSQ+NtoB+(1|ID), data=IN, family="poisson")

# model summaries
summary(m_neg)
confint(m_neg, method="Wald")

plot_model(m_neg, type = "pred", terms = c("c_RFQ"),
                       axis.title = c("RFQ-C", "Negative Interactions"),
                       title = "")
# Plot(s)
ef_neg <- effects::effect("cond_broad", m_neg)
summary(ef_neg)
x_neg<-as.data.frame(ef_neg)

Fig4<-ggplot(x_neg, aes(x=cond_broad, y=fit)) +
  geom_line(size=2) +
  geom_point(size=5) +
  geom_errorbar(aes(ymin=lower, ymax=upper), width=0.5, size = 2) + 
  theme_bw(base_size=26) + 
  labs(x = "",
       y = "Negative Interactions / Dream") +
  scale_x_discrete(labels=c("E" = "Pre-Seclusion", "I" = "Seclusion", "P"="Post-Seclusion"))+
  scale_y_continuous(limits = c(0, 1.5)) +
  theme(panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black")) 
Fig4 

# POSITIVE INTERACTIONS
hist(IN$POS_int)
aggregate(POS_int~cond_broad, data=IN, FUN = mean)
aggregate(POS_int~cond_broad, data=IN, FUN = sd)

## # Model 7: the amount of positive interactions in dreams did not show any effects
m_pos1<-glmer(POS_int~cond_broad+c_RFQ+cPSQ+NtoB+(cond_broad|ID), 
              control=lmerControl(optCtrl=list(maxfun=40000)),
              data=IN, family="poisson")
m_pos2<-glmer(POS_int~cond_broad+c_RFQ+cPSQ+NtoB+(0+cond_broad|ID), 
              control=lmerControl(optCtrl=list(maxfun=40000)),
              data=IN, family="poisson")

m_pos<-glmer(POS_int~cond_broad+c_RFQ+cPSQ+cNtoB+(1|ID), data=IN, family="poisson")

# model summaries
summary(m_pos)
confint(m_pos, method="Wald") #95% CI FOR MODEL VALUES

# Plot(s)
ef_pos <- effects::effect("cond_broad", m_pos)
summary(ef_pos)
x_pos<-as.data.frame(ef_pos)

Fig5<-ggplot(x_pos, aes(x=cond_broad, y=fit)) +
  geom_line(size=2) +
  geom_point(size=5) +
  geom_errorbar(aes(ymin=lower, ymax=upper), width=0.5, size = 2) + 
  theme_bw(base_size=26) + 
  labs(x = "",
       y = "Positive Interactions / Dream") +
  scale_x_discrete(labels=c("E" = "Pre-Seclusion", "I" = "Seclusion", "P"="Post-Seclusion"))+
  scale_y_continuous(limits = c(0, 1.5)) +
  theme(panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black")) 
Fig5 
