#PREP
#install packages
require(lme4)
require(lmerTest)
require(magrittr)
require(lsmeans)
require(lattice)
require(ggplot2)
require(Hmisc)
require(dplyr)
require(doBy)
require(effects)
require(psy)
library(reshape2)
library(tidyr)
library(psych)
library(reghelper)
library(MuMIn)

#Import the data (data to be used -> Data_RewPavg)
setwd("/Volumes/Seagate Backup Plus Drive/Data Backups - Grad School/Masters Thesis 2017 - Backup/Working Directory/")
Data <- read.csv("Data_LPPavg", sep = "")

#dummy code group (In "Group", 1=Savor, 2=Dampen, 3=Control)
Data$Group_dummy1 <- ifelse(Data$Group == 1, 1, 0)
Data$Group_dummy2 <- ifelse(Data$Group == 2, 1, 0)

#create change variables for PA
Data$ΔPA1 <- (Data$State.Positive.Affect.2 - Data$State.Positive.Affect.1)
Data$ΔPA2 <- (Data$State.Positive.Affect.3 - Data$State.Positive.Affect.2)

#winsorize outlier
Data$AvgAmp[Data$AvgAmp > 27.6] <- 15.6

#create subsetted data for by tasks and types
Data_1 <- subset(Data, Task=="1")
Data_1_Difference <- subset(Data_1, Type=="Difference")
Data_1 <-subset(Data_1, Type!="Difference")
Data_2 <- subset(Data, Task=="2")
Data_2_Difference <- subset(Data_2, Type=="Difference")
Data_2 <-subset(Data_2, Type!="Difference")
Data_Difference <- subset(Data, Type=="Difference")
Data_2_Savor <- subset(Data_2, Group=="1")
Data_2_Dampen <- subset(Data_2, Group=="2")
Data_2_Savor_Wins <- subset(Data_2_Savor, Type=="Win")
Data_2_Dampen_Wins <- subset(Data_2_Dampen, Type=="Win")

#create avg dataset across electrodes - to be used when doing OLS (i.e., when AvgAmp isn't DV)
Data_1_Difference.avg <- summarise(group_by(Data_1_Difference, Subject), AvgAmp = mean(AvgAmp, na.rm=TRUE))
Data_1_Difference.avg <- left_join(Data_1_Difference.avg, Data_1_Difference[Data_1_Difference$Channel=="FCZ", c(1:3, 6:67)], by="Subject")

##create new variable for RewP change across tasks
Data_Change<-dcast(Data, Subject + Type + Channel + Age + Sex + Ethnicity + 
                     Income + Language + Distraction + Fault.Finding + Negative.Mental.Time.Travel + 
                     Suppression + Capitalization + Behavioral.Display + Being.Present + 
                     Positive.Mental.Time.Travel + Dampening.Composite + Savoring.Composite + 
                     Dampening.Diversity +Savoring.Diversity +Expressive.Suppression + 
                     Cognitive.Reappraisal + General.Distress + Anhedonic.Depression + 
                     Anxious.Arousal + Trait.Positive.Affect + Trait.Negative.Affect + 
                     State.Positive.Affect.1 + State.Negative.Affect.1 + State.Positive.Affect.2 + 
                     State.Negative.Affect.2 + State.Positive.Affect.3 + State.Negative.Affect.3 + 
                     Distraction_c + Fault.Finding_c + Negative.Mental.Time.Travel_c + 
                     Suppression_c + Capitalization_c + Behavioral.Display_c + Being.Present_c + 
                     Positive.Mental.Time.Travel_c + Dampening.Composite_c + Savoring.Composite_c + 
                     Dampening.Diversity_c +Savoring.Diversity_c +Expressive.Suppression_c + 
                     Cognitive.Reappraisal_c + General.Distress_c + Anhedonic.Depression_c + 
                     Anxious.Arousal_c + Trait.Positive.Affect_c + Trait.Negative.Affect_c + 
                     State.Positive.Affect.1_c + State.Negative.Affect.1_c + State.Positive.Affect.2_c + 
                     State.Negative.Affect.2_c + State.Positive.Affect.3_c + State.Negative.Affect.3_c + 
                     Group + Savor.Success + Dampen.Success + Group_dummy1 + 
                     Group_dummy2 + ΔPA1 + ΔPA2 ~ Task, value.var="AvgAmp")

Data_Change$ΔAvgAmp <- Data_Change$"2" - Data_Change$"1"
attach(Data_Change)
Data_Change$ΔAvgAmp <- Data_Change$"2" - Data_Change$"1"
detach(Data_Change)

#create win diff subsets
Data_Change_Wins <- subset(Data_Change, Type=="Win")
Data_Change_Difference <- subset(Data_Change, Type=="Difference")

#create .avg for change dataset
Data_Change.avg <- summarise(group_by(Data_Change, Subject), ΔAvgAmp = mean(ΔAvgAmp, na.rm=TRUE))
Data_Change.avg <- left_join(Data_Change.avg, Data_Change[Data_Change$Channel=="FCZ", c(1:3, 6:68)], by="Subject")

Data_Change_Wins.avg <- summarise(group_by(Data_Change_Wins, Subject), ΔAvgAmp = mean(ΔAvgAmp, na.rm=TRUE))
Data_Change_Wins.avg <- left_join(Data_Change_Wins.avg, Data_Change_Wins[Data_Change_Wins$Channel=="FCZ", c(1:3, 6:68)], by="Subject")

Data_Change_Difference.avg <- summarise(group_by(Data_Change_Difference, Subject), ΔAvgAmp = mean(ΔAvgAmp, na.rm=TRUE))
Data_Change_Difference.avg <- left_join(Data_Change_Difference.avg, Data_Change_Difference[Data_Change_Difference$Channel=="FCZ", c(1:3, 6:68)], by="Subject")

#individual differences predicting lpp
  #1. PA Regulation 
  #savoring
    m1 = lmer(AvgAmp ~ Savoring.Composite_c + (1|Subject) + (1|Channel), data = Data_1_Difference, REML = TRUE)
      summary(m1)
      confint(m1)
      #calculate full model
      m1.full = 
        lmer(AvgAmp ~ Savoring.Composite_c + (1|Subject) + (1|Channel), data = Data_1_Difference) %>% 
        r.squaredGLMM()
      #calculate reduced model
      m1.red = lmer(AvgAmp ~ 1 + (1|Subject) + (1|Channel), data = Data_1_Difference) %>% 
        r.squaredGLMM()
      #calculate cohen's f statistic
      effect.size = (m1.full[1] - m1.red[1])/(1-m1.full[1])
  
  #dampening
    m2 = lmer(AvgAmp ~ Dampening.Composite_c + (1|Subject) + (1|Channel), data = Data_1_Difference, REML = TRUE)
      summary(m2)
      confint(m2)
      #calculate full model
      m2.full = 
        lmer(AvgAmp ~ Dampening.Composite_c + (1|Subject) + (1|Channel), data = Data_1_Difference) %>% 
        r.squaredGLMM()
      #calculate reduced model
      m2.red = lmer(AvgAmp ~ 1 + (1|Subject) + (1|Channel), data = Data_1_Difference) %>% 
        r.squaredGLMM()
      #calculate cohen's f statistic
      effect.size = (m2.full[1] - m2.red[1])/(1-m2.full[1])
      
  #2. General Emotion Regulation
  #expressive suppression
   m3 = lmer(AvgAmp ~ Expressive.Suppression_c + (1|Subject) + (1|Channel), data = Data_1_Difference, REML = TRUE)
      summary(m3)
      confint(m3)
      #calculate full model
      m3.full = 
        lmer(AvgAmp ~ Expressive.Suppression_c + (1|Subject) + (1|Channel), data = Data_1_Difference) %>% 
        r.squaredGLMM()
      #calculate reduced model
      m3.red = lmer(AvgAmp ~ 1 + (1|Subject) + (1|Channel), data = Data_1_Difference) %>% 
        r.squaredGLMM()
      #calculate cohen's f statistic
      effect.size = (m3.full[1] - m3.red[1])/(1-m3.full[1])
  
  #cognitive reappraisal
   m4 = lmer(AvgAmp ~ Cognitive.Reappraisal_c + (1|Subject) + (1|Channel), data = Data_1_Difference, REML = TRUE)
      summary(m4)
      confint(m4)
      #calculate full model
      m4.full = 
        lmer(AvgAmp ~ Expressive.Suppression_c + (1|Subject) + (1|Channel), data = Data_1_Difference) %>% 
        r.squaredGLMM()
      #calculate reduced model
      m4.red = lmer(AvgAmp ~ 1 + (1|Subject) + (1|Channel), data = Data_1_Difference) %>% 
        r.squaredGLMM()
      #calculate cohen's f statistic
      effect.size = (m4.full[1] - m4.red[1])/(1-m4.full[1])
  
  #3. Depressive Symptoms   
  #general distress
   m5 = lmer(AvgAmp ~ Anhedonic.Depression + (1|Subject) + (1|Channel), data = Data_1_Difference, REML = TRUE)
      summary(m5)
      confint(m5)
      #calculate full model
      m5.full = 
        lmer(AvgAmp ~ General.Distress_c + (1|Subject) + (1|Channel), data = Data_1_Difference) %>% 
        r.squaredGLMM()
      #calculate reduced model
      m5.red = lmer(AvgAmp ~ 1 + (1|Subject) + (1|Channel), data = Data_1_Difference) %>% 
        r.squaredGLMM()
      #calculate cohen's f statistic
      effect.size = (m5.full[1] - m5.red[1])/(1-m5.full[1])
  
  #anhedonic depression
   m6 = lmer(AvgAmp ~ General.Distress_c + (1|Subject) + (1|Channel), data = Data_1_Difference, REML = TRUE)
      summary(m6)
      confint(m6)
      #calculate full model
      m6.full = 
        lmer(AvgAmp ~ Anhedonic.Depression_c + (1|Subject) + (1|Channel), data = Data_1_Difference) %>% 
        r.squaredGLMM()
      #calculate reduced model
      m6.red = lmer(AvgAmp ~ 1 + (1|Subject) + (1|Channel), data = Data_1_Difference) %>% 
        r.squaredGLMM()
      #calculate cohen's f statistic
      effect.size = (m6.full[1] - m6.red[1])/(1-m6.full[1])

#GROUP DIFFERENCES
#predicting task 2 lpp from success
  #savor
    m17 = lmer(AvgAmp ~ Savor.Success + (1|Subject) + (1|Channel), data = Data_2_Savor_Wins, REML = TRUE)
      summary(m17)
      confint(m17)
      anova(m17)
      #calculate full model
      m17.full = 
        lmer(AvgAmp ~ Savor.Success + (1|Subject) + (1|Channel), data = Data_2_Savor_Wins) %>% 
        r.squaredGLMM()
      #calculate reduced model
      m17.red = lmer(AvgAmp ~ 1 + (1|Subject) + (1|Channel), data = Data_2_Savor_Wins) %>% 
        r.squaredGLMM()
      #calculate cohen's f statistic
      effect.size = (m17.full[1] - m17.red[1])/(1-m17.full[1])
  #dampen
    m18 = lmer(AvgAmp ~  Dampen.Success + (1|Subject) + (1|Channel), data = Data_2_Dampen_Wins, REML = TRUE)
      summary(m18) 
      confint(m18)
      anova(m18)
      #calculate full model
      m18.full = 
        lmer(AvgAmp ~ Dampen.Success + (1|Subject) + (1|Channel), data = Data_2_Dampen_Wins) %>% 
        r.squaredGLMM()
      #calculate reduced model
      m18.red = lmer(AvgAmp ~ 1 + (1|Subject) + (1|Channel), data = Data_2_Dampen_Wins) %>% 
        r.squaredGLMM()
      #calculate cohen's f statistic
      effect.size = (m18.full[1] - m18.red[1])/(1-m18.full[1])
