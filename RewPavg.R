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
Data <- read.csv("Data_RewPavg", sep = "")

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

#paired-samples t-test to test if delta.RewP changes by task
Data_wide <- spread(Data_Difference, Task, AvgAmp)
Data_wide <- rename(Data_wide, "AvgAmp1" = "1")
Data_wide <- rename(Data_wide, "AvgAmp2" = "2")

t.test(Data_wide$AvgAmp1,
       Data_wide$AvgAmp2,
       paired=TRUE,
       conf.level=0.95)

#overall, people had larger response to wins than losses. 
m.type = lmer(AvgAmp ~ Type + (Type|Subject) + (1|Subject:Channel), data = Data, REML = TRUE)
summary(m.type)

#individual differences predicting delta RewP
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

#interaction between trait emotion regulation and avgamp in predicting depressive symptoms
  #1. General Distress
    m7 = lm(General.Distress ~ AvgAmp*Savoring.Composite, data = Data_1_Difference.avg)
      summary(m7)
      anova(m7)
      confint(m7)
      simple_slopes(m7)

    m8 = lm(General.Distress ~ AvgAmp*Dampening.Composite, data = Data_1_Difference.avg)
      summary(m8)
      anova(m8)
      confint(m8)
      anova(m8)

    m9 = lm(General.Distress ~ AvgAmp*Expressive.Suppression, data = Data_1_Difference.avg)
      summary(m9)
      anova(m9)
      confint(m9)
      simple_slopes(m9)

    m10 = lm(General.Distress ~ AvgAmp*Cognitive.Reappraisal, data = Data_1_Difference.avg)
      summary(m10)
      anova(m10)
      confint(m10)
      simple_slopes(m10)


  #2. Anhedonic Depression
    m11 = lm(Anhedonic.Depression ~ AvgAmp*Savoring.Composite, data = Data_1_Difference.avg)
      summary(m11)
      anova(m11)
      confint(m11)

    m12 = lm(Anhedonic.Depression ~ AvgAmp*Dampening.Composite, data = Data_1_Difference.avg)
      summary(m12)
      anova(m12)
      confint(m12)

    m13 = lm(Anhedonic.Depression ~ AvgAmp*Expressive.Suppression, data = Data_1_Difference.avg)
      summary(m13)
      confint(m13)
      anova(m13)
      simple_slopes(m13)

    m14 = lm(Anhedonic.Depression ~ AvgAmp*Cognitive.Reappraisal, data = Data_1_Difference.avg)
      summary(m14)
      confint(m14)
      anova(m14)
      simple_slopes(m14)

#GROUP DIFFERENCES
   #predicting delta rewp from group after covarying for delta rewp task 1   

    m = lmer(AvgAmp2 ~ Group*AvgAmp1 + (1|Subject) + (1|Channel), data = Data_wide, REML=TRUE)
      summary(m)
      confint(m)
      anova(m)
      #calculate full model
      m.full = 
        lmer(AvgAmp2 ~ Group*AvgAmp1 + (1|Subject) + (1|Channel), data = Data_wide) %>% 
        r.squaredGLMM()
      #calculate reduced model 1
      m.red1 = lmer(AvgAmp2 ~ Group+AvgAmp1 + (1|Subject) + (1|Channel), data = Data_wide) %>% 
        r.squaredGLMM()
      #calculate reduced model 2
      m.red2 = lmer(AvgAmp2 ~ Group + (1|Subject) + (1|Channel), data = Data_wide) %>% 
        r.squaredGLMM()
      #calculate cohen's f statistic of INTERACTION
      effect.size = (m.full[1] - m.red1[1])/(1-m.full[1]) 
      #calculate cohen's f statistic of INTERACTION + GROUP
      effect.size = (m.full[1] - m.red2[1])/(1-m.full[1]) 
    
   #same thing but now looking at differences between each of the three groups
      #subset data to run models
      Data_wide_SD <- subset(Data_wide, Group!="3")
      Data_wide_CD <- subset(Data_wide, Group!="1")
      Data_wide_SC <- subset(Data_wide, Group!="2")

      #savor vs. dampen
      Data_wide_SD$Group <- as.factor(Data_wide_SD$Group)
      mSD = lmer(AvgAmp2 ~ Group*AvgAmp1 + (1|Subject) + (1|Channel), data = Data_wide_SD, REML=TRUE)
        summary(mSD)
        plot(allEffects(mSD))
        confint(mSD)
        #calculate full model
        mSD.full = 
          lmer(AvgAmp2 ~ Group*AvgAmp1 + (1|Subject) + (1|Channel), data = Data_wide_SD) %>% 
          r.squaredGLMM()
        #calculate reduced model 1
        mSD.red1 = lmer(AvgAmp2 ~ Group+AvgAmp1 + (1|Subject) + (1|Channel), data = Data_wide_SD) %>% 
          r.squaredGLMM()
        #calculate reduced model 2
        mSD.red2 = lmer(AvgAmp2 ~ Group + (1|Subject) + (1|Channel), data = Data_wide_SD) %>% 
          r.squaredGLMM()
        #calculate cohen's f statistic of INTERACTION
        effect.size = (mSD.full[1] - mSD.red1[1])/(1-mSD.full[1]) 
        #calculate cohen's f statistic of INTERACTION + GROUP
        effect.size = (mSD.full[1] - mSD.red2[1])/(1-mSD.full[1]) 
      
      #control vs. dampen
      Data_wide_CD$Group <- as.factor(Data_wide_CD$Group)
      mCD = lmer(AvgAmp2 ~ Group*AvgAmp1 + (1|Subject) + (1|Channel), data = Data_wide_CD, REML=TRUE)
        summary(mCD)
        anova(mCD)
        confint(mCD)
        #calculate full model
        mCD.full = 
          lmer(AvgAmp2 ~ Group*AvgAmp1 + (1|Subject) + (1|Channel), data = Data_wide_CD) %>% 
          r.squaredGLMM()
        #calculate reduced model 1
        mCD.red1 = lmer(AvgAmp2 ~ Group+AvgAmp1 + (1|Subject) + (1|Channel), data = Data_wide_CD) %>% 
          r.squaredGLMM()
        #calculate reduced model 2
        mCD.red2 = lmer(AvgAmp2 ~ Group + (1|Subject) + (1|Channel), data = Data_wide_CD) %>% 
          r.squaredGLMM()
        #calculate cohen's f statistic of INTERACTION
        effect.size = (mCD.full[1] - mCD.red1[1])/(1-mCD.full[1]) 
        #calculate cohen's f statistic of INTERACTION + GROUP
        effect.size = (mCD.full[1] - mCD.red2[1])/(1-mCD.full[1]) 
      
      #savor vs. control
      Data_wide_SC$Group <- as.factor(Data_wide_SC$Group)
      mSC = lmer(AvgAmp2 ~ Group*AvgAmp1 + (1|Subject) + (1|Channel), data = Data_wide_SC, REML=TRUE)
        summary(mSC)
        plot(allEffects(mSC))
        confint(mSC)
        #savor is significantly different than dampen and than control
        #calculate full model
        mSC.full = 
          lmer(AvgAmp2 ~ Group*AvgAmp1 + (1|Subject) + (1|Channel), data = Data_wide_SC) %>% 
          r.squaredGLMM()
        #calculate reduced model 1
        mSC.red1 = lmer(AvgAmp2 ~ Group+AvgAmp1 + (1|Subject) + (1|Channel), data = Data_wide_SC) %>% 
          r.squaredGLMM()
        #calculate reduced model 2
        mSC.red2 = lmer(AvgAmp2 ~ Group + (1|Subject) + (1|Channel), data = Data_wide_SC) %>% 
          r.squaredGLMM()
        #calculate cohen's f statistic of INTERACTION
        effect.size = (mSC.full[1] - mSC.red1[1])/(1-mSC.full[1]) 
        #calculate cohen's f statistic of INTERACTION + GROUP
        effect.size = (mSC.full[1] - mSC.red2[1])/(1-mSC.full[1])
      
   #predicting change in rewp across tasks from success   
      #savor
        m15 = lmer(ΔAvgAmp ~ Savor.Success + (1|Subject) + (1|Channel), data = Data_Change_Wins, REML = TRUE)
          summary(m15)
          anova(m15)
          confint(m15)
          #calculate full model
          m15.full = 
            lmer(ΔAvgAmp ~ Savor.Success + (1|Subject) + (1|Channel), data = Data_Change_Wins) %>% 
            r.squaredGLMM()
          #calculate reduced model
          m15.red = lmer(ΔAvgAmp ~ 1 + (1|Subject) + (1|Channel), data = Data_Change_Wins) %>% 
            r.squaredGLMM()
          #calculate cohen's f statistic
          effect.size = (m15.full[1] - m15.red[1])/(1-m15.full[1])
      
        #dampen  
        m16 = lmer(ΔAvgAmp ~ Dampen.Success + (1|Subject) + (1|Channel), data = Data_Change_Wins, REML = TRUE)
          summary(m16)
          confint(m16)
          #calculate full model
          m16.full = 
            lmer(ΔAvgAmp ~ Dampen.Success + (1|Subject) + (1|Channel), data = Data_Change_Wins) %>% 
            r.squaredGLMM()
          #calculate reduced model
          m16.red = lmer(ΔAvgAmp ~ 1 + (1|Subject) + (1|Channel), data = Data_Change_Wins) %>% 
            r.squaredGLMM()
          #calculate cohen's f statistic
          effect.size = (m16.full[1] - m16.red[1])/(1-m16.full[1])
        
   #predicting task 2 rewp from success
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
   
  #predicting depressive symptoms from change in rewp across task and success       
      #General Distress
        m19 = lm(General.Distress ~ ΔAvgAmp*Savor.Success, data = Data_Change_Wins.avg)
          summary(m19)
          confint(m19)
          anova(m19)
          
      #Anhedonia
        m24 = lm(Anhedonic.Depression ~ ΔAvgAmp*Savor.Success, data = Data_Change_Wins.avg)
          summary(m24)
          confint(m24)
          anova(m24)

#State PA 
  #winsorize outliers
    Data_1_Difference.avg$ΔPA1[Data_1_Difference.avg$ΔPA1 < -23.53325] <- -21.1
    Data_1_Difference.avg$ΔPA2[Data_1_Difference.avg$ΔPA2 > 18.30549] <- 11.1
  
  #after accounting for state pa change across first doors task, does group predict state pa change across doors task 2
    m1 = lm(ΔPA2 ~ ΔPA1*Group, data=Data_1_Difference.avg)
      summary(m1)
  
  #subset group pairs
    Data_1_Difference.avg_SD <- subset(Data_1_Difference.avg, Group!="3")
    Data_1_Difference.avg_CD <- subset(Data_1_Difference.avg, Group!="1")
    Data_1_Difference.avg_SC <- subset(Data_1_Difference.avg, Group!="2")
  
  #are savor and dampen different from each other
    m1a = lm(ΔPA2 ~ ΔPA1*Group, data=Data_1_Difference.avg_SD)
      summary(m1a)
      confint(m1a)
      anova(m1a)
  
  #are control and dampen different from each other
    m1b = lm(ΔPA2 ~ ΔPA1*Group, data=Data_1_Difference.avg_CD)
      summary(m1b)
      confint(m1b)
      anova(m1b)
  
  #are savor and control different from each other
     m1c = lm(ΔPA2 ~ ΔPA1*Group, data=Data_1_Difference.avg_SC)
      summary(m1c)
     confint
     anova(m1c)
  
  #now looking at after accounting for state pa change across first doors task, does success predict state pa change across doors task 2
  
  #subset groups
    Data_1_Difference.avg_Savor <- subset(Data_1_Difference.avg, Group=="1")
    Data_1_Difference.avg_Dampen <- subset(Data_1_Difference.avg, Group=="2")
    Data_1_Difference.avg_Control <- subset(Data_1_Difference.avg, Group=="3")
  
  #savor group
    m1d = lm(ΔPA2 ~ ΔPA1*Savor.Success, data=Data_1_Difference.avg_Savor)
      summary(m1d)
      confint(m1d)
      anova(m1d)
  
  #dampen group
    m1e = lm(ΔPA2 ~ ΔPA1*Dampen.Success, data=Data_1_Difference.avg_Dampen)
      summary(m1e)
      confint(m1e)
      anova(m1e)