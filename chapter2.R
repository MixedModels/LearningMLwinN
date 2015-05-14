# Heath O'Brien (heath.obrien-at-gmail-dot-com)
# MLwiN tutorial, chapter 2
# 12 May 2015

# last modified 14 May 2015

# This is an attempt to reproduce the output of chapter 2 of the MLwiN tutorial

# clear R of all objects
rm(list=ls())

library(dplyr)
library(ggplot2)
library(lmtest)
library(lme4)
#2.2: Opening  the  worksheet  and  looking  at the data
SchoolData<-read.csv(file = "tutorial.txt", sep="\t")
SchoolData$school<-factor(SchoolData$school)
SchoolData$girl<-factor(SchoolData$girl)
summary(SchoolData)

#2.3: Comparing two groups
group_by(SchoolData, girl) %>% summarise(N=length(normexam), MEANS=mean(normexam), SDs=sd(normexam))
summarise(SchoolData, N=length(normexam), MEANS=mean(normexam), SDs=sd(normexam))

2*pnorm(-7.23,0,1)

MOD.1 <- lm(normexam ~ girl, data=SchoolData)
summary(MOD.1)
# yi = -0.14120(0.02464) + 0.23373(0.03181)x1i 
# σ2e = 0.9926**2 = 0.9852548

#2.4: Comparing more than two groups: Fixed effects models
group_by(SchoolData, school) %>% summarise(MEANS=mean(normexam)) %>%
  ggplot(., (aes(MEANS)))+geom_bar()

MOD.2 <- lm(normexam ~ school, data=SchoolData)
summary(MOD.2)
# F = 12.23 on 64 and 3994 DF,  p-value: < 2.2e-16
#This doesn't look exactly the same because the reference school is 1 
#rather than 65 and I'm not sure how to change this

filter(SchoolData, school==65) %>%summarise(mean(normexam))
# = -0.309375 = 0.500548 + -0.809923

-2 * logLik(MOD.2)
-2 * logLik(lm(normexam ~ 1, data=SchoolData))
1 - pchisq(11510.55-10783.99, df=64)

#or simply
lrtest(MOD.2, lm(normexam ~ 1, data=SchoolData))

MOD.3 <- lm(normexam ~ school + schgend, data=SchoolData)
summary(MOD.3)

#2.5: Comparing means: Random effects or multilevel model
MOD.4 <- lmer(normexam ~ +(1|school), data=SchoolData)
summary(MOD.4)

#I'm a bit sketchy on how to calculate the standard errors for the randomeffects, but here goes:
school_SE <- 0.4143/65**.5
residual_SE <- 0.9209/4059**.5

#Dur, the Std.Dev. is just the square root of the Variance. It has nothing to do with the 
#standard error of the estimate of the variance. I guess I have no idea how to get R to produce
#the numbers in parens. 

#β0j = -0.01408(0.05407) + µ0j
#σ2µ0 = 0.1717(?)
#σ2e = 0.8480(?)
#these numbers aren't exactly the same, but they're in the ballpark

lrtest(MOD.4, lm(normexam ~ 1, data=SchoolData))
#this gives a warning, but the answer is sensible (494.81 vs. 498.71)

#variance partition coefficient:

VPC <-0.1717/(0.1717+0.8480)

#need to change the reference level for schgend
SchoolData$schgend<-relevel(SchoolData$schgend, ref="mixedsch")

MOD.5 <- lmer(normexam ~ schgend + (1|school), data=SchoolData)
summary(MOD.5)
#yij = β0j + 0.06404(0.15308)boyschj + 0.25750(0.11973)girlschj + eij
#β0j = -0.10239(0.07194) + µ0j
#σ2µ0 = 0.1637
#σ2e = 0.8480

