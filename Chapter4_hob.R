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
library(broom)

#4.1 Random intercept models

SchoolData<-read.csv(file = "tutorial.txt", sep="\t")
SchoolData$school<-factor(SchoolData$school)
SchoolData$girl<-factor(SchoolData$girl)
summary(SchoolData)


# Let's start by plotting the response, normexam, against standlrt:
ggplot(SchoolData, aes(standlrt, normexam))+geom_point()

#test effect of standlrt on normexam (fixed effects model)
MOD.1 <- lm(normexam ~ standlrt, data=SchoolData)
summary(MOD.1)
-2*logLik(MOD.1)
#normexam = -0.002(0.013) + 0.595(0.013)*standlrt +ej
#σ2e = 0.8056**2 = 0.649(???)
#-2*logLIk = 9762.037

#test effect of schools (as random effect) on normexam after taking account standlrt
MOD.2 <- lmer(normexam ~ standlrt + (1|school), data = SchoolData)
summary(MOD.2)
-2*logLik(MOD.2)
#normexam = boj + 0.563(0.012) * standlrt + ej
#boj = 0.015(0.040) + uoj
#σ2µo = 0.094
#σ2e = 0.566
#-2*logLIk = 9370.233

#lmer reports residual variance while lm reports residual SE (tho it looks more like SD?)

MOD.3 <- lmer(normexam ~ standlrt + (1|school), data = SchoolData, REML=F)
summary(MOD.3)
-2*logLik(MOD.3)
#normexam = boj + 0.563(0.012) * standlrt + ej
#boj = 0.015(0.040) + uoj
#σ2µo = 0.092
#σ2e = 0.566
#-2*logLIk = 9370.233

#results are identical with REML and lmerMod except for variance of school intercept,
#which is closer to MLwiN resuls with the latter method.
9762.037 - 9370.233
lrtest(MOD.1, MOD.3)


#4.2    Graphing  predicted  school  lines  from  a random intercept model
augment(MOD.3) %>%
  distinct(standlrt) %>%
    ggplot(., aes(standlrt, .fixed))+geom_line()

augment(MOD.3) %>%
  ggplot(., aes(standlrt, .fitted, group=school))+geom_line()

#plot points + overall line and 95% CI for intercept (can also use predict to plot get SE)
augment(MOD.3) %>%
  distinct(standlrt) %>%
  ggplot(., aes(standlrt, .fixed)) + 
  geom_point(aes(standlrt, normexam), data=SchoolData) + 
  geom_ribbon(aes(ymin = .fixed - 1.96 * 0.3036/sqrt(65), ymax=.fixed + 1.96 * 0.3036/sqrt(65)), alpha=.3) +
  geom_line()

#plot points + individual lines, with SD for intercept:
distinct <- augment(MOD.3) %>% distinct(standlrt)

augment(MOD.3) %>%
  ggplot(., aes(standlrt, .fitted)) +
  geom_ribbon(aes(ymin = .fixed - 0.3036, ymax=.fixed + 0.3036), alpha=.5, data=distinct) +
  geom_point(aes(standlrt, normexam), data=SchoolData) +
  geom_line(aes(group =school))

#4.3 The effect of clustering on the standard errors of coefficients
SchoolData$schgend<-relevel(SchoolData$schgend, ref="mixedsch")
MOD.4 <- lmer(normexam ~ standlrt + schgend + (1|school), data=SchoolData)
summary(MOD.4)
-2*logLik(MOD.4)
#normexam = boj + 0.564(0.012) * standlrt + 0.097(0.112) * schgendboysch + 0.245(0.087) * schgendgirlsch + eij
#boj = -0.088(0.052) + uoj
#σ2µo = 0.085
#σ2e = 0.566
#-2*logLIk = 9368.243

#single-level model
MOD.5 <- lm(normexam ~ standlrt + schgend, data=SchoolData)
summary(MOD.5)
0.7985**2
-2*logLik(MOD.5)
#normexam = -0.097 + 0.594(0.013) * standlrt + 0.118(0.013) * schgendboysch + 0.236(0.028) * schgendgirlsch + eij
#σ2e = 0.638
#-2*logLIk = 9688.658

#It's not trivial to get the CI for the lines conditional on gender.
#In principle, it should be possible to calculate them manually from the model summary, 
#but I'd need to factor in the uncertianty in the slope in addition to the uncertainty in the intercept
#this method from visreg gets close:
library(visreg)
visreg(MOD.4, "schgend", type="contrast")

#4.4 Does the coefficient of standlrt vary across schools? Introducing a random slop
MOD.6 <- lmer(normexam ~ standlrt + (standlrt|school), data=SchoolData)
summary(MOD.6)
-2*logLik(MOD.6)
#normexam = boj + b1j * standlrt + eij
#boj = -0.013(0.040) + uoj
#b1j = 0.557(0.020) + u1j

#σ2µo = 0.092
#σ2µ1 = 0.015
#σµo1 = Corr*sqrt(σ2µo*σ2µ1) = 0.50*sqrt(0.092*0.015) = 0.018
#σ2e = 0.554
#-2*logLIk = 9329.045

lrtest(MOD.3, MOD.6)

#4.5  Graphing  predicted  school  lines  from  a random slope model
augment(MOD.6) %>%
  ggplot(., aes(standlrt, .fitted)) +
  geom_point(aes(standlrt, normexam), data=SchoolData) +
  geom_line(aes(group =school))

