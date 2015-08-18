# Heath O'Brien (heath.obrien-at-gmail-dot-com)
# MLwiN tutorial, chapter 6
# 13 July 2015

# last modified 13 July 2015

# This is an attempt to reproduce the output of chapter 6 of the MLwiN tutorial

# clear R of all objects
rm(list=ls())

library(lme4) #lmer
library(lmtest)

library(dplyr) #arrange
library(ggplot2) #ggplot
library(broom) #augment
library(arm) #se.coef
library(grid)


SchoolData<-read.csv(file = "BTSync/Courses/LearningMLwinN/tutorial.txt", sep="\t")
SchoolData$school<-factor(SchoolData$school)
SchoolData$girl<-factor(SchoolData$girl)
SchoolData$schgend<-relevel(SchoolData$schgend, ref="mixedsch")
SchoolData$schav<-relevel(SchoolData$schav, ref="low")
summary(SchoolData)

#Fit basic model from chapter 4
MOD.1 <- lmer(normexam ~ standlrt + (standlrt|school), data = SchoolData, REML=F)
summary(MOD.1)

#6.1 The impact of school gender on girls' achievement

MOD.2 <- lmer(normexam ~ standlrt + schgend + girl + (standlrt|school), data = SchoolData, REML=F)
summary(MOD.2)
# normexam = boj + b1j * standlrt +  0.16856(0.03383)girlij +
# 0.17997(0.09912)boyschij + 0.17479(0.07875)girlschij + eij
#boj = -0.18987(0.05136) + uoj
#b1j = 0.55451(0.01994) + u1j

#σ2µo = 0.07959
#σ2µ1 = 0.01466
#σµo1 = Corr*sqrt(σ2µo*σ2µ1) = 0.59*sqrt(0.07959*0.01466) = 0.0201534
#σ2e = 0.55039
#-2*logLIk = 9282.4

# Likelihood is a bit higher. Otherwise everything is identical within roundoff error

lrtest(MOD.1, MOD.2) #highely significant

MOD.3 <- lmer(normexam ~ standlrt + schgend + girl + standlrt:schgend + (standlrt|school), data = SchoolData, REML=F)
summary(MOD.3)

lrtest(MOD.2, MOD.3) #not significant

MOD.4 <- lmer(normexam ~ standlrt + schgend + girl + standlrt:girl + (standlrt|school), data = SchoolData, REML=F)
summary(MOD.4)

lrtest(MOD.2, MOD.4) #not significant

# 6.2 Contextual effects of school intake ability averages
MOD.5 <- lmer(normexam ~ standlrt + schgend + girl + schav + (standlrt|school), data = SchoolData, REML=F)
summary(MOD.5)
# normexam = boj + b1j * standlrt +  0.16743(0.03383)*girlij +
# 0.18709(0.09766)*boyschij + 0.15702(0.07772)*girlschij + 
# -0.06684(0.08524)*midij + -0.17407(0.09865)*highij + eij
#boj = -0.09167(0.07714) + uoj
#b1j = 0.55164(0.02006) + u1j

#σ2µo = 0.07081
#σ2µ1 = 0.01471
#σµo1 = Corr*sqrt(σ2µo*σ2µ1) = 0.50*sqrt(0.07081*0.01471) = 0.01613703
#σ2e = 0.55036
#-2*logLIk = 9279.8

lrtest(MOD.2, MOD.5) #not significant

MOD.6 <- lmer(normexam ~ standlrt + schgend + girl + schav + standlrt:schav + (standlrt|school), data = SchoolData, REML=F)
summary(MOD.6)

lrtest(MOD.2, MOD.6) #significant

#code to reproduce the plot on page 88 of the manual (adding confidence lines will be non-trivial)
ggplot(SchoolData)+geom_abline(intercept = 0.29083, slope=0.18070) +
  scale_y_continuous(limits=c(-.2, .8), breaks=seq(-.2, .8, .2)) + 
  scale_x_continuous(limits=c(-2.4, 3.2), breaks=seq(-24, 32, 8)/10) +
  geom_hline(yintercept = 0, linetype=2) +
  geom_vline(xintercept = 0, linetype=2) +
  ylab("hilowdiff") +
  xlab("standlrt") +
  theme(plot.margin = unit(c(1,1,1.1,1), "cm"))

grid.text("© Heath O'Brien 2015", x=unit(.99, "npc"), y=unit(.01, "npc"), just=c("right", "bottom"))

