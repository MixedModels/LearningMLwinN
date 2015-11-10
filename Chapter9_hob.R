library(readr)
library(aod) #wald.test
library(lme4) #glmer
library(car) #linearHypothesis
library(MASS) #glmmPQL
library(lmtest) #lrtest
library(dplyr)

#import and format dataset
ContraceptiveUse <- read_csv("https://raw.githubusercontent.com/MixedModels/LearningMLwinN/master/bang.csv")
ContraceptiveUse$age <- round(ContraceptiveUse$age)
ContraceptiveUse$district<-factor(ContraceptiveUse$district)
ContraceptiveUse$use<-factor(ContraceptiveUse$use)
ContraceptiveUse$use4<-factor(ContraceptiveUse$use4)
ContraceptiveUse$lc<-factor(ContraceptiveUse$lc)
ContraceptiveUse$urban<-factor(ContraceptiveUse$urban)
ContraceptiveUse$educ<-factor(ContraceptiveUse$educ)
ContraceptiveUse$hindu<-factor(ContraceptiveUse$hindu)
ContraceptiveUse$cons<-factor(ContraceptiveUse$cons)

#Tabulate
table(ContraceptiveUse$lc, ContraceptiveUse$use)
prop.table(table(ContraceptiveUse$lc, ContraceptiveUse$use), 1)

#9.2 Single-level logistic regression
MOD.1 <-glm(use ~ lc, data=ContraceptiveUse, family = "binomial")
summary(MOD.1)

exp(1.093)/exp(.933) #odds ratio for 2 kids vs 1
1/(1+exp(-(-1.123+0.933))) #probability for a woman with 1 kid

#test for significant difference betwqeen 1 vs 2 children
wald.test(b = coef(MOD.1), Sigma = vcov(MOD.1), L = cbind(0,1,-1,0))
linearHypothesis(MOD.1, "lc2 = lc1")

MOD.2 <-glm(use ~ lc, data=ContraceptiveUse, family = "binomial"(link='probit'))
summary(MOD.2)

MOD.3 <-glm(use ~ lc + age, data=ContraceptiveUse, family = "binomial")
summary(MOD.3)

#9.3 A two-level random intercept mode

MOD.4 <- glmer(use ~ lc + age + (1|district), data=ContraceptiveUse, family = "binomial")
summary(MOD.4)
#These numbers are almost identical to the 2nd Order PQL results

#VPC = var/var+3.29)
0.3076/(0.3076+3.29)
lrtest(MOD.4, MOD.3)


MOD.5 <- glmmPQL(use ~ lc + age, ~1|district, data=ContraceptiveUse, family = "binomial")
summary(MOD.5)
#This PQL method from MASS gives slightly different results (I'm guessing they're 1st order or something?)

#Adding further explanatory variables

MOD.6 <- glmer(use ~ lc + age + urban + educ + hindu + (1|district), data=ContraceptiveUse, family = "binomial")
summary(MOD.6)

#9.4    A two-level random coeffcient model
MOD.7 <- glmer(use ~ lc + age + urban + educ + hindu + (urban|district), data=ContraceptiveUse, family = "binomial")
summary(MOD.7)

MOD.8 <- glmer(use ~ lc + age + urban + educ + hindu + d_lit + d_pray + (urban|district), data=ContraceptiveUse, family = "binomial")
summary(MOD.8)
#This one fails to converge, but the numbers are very similar to the MLWin results
#I think this is being treated as an individual level effect tho.

#9.5 Modelling binomial data
BinomContraceptiveUse <-ContraceptiveUse %>% 
  group_by(district) %>%
  summarise(d_use = sum(as.numeric(levels(use)[use]))/n(), n=n(), d_lit = mean(d_lit), d_pray = mean(d_pray))

MOD.9 <- glmer(d_use ~ d_lit + d_pray + (1|district), data=BinomContraceptiveUse, family = "binomial", weights=n)
summary(MOD.9)

#These numbers are in the ball-park, but def. not the same