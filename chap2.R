rm(list=ls()) # clear memory
tutorial.data<-read.table(file.choose(),header=T) # select data file
View(tutorial.data) # check spreadsheet
attach(tutorial.data) # attach data file

# DESCRIPTIVE STATS AND Z-TEST 
library(plyr) # for splitting data by groups
ddply(tutorial.data,"girl",summarise,N=length(normexam),mean=mean(normexam),sd=sd(normexam)) # descriptive stats by gender
data.frame(N=length(normexam),mean(normexam),sd(normexam)) # descriptive stats for whole sample
2*pnorm(7.23,lower.tail=FALSE) # two-tailed probability of z-score > |7.23|

# FIXED EFFECTS MODELS
summary(model1<-lm(normexam~factor(girl))) # fixed-effects model on exam scores w.r.t. gender
mean.score<-aggregate(normexam,list(school),mean) # save mean scores by school
hist(mean.score[,2],breaks=20,col="grey")
summary(model2<-lm(normexam~factor(school))) # fixed-effects model on exam scores w.r.t. school
summary(model3<-lm(normexam~relevel(factor(school),"65"))) # changing reference category to School65
anova(lm(normexam~factor(school))) # ANOVA table on exam scores w.r.t. school
pf(12.23,64,3994,lower.tail=FALSE) # probability of F-score >12.233
-2*logLik(model3) # log-likelihood for fixed-effects model on exam scores w.r.t. school
-2*logLik(lm(normexam~1)) # log-likelihood for model on exam scores with intercept only
pchisq(11510.55-10783.99,64,lower.tail=FALSE) # LR test to compare models
summary(model4<-update(model3,~.+schgend)) # attempt to add effect of school gender (confounded)

# RANDOM EFFECTS MODELS
library(lme4) # calls lme4 package for fitting mixed models
summary(model5<-lmer(normexam~(1|school),REML=FALSE)) # variance components model with random intercept for school
-2*logLik(model5) # log-likelihood for this model
pchisq(11510.55-2*5505.9,1,lower.tail=FALSE) # LR test to compare with single-level (intercept-only) model
summary(model6<-lmer(normexam~schgend+(1|school),REML=FALSE)) # mixed model including school-level gender variable
summary(model7<-lmer(normexam~relevel(schgend,"mixedsch")+(1|school),REML=FALSE)) # change reference category to mixedsch
