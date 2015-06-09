rm(list=ls()) # clear memory
tutorial.data<-read.table(file.choose(),header=T) # select data file
View(tutorial.data) # check spreadsheet
attach(tutorial.data) # attach data file

library(lme4) # calls lme4 package for fitting mixed models
summary(model5<-lmer(normexam~(1|school),REML=FALSE)) # variance components model with random intercept for school
library(lattice) # calls lattice package for additional graphics
dotplot(ranef(model5,condVar=TRUE)) # caterpillar plot of level-2 (school) resids with error bars
qqnorm(resid(model5)) # normal plot of level-1 (student) resids
qqnorm(ranef(model5)$school[[1]]) # normal plot of level-2 (school) resids
