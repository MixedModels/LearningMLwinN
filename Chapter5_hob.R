# Heath O'Brien (heath.obrien-at-gmail-dot-com)
# MLwiN tutorial, chapter 5
# 13 July 2015

# last modified 13 July 2015

# This is an attempt to reproduce the output of chapter 5 of the MLwiN tutorial

# clear R of all objects
rm(list=ls())

library(dplyr) #arrange
library(ggplot2) #ggplot
library(lme4) #lmer
library(broom) #augment
library(arm) #se.coef
library(grid)

#5.1 Displaying multiple graphs

SchoolData<-read.csv(file = "BTSync/Courses/LearningMLwinN/tutorial.txt", sep="\t")
SchoolData$school<-factor(SchoolData$school)
SchoolData$girl<-factor(SchoolData$girl)
summary(SchoolData)

#Fit model with random intercept and fixed slope
MOD.1 <- lmer(normexam ~ standlrt + (1|school), data = SchoolData, REML=F)

#extract school-level residuals and SEs
MOD.1.RE <- data.frame(ranef(MOD.1)[1], se.coef(MOD.1)[2])
colnames(MOD.1.RE) <- c("Residuals", "SE")
MOD.1.RE$school <- rownames(MOD.1.RE) #this is risky because it assumes that the school residuals are in order
MOD.1.RE$Rank <- row_number(MOD.1.RE$Residuals) #number rows


#Make Caterpiller plot
Caterpiller <- ggplot(MOD.1.RE, aes(Rank, Residuals)) +
  geom_errorbar(aes(ymin=Residuals-SE*1.96, ymax=Residuals+SE*1.96)) +
  geom_point(shape=17, colour="blue", size=10) +
  scale_y_continuous(limits=c(-1.2, .9), breaks=seq(-1.2, .9, .3))

#Make scatterplot
Scatter <- ggplot(SchoolData, aes(standlrt, normexam)) +
  geom_point(shape=17, colour="blue", size=3) +
  scale_y_continuous(limits=c(-4, 4), breaks=seq(-3, 4, 1)) +
  scale_x_continuous(breaks=seq(-24, 32, 8)/10)

#helper function for gird plotting
vplayout <- function(x, y) viewport(layout.pos.row = x, layout.pos.col = y)

grid.newpage()
pushViewport(viewport(layout = grid.layout(2, 1)))
print(Caterpiller, vp = vplayout(1, 1))
print(Scatter, vp = vplayout(2, 1))

#5.2 Highlighting in graphs

#Plot slopes
Slopes <- augment(MOD.1) %>%
  ggplot(., aes(standlrt, .fitted)) +
  geom_line(aes(group =school), colour="blue") +
  scale_y_continuous(breaks=seq(-2.4, 1.8, .6)) + 
  scale_x_continuous(breaks=seq(-24, 32, 8)/10)

grid.newpage()
pushViewport(viewport(layout = grid.layout(3, 1)))
print(Caterpiller, vp = vplayout(1, 1))
print(Slopes, vp = vplayout(2,1))
print(Scatter, vp = vplayout(3, 1))

OverallAverage <- augment(lm(normexam ~ standlrt, data = SchoolData))
MOD.1.Augment <- augment(MOD.1)
Slopes <- ggplot(MOD.1.Augment, aes(standlrt, .fitted)) +
  geom_line(aes(group =school), colour="blue") +
  geom_line(aes(standlrt, .fitted), data=OverallAverage, colour='yellow', size=3) +
  scale_y_continuous(breaks=seq(-2.4, 1.8, .6)) + 
  scale_x_continuous(breaks=seq(-24, 32, 8)/10)

grid.newpage()
pushViewport(viewport(layout = grid.layout(3, 1)))
print(Caterpiller, vp = vplayout(1, 1))
print(Slopes, vp = vplayout(2,1))
print(Scatter, vp = vplayout(3, 1))

#Highlight school with highest residual
MaxSchool = filter(MOD.1.RE, Rank == max(MOD.1.RE$Rank))$school
grid.newpage()
pushViewport(viewport(layout = grid.layout(3, 1)))
print(Caterpiller+geom_point(shape=17, colour="red", size=10, data=filter(MOD.1.RE, school == MaxSchool)), vp = vplayout(1, 1))
print(Slopes+geom_line(colour="red", data=filter(MOD.1.Augment, school == MaxSchool)), vp = vplayout(2,1))
print(Scatter + geom_point(shape=17, colour="red", size=3, data=filter(SchoolData, school == MaxSchool)), vp = vplayout(3, 1))

#Highlight school with highest SE
MaxSE = filter(MOD.1.RE, SE == max(MOD.1.RE$SE))$school
grid.newpage()
pushViewport(viewport(layout = grid.layout(3, 1)))
print(Caterpiller+geom_point(shape=17, colour="cyan", size=10, data=filter(MOD.1.RE, school == MaxSE)), vp = vplayout(1, 1))
print(Slopes+geom_line(colour="cyan", data=filter(MOD.1.Augment, school == MaxSE)), vp = vplayout(2,1))
print(Scatter + geom_point(shape=17, colour="cyan", size=3, data=filter(SchoolData, school == MaxSE)), vp = vplayout(3, 1))

#Highlight school with lowest residual
MinSchool = filter(MOD.1.RE, Rank == min(MOD.1.RE$Rank))$school
grid.newpage()
pushViewport(viewport(layout = grid.layout(3, 1)))
print(Caterpiller+geom_point(shape=17, colour="green", size=10, data=filter(MOD.1.RE, school == MinSchool)), vp = vplayout(1, 1))
print(Slopes+geom_line(colour="green", data=filter(MOD.1.Augment, school == MinSchool)), vp = vplayout(2,1))
print(Scatter + geom_point(shape=17, colour="green", size=3, data=filter(SchoolData, school == MinSchool)), vp = vplayout(3, 1))


#Add random slopes to model
MOD.2 <- lmer(normexam ~ standlrt + (standlrt|school), data = SchoolData, REML=F)

MOD.2.RE <- data.frame(ranef(MOD.2)[1], se.coef(MOD.2)[2])
colnames(MOD.2.RE) <- c("InterceptResiduals", "SlopeResiduals", "InterceptSE", "SlopeSE")
MOD.2.RE$school <- rownames(MOD.2.RE) #this is risky because it assumes that the school residuals are in order
MOD.2.RE$Rank <- row_number(MOD.2.RE$InterceptResiduals)
MOD.2.Augment <- augment(MOD.2)

SlopeVIntercept <- ggplot(MOD.2.RE, aes(SlopeResiduals, InterceptResiduals)) +
  geom_point(shape=17, colour="blue", size=10) +
  scale_y_continuous(breaks=seq(-.8, .6, .2)) +
  scale_x_continuous(breaks=seq(-.16, .32, .08))
  
Slopes2 <- ggplot(MOD.2.Augment, aes(standlrt, .fitted)) +
  geom_line(aes(group =school), colour="blue") +
  geom_line(aes(standlrt, .fitted), data=OverallAverage, colour='yellow', size=3) +
  scale_y_continuous(breaks=seq(-2.4, 1.8, .6)) + 
  scale_x_continuous(breaks=seq(-24, 32, 8)/10)

grid.newpage()
pushViewport(viewport(layout = grid.layout(3, 1)))
print(SlopeVIntercept, vp = vplayout(1, 1))
print(Slopes2, vp = vplayout(2,1))
print(Scatter, vp = vplayout(3, 1))

#Highlight school with highest and lowest residual
MinSchool = filter(MOD.2.RE, Rank == min(MOD.2.RE$Rank))$school
MaxSchool = filter(MOD.2.RE, Rank == max(MOD.2.RE$Rank))$school
grid.newpage()
pushViewport(viewport(layout = grid.layout(3, 1)))
print(SlopeVIntercept +
        geom_point(shape=17, colour="green", size=10, data=filter(MOD.2.RE, school == MinSchool)) +
        geom_point(shape=17, colour="red", size=10, data=filter(MOD.2.RE, school == MaxSchool)), vp = vplayout(1, 1))
print(Slopes2 +
        geom_line(colour="green", data=filter(MOD.2.Augment, school == MinSchool)) +
        geom_line(colour="red", data=filter(MOD.2.Augment, school == MaxSchool)), vp = vplayout(2,1))
print(Scatter + 
        geom_point(shape=17, colour="green", size=3, data=filter(SchoolData, school == MinSchool)) +
        geom_point(shape=17, colour="red", size=3, data=filter(SchoolData, school == MaxSchool)), vp = vplayout(3, 1))


#I need to figure out how to plot CIs for the slopes, but I'm going to move on to chapter 6 for now