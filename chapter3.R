library(lme4)
library(lattice)
library(ggplot2)
#3.1: What are multilevel residuals?
SchoolData<-read.csv(file = "tutorial.txt", sep="\t")

#change variables to factors and relevel
SchoolData$school<-factor(SchoolData$school)
SchoolData$schgend<-relevel(SchoolData$schgend, ref=65)
SchoolData$girl<-factor(SchoolData$girl)
SchoolData$schgend<-relevel(SchoolData$schgend, ref="mixedsch")
summary(SchoolData)

MOD.1 <- lmer(normexam ~ (1|school), data=SchoolData)
summary(MOD.1)

MOD.1.RE <- ranef(MOD.1, condVar=TRUE, whichel = "school")

dotplot(MOD.1.RE)

#3.3: Normal plots
qqnorm(residuals(MOD.1))
qqmath(MOD.1.RE)

#Alternatively, the means and SEs can be extracted from MOD.1.RE and ploted how you like
#This is nicked from http://stackoverflow.com/questions/13847936/in-r-plotting-random-effects-from-lmer-lme4-package-using-qqmath-or-dotplot
#I'm a bit unclear on some of the details, especially the business of sd.interc (I think they're CIs)
qq <- attr(ranef(MOD.1, condVar = TRUE)[[1]], "postVar")
rand.interc<- MOD.1.RE$school
MOD.1.RES.SE <- data.frame(Intercepts = MOD.1.RE$school[,1],
                           sd.interc = 2*sqrt(qq[,,1:length(qq)]),
                           lev.names=rownames(rand.interc)
                           )
MOD.1.RES.SE$lev.names<-factor(MOD.1.RES.SE$lev.names,
                               levels=MOD.1.RES.SE$lev.names[order(MOD.1.RES.SE$Intercepts)]
                               )
ggplot(MOD.1.RES.SE,aes(lev.names,Intercepts)) +
  geom_errorbar(aes(ymin=Intercepts-sd.interc, ymax=Intercepts+sd.interc), width=0,color="black") + 
  geom_point(aes(size=2), fill='black') +
  coord_flip()
qqplot(qnorm(seq(0,1,by=1/65))[2:65], MOD.1.RES.SE$Intercept)