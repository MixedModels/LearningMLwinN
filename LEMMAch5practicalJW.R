# Lemma Ch 5 practicals 
# GLMM discussion group 13/10/15
# Josephine Walker

setwd("C:/Users/Josephine/Google Drive/Conferences and Courses/GLMM group/Lemma ch 5")

# 5.1
mydata <- read.table(file = "5.1.txt", sep = ",", header = TRUE) # read in data (from same directory)

str(mydata) # check what the data look like


# P5.1.1 A multilevel model of attainment with school effects
# null model with school effects only
# school as a random effect, assuming school-level residuals follows normal distribution

library(lme4)

nullmodel <- lmer(score ~ (1 | schoolid), data = mydata, REML = FALSE)
# specify maximum likelihood estimation not REML
# 1 | schoolid specifies random intercept and this is intercept only model (no predictors by slope)


summary(nullmodel)

# Partitioning variance
# Between-school variance is 61.02 (schoolid intercept)
# within-school variance is 258.36 (residual)
VPC <- 61.02/(258.36+61.02) 
VPC
# does anyone know how to refer to these numbers in the summary by variable names?


# P5.1.2 Examining school effects (residuals)

# test whether school random effect improves the fit of the model compared to the intercept only
fit <- lm(score ~ 1, data = mydata)

summary(fit)

logLik(nullmodel)

logLik(fit)

# LR
LR <- 2*(-143269.5 - -145144.4)
LR # difference in df = 1. High value is definitely significant, so we keep the school effects.

u0 <- ranef(nullmodel, postVar = TRUE) # deprecated?
u0 <- ranef(nullmodel, condVar=TRUE)

# create a random effects object which contains variance-covariance matrix called postVar
u0se <- sqrt(attr(u0[[1]], "postVar")[1, , ]) 
# would be var-covar matrix but only one random effect so just variance vector?

str(u0[1]) # contains school-level residuals and their posterior variances

str(u0[[1]]) # only one random effect here
head(attr(u0[[1]], "postVar")[1, , ])
head(u0[[1]])

# create dataframe with identifier, residual, and se for every school
schoolid <- as.numeric(rownames(u0[[1]]))

u0tab <- cbind(schoolid, u0[[1]], u0se)

colnames(u0tab) <- c("schoolid","u0","u0se")

# sort
u0tab <- u0tab[order(u0tab$u0), ]

u0tab <- cbind(u0tab, c(1:dim(u0tab)[1]))

colnames(u0tab)[4] <- "u0rank"

u0tab <- u0tab[order(u0tab$schoolid), ]

u0tab[1:10, ]

# caterpillar plot
plot(u0tab$u0rank, u0tab$u0, type = "n", xlab = "u_rank", ylab = "conditional modes of r.e. for school_id:_cons") # type = n means no data go in plot

segments(u0tab$u0rank, u0tab$u0 - 1.96*u0tab$u0se, u0tab$u0rank, u0tab$u0 + 1.96*u0tab$u0se)

points(u0tab$u0rank, u0tab$u0, col = "blue")

abline(h = 0, col = "red")

# small schools have wider intervals than larger schools

####################################################################

# 5.2

mydata <- read.table("5.2.txt", sep = ",", header = TRUE)

library(lme4)

# fit regression with random intercept of school, explanatory variable is numeric variable cohort ranging from -6 to 8 (centred)
fit <- lmer(score ~ cohort90 + (1 | schoolid), data = mydata, REML = FALSE)

summary(fit)
# reduction in school variance and overall variance compared to previous model, indicates taht distribution of students by cohort differs from school to school 

# save predicted value for each school
predscore <- fitted(fit)

datapred <- unique(data.frame(cbind(predscore = predscore, cohort90 = mydata$cohort90, schoolid = mydata$schoolid)))
head(datapred)

library(lattice) # need for xyplot
xyplot(predscore ~ cohort90, data = datapred, groups = schoolid, type = c("p", "l"), col = "blue") 

# get rid of schools that only have one cohort
datapred <- datapred[order(datapred$schoolid, datapred$cohort90), ]

datapred$multiplecohorts <- rep(0, length(datapred$schoolid))

datapred$multiplecohorts[datapred$schoolid %in% unique(datapred$schoolid[duplicated(datapred$schoolid)])] <- 1

xyplot(predscore ~ cohort90, data = datapred[datapred$multiplecohorts == 1, ], groups = schoolid, type = c("p", "l"), col = "blue")

###################################################################
# 5.3

mydata <- read.table("5.3.txt", header = TRUE, sep = ",")

library(lme4)

fit <- lmer(score ~ cohort90 + (1 + cohort90 | schoolid), data = mydata, REML = FALSE) # add random slope

summary(fit) # now random effec includes correlation
# if we wanted independent random intercepts and slopes then do (1| schoolid) for random intercept and + (0 + cohort90 | schoolid), otherwise they covary.


# P5.3.1 Testing for random slopes

fita <- lmer(score ~ cohort90 + (1 | schoolid), data = mydata, REML = FALSE)
summary(fita)

anova(fit, fita) # this model has better fit so cohort effect differs across schools

# P5.3.3 Examining intercept and slope residuals for schools

# covariance estimate is -1.024? text of practical doesn't quite match
VarCorr(fit)$schoolid

myrandomeff <- ranef(fit, condVar = TRUE)

plot(myrandomeff[[1]], xlab = "Intercept (u0j)", ylab = "Slope of cohort90 (u1j)")

abline(h = 0, col = "red")

abline(v = 0, col = "red")

# compute predicted score for each student
predscore <- fitted(fit)

# subset to multiple cohorts
datapred <- cbind(predscore = predscore, cohort90 = mydata$cohort90, schoolid = mydata$schoolid)

datapred <- data.frame(unique(datapred))

datapred <- datapred[order(datapred$schoolid, datapred$cohort90), ]

datapred$multiplecohorts <- rep(0, dim(datapred)[1])

datapred$multiplecohorts[datapred$schoolid %in% unique(datapred$schoolid[duplicated(datapred$schoolid)])] <- 1

xyplot(predscore ~ cohort90, data = datapred[datapred$multiplecohorts == 1, ], groups = schoolid, type = c("p", "l"), col = "blue")


# P5.3.4 Between - school variance as a function of cohort

# use given equation for level 2 variance in model with a random slope
x <- c(-6:8)

y <- 42.859 - 2.048*x + 0.161*x^2

plot(x, y, type = "l", xlim = c(-6, 10))


# P5.3.5 Adding a random coefficient for gender (dichotomous x )

# fixed effect only for comparison
(fit2a <- lmer(score ~ cohort90 + female + (1 + cohort90 | schoolid), data = mydata, REML = FALSE)) # brackets show small summary?
summary(fit2a) 

(fit2 <- lmer(score ~ cohort90 + female + (1 + cohort90 + female | schoolid), data = mydata, REML = FALSE))

anova(fit2, fit2a)


# P5.3.6 Adding a random coefficient for social class (categorical x )

mydata$sclass1 <- mydata$sclass == 1

mydata$sclass2 <- mydata$sclass == 2

mydata$sclass4 <- mydata$sclass == 4

(fit3a <- lmer(score ~ cohort90 + female + sclass1 +sclass2 + sclass4 + (1 + cohort90 | schoolid), data = mydata, REML = FALSE))

(fit3 <- lmer(score ~ cohort90 + female + sclass1 +sclass2 + sclass4 + (1 + cohort90 + sclass1 +sclass2 + sclass4 | schoolid), data = mydata, REML = FALSE))

anova(fit3, fit3a)

VarCorr(fit3)$schoolid

11.267  +  2*4.813  +  7.136

11.267  +  2*5.059  +  3.321

11.267

11.267  +  2*8.077  +  7.184

###################################################################
# 5.4

mydata <- read.table("5.4.txt", header = TRUE, sep = ",")

library(lme4)

mydata_un <- unique(mydata[, c(2, 7, 8, 9)])

cbind(Freq = table(mydata_un$schtype), Perc = prop.table(table(mydata_un$schtype)), Cum = cumsum(prop.table(table(mydata_un$schtype))))

cbind(Freq = table(mydata_un$schurban), Perc = prop.table(table(mydata_un$schurban)), Cum = cumsum(prop.table(table(mydata_un$schurban))))

cbind(Freq = table(mydata_un$schdenom), Perc = prop.table(table(mydata_un$schdenom)), Cum = cumsum(prop.table(table(mydata_un$schdenom))))

(fit1a <- lmer(score ~ cohort90 + female + sclass1 + sclass2 + sclass4 + (1 + cohort90 | schoolid), data = mydata, REML = FALSE))


# P5.4.1 Contextual effects

(fit2 <- update(fit1a, . ~ . + schtype))

(fit3 <- update(fit2, . ~ . + schurban))

(fit4 <- update(fit3, . ~ . + schdenom))


# P5.4.2 Cross - level interactions

mydata$cohort90Xschtype <- mydata$cohort90*mydata$schtype


(fit5 <- update(fit3, . ~ . + cohort90Xschtype))

# why do they make the interaction a separate variable instead of adding it in the fixed effects as is?
(fit6 <- update(fit3, .~ . + cohort90:schtype)) # : vs *





###################################################################
#### Quiz questions
mydata<-read.table(file="5.4.txt", sep=",",header=T)
library(lme4)

model.1<-lmer(score~cohort90+female+sclass1+sclass2+sclass4+schtype+schurban+(1+cohort90|schoolid),data=mydata,REML=F)
summary(model.1)

schsclass1<-aggregate(mydata$sclass1,list(mydata$schoolid),mean)
names(schsclass1)<-c("schoolid","schsclass1")
mydata.aggregate<-merge(mydata,schsclass1,by="schoolid")

model.2<-lmer(score~cohort90+female+sclass1+sclass2+sclass4+schtype+schurban+schsclass1+(1+cohort90|schoolid),data=mydata.aggregate,REML=F)
summary(model.2)

#What is the predicted exam score for a boy from the 1990 cohort with a parent in a managerial occupation whose school is state-funded, rural and has 10% of students from social class 1?
#boy is female =0
#1990 cohort is intercept variance 19.23381
#managerial parent is sclass1 estimate 10.77
#state funded school is 0 schtype
#rural school is 0 schurban
#10% schsclass1 is 0.1*20.5154 
sum(19.23381,10.77,0.1*20.5154) #32.055
#or find the right one in the fitted model - why doesn't this match??
fitted2 <- fitted(model.2)
kid <- subset(mydata.aggregate,cohort90==1 & female==0 & sclass==1 & schtype==0 & schurban==0 & schsclass1==0.1)


#By how much would you expect a student's score to change if the school had 20% of students from social class 1 instead of 10% (and all other student and school characteristics remain the same)? 
0.1*20.5154

#Consider a working class boy whose school is state-funded and rural. Suppose that he was from the 1998 cohort and attending a school with 20% of students in social class 1. What is his predicted exam score?
# working class is reference category sclass3
# add interaction to the model
model.3<-update(model.2,.~.+cohort90:schsclass1)
summary(model.3)
19.73951 + 1.57268*8 + 0.2*18.93023 + -1.234*8*0.2 # 34.133

###################################################################
# 5.5

# can't do this in R, still? any ideas?
