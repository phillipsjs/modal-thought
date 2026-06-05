####                      Info:                             ####
####   Authors: Phillips, J. & Cushman, F.                   ###
####                                                         ###
####   Title: Morality constrains default representations    ###
####            of possibility                               ###
####                                                         ###
####   Contact: phillips01@g.harvard.edu                     ###

#### setup and data ####
rm(list=ls())

#set working directory
setwd("C:/Users/Jphil/Dropbox/implicitModality/implicitModality")
  
##load packages, functions, etc.
require(optimx)
library(ggplot2)
library(plyr)
library(tidyr)
library(reshape2)
library(lme4)
library(lsr)
library(corrplot)
library(ppcor)
library(mediation)

blackGreyPalette <- c("#2C3539", "#999999")
substrRight <- function(x, n){
  substr(x, nchar(x)-n+1, nchar(x))
}
   
## load data
#study 1
d1a <- read.csv("data/study1possibilityData.csv", stringsAsFactors = F) # Possibility judgments
d1b <- read.csv("data/study1eventData.csv",stringsAsFactors = F) # Event ratings
#study 2
d2a <- read.csv("data/study2oughtData.csv", stringsAsFactors = F) # Ought judgments
d2b <- read.csv("data/study2mightData.csv", stringsAsFactors = F) # Might judgments
d2c <- read.csv("data/study2couldData.csv", stringsAsFactors = F) # Could judgments
d2d <- read.csv("data/study2mayData.csv", stringsAsFactors = F) # May judgments
d2e <- read.csv("data/study2shouldData.csv", stringsAsFactors = F) # Should judgments
#study 3
d3 <- read.csv("data/study3forceData.csv") # Force judgments
d3b <-read.csv("data/study3wantData.csv") # Want judgments

d1a$study <- "Study 1a"
d1b$study <- "Study 1b"
d2a$study <- "Study 2a"
d2b$study <- "Study 2b"
d2c$study <- "Study 2c"
d2d$study <- "Study 2d"
d2e$study <- "Study 2e"
d3$study <- "Study 3"
d3b$study <- "Study 3b"

#### Demographics ####

## for all studies conducted on testable
demog <- rbind(
  d1a[!(duplicated(d1a$turkID)),c(31:37,52)], 
  d1b[!(duplicated(d1b$turkID)),c(31:37,52)], 
  d2a[!(duplicated(d2a$turkID)),c(31:37,52)], 
  d2b[!(duplicated(d2b$turkID)),c(31:37,52)], 
  d2c[!(duplicated(d2c$turkID)),c(31:37,52)], 
  d2d[!(duplicated(d2d$turkID)),c(31:37,52)], 
  d2e[!(duplicated(d2e$turkID)),c(31:37,52)]
)

demog$sex <- factor(demog$sex)
demog$sex <- factor(c("Female","Male","Other")[demog$sex])

demog.age <- aggregate(age~study, demog[demog$age>18,], #Anything under 18 is a typo as Amazon restricts mturk workers below 18
                       FUN=function(x) c(M =mean(x), SD =sd(x))) 
demog.gender <- aggregate(sex~study, demog, FUN= table)
demog.n <- aggregate(turkID~study, demog, FUN=length)

### age and gender info
print(cbind(demog.age,demog.gender[,2],demog.n[,2])) #nb: where n > (female+male), n-(female+male) participants did not report gender

### education
demog$education <- factor(demog$education)
demog$education <- factor(c("less than high school","high school or equivalent","college or technical school",
                            "Bachelors degree","Master or Doctorate degree")[demog$education])
demog.education <- aggregate(education~study, demog, FUN=table)

#study 3 and 3b conducted on qualtrics so the demographic items were slightly different
# Force study
d3$gender <- factor(c("male","female")[d3$gender])
table(d3$gender,exclude=NULL)
d3.age <- matrix(c("mean",mean(d3$age,na.rm=T),"sd",sd(d3$age,na.rm=T),"n",length(d3$age)),ncol=3)
print(d3.age)

# Desire study
d3b$gender <- factor(c("male","female")[d3b$gender])
table(d3b$gender,exclude=NULL)
d3b.age <- matrix(c("mean",mean(d3b$age,na.rm=T),"sd",sd(d3b$age,na.rm=T),"n",length(d3b$age)),ncol=3)
print(d3b.age)

#### Explicit vs. Implicit Representations of Possibility #### 

#d1a <- read.csv("data/study1possibilityData.csv", stringsAsFactors = F)

d1a <- d1a[d1a$condition1!="na",] ## removes practice trials and prompts
d1a <- d1a[d1a$responses!=99,] ## removes seen but unanswered trials (e.g., timeouts)

d1a$condition1 <- factor(d1a$condition1, levels=c("possible","immoral","improbable","irrational","impossible"))
d1a$condition2 <- factor(d1a$condition2, levels=c("scenario1","scenario2","scenario3","scenario4","scenario5","scenario6"))
d1a$condition3 <- factor(d1a$condition3, levels=c("reflective","speeded"))

d1a$responses <- factor(d1a$responses, levels=c("f","j"))
d1a$responses <- as.numeric(d1a$responses)-1

d1a.exclude <- ddply(d1a[d1a$RTs<6000,], c("turkID","condition3"), summarise,
                     meanRT = mean(RTs, na.rm=T)
)

ddply(d1a[d1a$RTs<6000,], c("condition3"), summarise,
      meanRT = mean(RTs, na.rm=T),
      sdRT = sd(RTs, na.rm=T)
)

d1a.excluded1a <- d1a.exclude$turkID[d1a.exclude$condition3=="reflective" & d1a.exclude$meanRT<1000]
d1a.excluded2 <- d1a.exclude$turkID[d1a.exclude$condition3=="speeded" & d1a.exclude$meanRT<800]

d1a <- d1a[!(d1a$turkID %in% d1a.excluded1a| d1a$turkID %in% d1a.excluded2),]

d1a <- d1a[d1a$RTs>500 ,]
d1a <- d1a[!(d1a$RTs<1500 & d1a$condition3=="reflective"),] ## comment out this line to see results without reflecitve exclusion condition

d1a.sum1 <- ddply(d1a, c("condition1","condition3","turkID"), summarise,
                  responses = mean(responses, na.rm=TRUE))

d1a.sum <- ddply(d1a.sum1, c("condition1","condition3"), summarise,
                 N    = length(responses),
                 mean = mean(responses, na.rm=TRUE)*100,
                 sd   = sd(responses,na.rm=TRUE)*100,
                 se   = sd / sqrt(N) )

d1a.sum$condition3 <- factor(c("Reflective","Speeded")[d1a.sum$condition3])
d1a.sum$condition1 <- factor(c("Ordinary","Immoral","Improbable","Irrational","Impossible")[d1a.sum$condition1])
d1a.sum$condition1 <- factor(d1a.sum$condition1,levels=c("Ordinary","Improbable","Impossible","Immoral","Irrational"))

fig1 <- ggplot(d1a.sum, aes(x=condition3, y=mean, fill=condition3)) +
  geom_bar(position="dodge", stat="identity")  +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.1, position=position_dodge(.9)) +
  facet_grid(~condition1) +
  scale_fill_manual(values=blackGreyPalette) +
  ylab("% of Events Judged Impossible") +
  xlab("") +
  coord_cartesian(ylim=c(0,90)) +
  theme_bw() +
  theme(
    plot.background = element_blank()
    ,panel.grid.major = element_blank()
    ,panel.grid.minor = element_blank()
    ,legend.title=element_blank()
    ,legend.position=c(.9,.9)
    ,legend.text=element_text(size=rel(1.4))
    ,axis.text.y=element_text(size=rel(1.5))
    ,axis.text.x=element_blank()
    ,axis.title.y=element_text(vjust=.9)
    ,axis.ticks = element_blank()
    ,strip.text=element_text(size=rel(1.5))
    ,axis.title=element_text(size=rel(1.5))    
  )

print(fig1)

# pdf("figs/fig1.pdf",width=9.5,height=8)
# plot(fig1)
# dev.off()

### overall tests ###
d1a$responses <- factor(c("possible","impossible")[d1a$responses+1]) 
lm1.0 <- glmer(responses ~ condition1 * condition3 + (condition1|turkID) + (1|condition2), data=d1a, family = "binomial", control = glmerControl(optimizer = c("optimx"), optCtrl = list(method = "nlminb")))
# interaction
lm1.1 <- glmer(responses ~ condition1 + condition3 + (condition1|turkID) + (1|condition2), data=d1a, family = "binomial",  control = glmerControl(optimizer = c("optimx"), optCtrl = list(method = "nlminb")))
anova(lm1.0,lm1.1)
# main effect of deliberation
lm1.2 <- glmer(responses ~ condition1 + (condition1|turkID) + (1|condition2), data=d1a, family = "binomial",  control = glmerControl(optimizer = c("optimx"), optCtrl = list(method = "nlminb")))
anova(lm1.1,lm1.2)
# main effect of event-type
lm1.3 <- glmer(responses ~ condition3 + (condition1|turkID) + (1|condition2), data=d1a, family = "binomial", control = glmerControl(optimizer = c("optimx"), optCtrl = list(method = "nlminb")))
anova(lm1.1,lm1.3)

## test for effect of deliberation on each type of event
print(d1a.sum)
summary(glmer(responses ~ condition3 + (1|turkID) + (1|condition2), data=d1a[d1a$condition1=="possible",], family = "binomial"))
summary(glmer(responses ~ condition3 + (1|turkID) + (1|condition2), data=d1a[d1a$condition1=="improbable",], family = "binomial"))
summary(glmer(responses ~ condition3 + (1|turkID) + (1|condition2), data=d1a[d1a$condition1=="impossible",], family = "binomial"))
summary(glmer(responses ~ condition3 + (1|turkID) + (1|condition2), data=d1a[d1a$condition1=="immoral",], family = "binomial"))
summary(glmer(responses ~ condition3 + (1|turkID) + (1|condition2), data=d1a[d1a$condition1=="irrational",], family = "binomial"))


## comparison between improbable and immoral
lm1.4 <- glmer(responses ~ condition1 * condition3 + (condition1|turkID) + (1|condition2), 
               data=d1a[d1a$condition1=="improbable"|d1a$condition1=="immoral",],
               family = "binomial", control = glmerControl(optimizer = c("optimx"), optCtrl = list(method = "nlminb")))
# interaction
lm1.5 <- glmer(responses ~ condition1 + condition3 + (condition1|turkID) + (1|condition2), 
               data=d1a[d1a$condition1=="improbable"|d1a$condition1=="immoral",], 
               family = "binomial",  control = glmerControl(optimizer = c("optimx"), optCtrl = list(method = "nlminb")))
anova(lm1.4,lm1.5)

## comparison between irrational and immoral
lm1.6 <- glmer(responses ~ condition1 * condition3 + (condition1|turkID) + (1|condition2), 
               data=d1a[d1a$condition1=="irrational"|d1a$condition1=="immoral",],
               family = "binomial", control = glmerControl(optimizer = c("optimx"), optCtrl = list(method = "nlminb")))
# interaction
lm1.7 <- glmer(responses ~ condition1 + condition3 + (condition1|turkID) + (1|condition2), 
               data=d1a[d1a$condition1=="irrational"|d1a$condition1=="immoral",], 
               family = "binomial",  control = glmerControl(optimizer = c("optimx"), optCtrl = list(method = "nlminb")))
anova(lm1.6,lm1.7)


## Ratings of events

#d1b <- read.csv("data/study1bdata.csv")

d1b$subjectGroup <- factor(c("probability","morality","rationality")[d1b$subjectGroup])
d1b$condition1 <- factor(d1b$condition1, levels=c("possible","immoral","improbable","irrational","impossible"))

d1b <- d1b[d1b$responses!=6,] ## this is excludes the NA responses


d1b.sum <- ddply(d1b, c("condition1","subjectGroup","turkID"), summarise,
                 N    = length(responses),
                 avg = mean(responses, na.rm=TRUE)
)

d1b.sum1 <- ddply(d1b.sum, c("condition1","subjectGroup"), summarise,
                  N    = length(avg),
                  mean = mean(avg, na.rm=TRUE),
                  sd   = sd(avg,na.rm=TRUE),
                  se   = sd / sqrt(N) 
)

d1b.sum1$condition1 <- factor(c("Ordinary","Immoral","Improbable","Irrational","Impossible")[d1b.sum1$condition1])
d1b.sum1$condition1 <- factor(d1b.sum1$condition1, levels=c("Ordinary","Improbable","Impossible","Immoral","Irrational"))
d1b.sum1$subjectGroup <- factor(c("Immorality","Improbability","Irrationality")[d1b.sum1$subjectGroup])
d1b.sum1$subjectGroup <- factor(d1b.sum1$subjectGroup, levels = c("Improbability","Irrationality","Immorality"))

fig1b <- ggplot(d1b.sum1, aes(x=subjectGroup, y=mean, fill=subjectGroup)) +
  geom_bar(position="dodge", stat="identity")  +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.1, position=position_dodge(.9)) +
  facet_wrap(~condition1) +
  scale_fill_manual(values=wes_palette("Moonrise2",3)) +
  ylab("Judgment of Event") +
  xlab("") +
  #coord_cartesian(ylim=c(0,90)) +
  theme_bw() +
  theme(
    plot.background = element_blank()
    ,panel.grid.major = element_blank()
    ,panel.grid.minor = element_blank()
    ,legend.title=element_blank()
    ,legend.position=c(.8,.25)
    ,legend.text=element_text(size=rel(1.4))
    ,axis.text.y=element_text(size=rel(1.5))
    ,axis.text.x=element_blank()
    ,axis.title.y=element_text(vjust=.9)
    ,axis.ticks = element_blank()
    ,strip.text=element_text(size=rel(1.5))
    ,axis.title=element_text(size=rel(1.5))    
  )

print(fig1b)


## Ordinary Events
### probability
var.test(d1b.sum$avg[d1b.sum$condition1=="possible" & d1b.sum$subjectGroup=="probability"],
         d1b.sum$avg[d1b.sum$condition1=="improbable" & d1b.sum$subjectGroup=="probability"])
t.test(d1b.sum$avg[d1b.sum$condition1=="possible" & d1b.sum$subjectGroup=="probability"],
       d1b.sum$avg[d1b.sum$condition1=="improbable" & d1b.sum$subjectGroup=="probability"], var.equal=T)
cohensD(d1b.sum$avg[d1b.sum$condition1=="possible" & d1b.sum$subjectGroup=="probability"],
        d1b.sum$avg[d1b.sum$condition1=="improbable" & d1b.sum$subjectGroup=="probability"])
### rationality
var.test(d1b.sum$avg[d1b.sum$condition1=="possible" & d1b.sum$subjectGroup=="rationality"],
         d1b.sum$avg[d1b.sum$condition1=="irrational" & d1b.sum$subjectGroup=="rationality"])
t.test(d1b.sum$avg[d1b.sum$condition1=="possible" & d1b.sum$subjectGroup=="rationality"],
       d1b.sum$avg[d1b.sum$condition1=="irrational" & d1b.sum$subjectGroup=="rationality"], var.equal=T)
cohensD(d1b.sum$avg[d1b.sum$condition1=="possible" & d1b.sum$subjectGroup=="rationality"],
        d1b.sum$avg[d1b.sum$condition1=="irrational" & d1b.sum$subjectGroup=="rationality"])
### morality
var.test(d1b.sum$avg[d1b.sum$condition1=="possible" & d1b.sum$subjectGroup=="morality"],
         d1b.sum$avg[d1b.sum$condition1=="immoral" & d1b.sum$subjectGroup=="morality"])
t.test(d1b.sum$avg[d1b.sum$condition1=="possible" & d1b.sum$subjectGroup=="morality"],
       d1b.sum$avg[d1b.sum$condition1=="immoral" & d1b.sum$subjectGroup=="morality"], var.equal=T)
cohensD(d1b.sum$avg[d1b.sum$condition1=="possible" & d1b.sum$subjectGroup=="morality"],
        d1b.sum$avg[d1b.sum$condition1=="immoral" & d1b.sum$subjectGroup=="morality"])

## Improbable Events
### rationality
var.test(d1b.sum$avg[d1b.sum$condition1=="improbable" & d1b.sum$subjectGroup=="rationality"],
         d1b.sum$avg[d1b.sum$condition1=="irrational" & d1b.sum$subjectGroup=="rationality"])
t.test(d1b.sum$avg[d1b.sum$condition1=="improbable" & d1b.sum$subjectGroup=="rationality"],
       d1b.sum$avg[d1b.sum$condition1=="irrational" & d1b.sum$subjectGroup=="rationality"], var.equal=T)
cohensD(d1b.sum$avg[d1b.sum$condition1=="improbable" & d1b.sum$subjectGroup=="rationality"],
        d1b.sum$avg[d1b.sum$condition1=="irrational" & d1b.sum$subjectGroup=="rationality"])
### morality
var.test(d1b.sum$avg[d1b.sum$condition1=="improbable" & d1b.sum$subjectGroup=="morality"],
         d1b.sum$avg[d1b.sum$condition1=="immoral" & d1b.sum$subjectGroup=="morality"])
t.test(d1b.sum$avg[d1b.sum$condition1=="improbable" & d1b.sum$subjectGroup=="morality"],
       d1b.sum$avg[d1b.sum$condition1=="immoral" & d1b.sum$subjectGroup=="morality"], var.equal=T)
cohensD(d1b.sum$avg[d1b.sum$condition1=="improbable" & d1b.sum$subjectGroup=="morality"],
        d1b.sum$avg[d1b.sum$condition1=="immoral" & d1b.sum$subjectGroup=="morality"])

## Impossible Events
### probability
var.test(d1b.sum$avg[d1b.sum$condition1=="impossible" & d1b.sum$subjectGroup=="probability"],
         d1b.sum$avg[d1b.sum$condition1=="improbable" & d1b.sum$subjectGroup=="probability"])
t.test(d1b.sum$avg[d1b.sum$condition1=="impossible" & d1b.sum$subjectGroup=="probability"],
       d1b.sum$avg[d1b.sum$condition1=="improbable" & d1b.sum$subjectGroup=="probability"], var.equal=T)
cohensD(d1b.sum$avg[d1b.sum$condition1=="impossible" & d1b.sum$subjectGroup=="probability"],
        d1b.sum$avg[d1b.sum$condition1=="improbable" & d1b.sum$subjectGroup=="probability"])
### rationality
var.test(d1b.sum$avg[d1b.sum$condition1=="impossible" & d1b.sum$subjectGroup=="rationality"],
         d1b.sum$avg[d1b.sum$condition1=="irrational" & d1b.sum$subjectGroup=="rationality"])
t.test(d1b.sum$avg[d1b.sum$condition1=="impossible" & d1b.sum$subjectGroup=="rationality"],
       d1b.sum$avg[d1b.sum$condition1=="irrational" & d1b.sum$subjectGroup=="rationality"])
cohensD(d1b.sum$avg[d1b.sum$condition1=="impossible" & d1b.sum$subjectGroup=="rationality"],
        d1b.sum$avg[d1b.sum$condition1=="irrational" & d1b.sum$subjectGroup=="rationality"])
### morality
var.test(d1b.sum$avg[d1b.sum$condition1=="impossible" & d1b.sum$subjectGroup=="morality"],
         d1b.sum$avg[d1b.sum$condition1=="immoral" & d1b.sum$subjectGroup=="morality"])
t.test(d1b.sum$avg[d1b.sum$condition1=="impossible" & d1b.sum$subjectGroup=="morality"],
       d1b.sum$avg[d1b.sum$condition1=="immoral" & d1b.sum$subjectGroup=="morality"], var.equal=T)
cohensD(d1b.sum$avg[d1b.sum$condition1=="impossible" & d1b.sum$subjectGroup=="morality"],
        d1b.sum$avg[d1b.sum$condition1=="immoral" & d1b.sum$subjectGroup=="morality"])

## Immoral Events
### probability
var.test(d1b.sum$avg[d1b.sum$condition1=="immoral" & d1b.sum$subjectGroup=="probability"],
         d1b.sum$avg[d1b.sum$condition1=="improbable" & d1b.sum$subjectGroup=="probability"])
t.test(d1b.sum$avg[d1b.sum$condition1=="immoral" & d1b.sum$subjectGroup=="probability"],
       d1b.sum$avg[d1b.sum$condition1=="improbable" & d1b.sum$subjectGroup=="probability"], var.equal=T)
cohensD(d1b.sum$avg[d1b.sum$condition1=="immoral" & d1b.sum$subjectGroup=="probability"],
        d1b.sum$avg[d1b.sum$condition1=="improbable" & d1b.sum$subjectGroup=="probability"])
### rationality
var.test(d1b.sum$avg[d1b.sum$condition1=="immoral" & d1b.sum$subjectGroup=="rationality"],
         d1b.sum$avg[d1b.sum$condition1=="irrational" & d1b.sum$subjectGroup=="rationality"])
t.test(d1b.sum$avg[d1b.sum$condition1=="immoral" & d1b.sum$subjectGroup=="rationality"],
       d1b.sum$avg[d1b.sum$condition1=="irrational" & d1b.sum$subjectGroup=="rationality"], var.equal = T)
cohensD(d1b.sum$avg[d1b.sum$condition1=="immoral" & d1b.sum$subjectGroup=="rationality"],
        d1b.sum$avg[d1b.sum$condition1=="irrational" & d1b.sum$subjectGroup=="rationality"])

## Irrational events Events
### probability
var.test(d1b.sum$avg[d1b.sum$condition1=="irrational" & d1b.sum$subjectGroup=="probability"],
         d1b.sum$avg[d1b.sum$condition1=="improbable" & d1b.sum$subjectGroup=="probability"])
t.test(d1b.sum$avg[d1b.sum$condition1=="irrational" & d1b.sum$subjectGroup=="probability"],
       d1b.sum$avg[d1b.sum$condition1=="improbable" & d1b.sum$subjectGroup=="probability"], var.equal=T)
cohensD(d1b.sum$avg[d1b.sum$condition1=="irrational" & d1b.sum$subjectGroup=="probability"],
        d1b.sum$avg[d1b.sum$condition1=="improbable" & d1b.sum$subjectGroup=="probability"])
### morality
var.test(d1b.sum$avg[d1b.sum$condition1=="irrational" & d1b.sum$subjectGroup=="morality"],
         d1b.sum$avg[d1b.sum$condition1=="immoral" & d1b.sum$subjectGroup=="morality"])
t.test(d1b.sum$avg[d1b.sum$condition1=="irrational" & d1b.sum$subjectGroup=="morality"],
       d1b.sum$avg[d1b.sum$condition1=="immoral" & d1b.sum$subjectGroup=="morality"], var.equal=T)
cohensD(d1b.sum$avg[d1b.sum$condition1=="irrational" & d1b.sum$subjectGroup=="morality"],
        d1b.sum$avg[d1b.sum$condition1=="immoral" & d1b.sum$subjectGroup=="morality"])

#### Generalizing across Modal Judgments ####

d2a$turkID <- d2a$turkID + max(d1a$turkID)
d2b$turkID <- d2b$turkID + max(d2a$turkID)
d2c$turkID <- d2c$turkID + max(d2b$turkID)
d2d$turkID <- d2d$turkID + max(d2c$turkID)
d2e$turkID <- d2e$turkID + max(d2d$turkID)

d2a$modal <- "Ought"
d2b$modal <- "Might"
d2c$modal <- "Could"
d2d$modal <- "May"
d2e$modal <- "Should"

d2 <- rbind(d2a,d2b,d2c,d2d,d2e)

d2 <- d2[d2$condition1!="na",]
d2 <- d2[d2$responses!=99,]

d2$condition1 <- factor(d2$condition1, levels=c("possible","immoral","improbable","irrational","impossible"))
d2$condition2 <- factor(d2$condition2, levels=c("scenario1","scenario2","scenario3","scenario4","scenario5","scenario6"))
d2$condition3 <- factor(d2$condition3, levels=c("reflective","speeded"))

d2$responses <- factor(d2$responses, levels=c("f","j"))
d2$responses <- as.numeric(d2$responses)-1

d2.exclude <- ddply(d2[d2$RTs<6000,], c("turkID","condition3"), summarise,
                    meanRT = mean(RTs, na.rm=T)
)

d2.excluded1 <- d2.exclude$turkID[d2.exclude$condition3=="reflective" & d2.exclude$meanRT<1000]
d2.excluded2 <- d2.exclude$turkID[d2.exclude$condition3=="speeded" & d2.exclude$meanRT<800]

d2 <- d2[!(d2$turkID %in% d2.excluded1| d2$turkID %in% d2.excluded2),]

d2 <- d2[d2$RTs>500 ,]
d2 <- d2[!(d2$RTs<1500 & d2$condition3=="reflective"),] ## Comment out this line if you want to look at analyses without reflective exclusion!

### Simple demonstration of the reading given each modal term ###

d1a$modal <- "Possible"
d2 <- rbind(d2,d1a)

d2$norm[d2$condition1=="possible"] <- "None"
d2$norm[d2$condition1=="improbable"|d2$condition1=="impossible"] <- "Descriptive"
d2$norm[d2$condition1=="immoral"|d2$condition1=="irrational"] <- "Prescriptive"

d2.sum1 <- ddply(d2, c("modal","norm","trialNo"), summarise, 
                  responses = mean(responses, na.rm=TRUE))
## if you get errors here, it's likely from factoring the the possibillity judgments for the glmers above

d2.sum <- ddply(d2.sum1, c("modal","norm"), summarise,
                 N    = length(responses),
                 mean = mean(responses, na.rm=TRUE)*100,
                 sd   = sd(responses,na.rm=TRUE)*100,
                 se   = sd / sqrt(N) )

d2.sum$norm <- factor(d2.sum$norm, levels = c("None","Descriptive","Prescriptive"))
d2.sum$modal <- factor(d2.sum$modal, levels = c("Possible","Could","May","Might","Should","Ought"))


fig2 <- ggplot(d2.sum, aes(x=norm, y=mean, fill=norm)) +
  geom_bar(position="dodge", stat="identity")  +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.1, position=position_dodge(.9)) +
  facet_grid(~modal) +
  scale_fill_manual(values=wes_palette("Moonrise2",3)) +
  ylab("Mean modal judgment") +
  xlab("") +
  coord_cartesian(ylim=c(0,90)) +
  theme_bw() +
  theme(
    plot.background = element_blank()
    ,panel.grid.major = element_blank()
    ,panel.grid.minor = element_blank()
    ,legend.title=element_blank()
    ,legend.text=element_text(size=rel(1.4))
    ,axis.text.y=element_text(size=rel(1.5))
    ,axis.text.x=element_blank()
    ,axis.title.y=element_text(vjust=.9)
    ,axis.ticks = element_blank()
    ,strip.text=element_text(size=rel(1.5))
    ,axis.title=element_text(size=rel(1.5))    
  )

print(fig2)


# pdf("figs/fig2a.pdf",width=9.5,height=8)
# plot(fig2a)
# dev.off()

d2.sum2 <- ddply(d2, c("condition1","condition3","modal"), summarise,
                 N    = length(responses),
                 mean = mean(responses, na.rm=TRUE)*100,
                 sd   = sd(responses,na.rm=TRUE)*100,
                 se   = sd / sqrt(N) )

d2.sum2$condition3 <- factor(c("Reflective","Speeded")[d2.sum2$condition3])
d2.sum2$condition1 <- factor(c("Ordinary","Immoral","Improbable","Irrational","Impossible")[d2.sum2$condition1])
d2.sum2$condition1 <- factor(d2.sum2$condition1,levels=c("Ordinary","Improbable","Impossible","Immoral","Irrational"))

fig1 <- ggplot(d2.sum2[d2.sum2$modal=="Could",], aes(x=condition3, y=mean, fill=condition3)) +
  geom_bar(position="dodge", stat="identity")  +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.1, position=position_dodge(.9)) +
  facet_grid(~condition1) +
  scale_fill_manual(values=blackGreyPalette) +
  ylab("% of Events Judged Impossible") +
  xlab("") +
  coord_cartesian(ylim=c(0,90)) +
  theme_bw() +
  theme(
    plot.background = element_blank()
    ,panel.grid.major = element_blank()
    ,panel.grid.minor = element_blank()
    ,legend.title=element_blank()
    ,legend.position=c(.9,.9)
    ,legend.text=element_text(size=rel(1.4))
    ,axis.text.y=element_text(size=rel(1.5))
    ,axis.text.x=element_blank()
    ,axis.title.y=element_text(vjust=.9)
    ,axis.ticks = element_blank()
    ,strip.text=element_text(size=rel(1.5))
    ,axis.title=element_text(size=rel(1.5))    
  )

print(fig1)




## Possible
var.test(d2a.sum2$mean[d2a.sum2$modal=="Possible" & d2a.sum2$norm=="Descriptive"],
         d2a.sum2$mean[d2a.sum2$modal=="Possible" & d2a.sum2$norm=="Prescriptive"])
t.test(d2a.sum2$mean[d2a.sum2$modal=="Possible" & d2a.sum2$norm=="Descriptive"],
       d2a.sum2$mean[d2a.sum2$modal=="Possible" & d2a.sum2$norm=="Prescriptive"])
cohensD(d2a.sum2$mean[d2a.sum2$modal=="Possible" & d2a.sum2$norm=="Descriptive"],
        d2a.sum2$mean[d2a.sum2$modal=="Possible" & d2a.sum2$norm=="Prescriptive"])
## Could
var.test(d2a.sum2$mean[d2a.sum2$modal=="Could" & d2a.sum2$norm=="Descriptive"],
         d2a.sum2$mean[d2a.sum2$modal=="Could" & d2a.sum2$norm=="Prescriptive"])
t.test(d2a.sum2$mean[d2a.sum2$modal=="Could" & d2a.sum2$norm=="Descriptive"],
       d2a.sum2$mean[d2a.sum2$modal=="Could" & d2a.sum2$norm=="Prescriptive"])
cohensD(d2a.sum2$mean[d2a.sum2$modal=="Could" & d2a.sum2$norm=="Descriptive"],
        d2a.sum2$mean[d2a.sum2$modal=="Could" & d2a.sum2$norm=="Prescriptive"])
## Might
var.test(d2a.sum2$mean[d2a.sum2$modal=="Might" & d2a.sum2$norm=="Descriptive"],
         d2a.sum2$mean[d2a.sum2$modal=="Might" & d2a.sum2$norm=="Prescriptive"])
t.test(d2a.sum2$mean[d2a.sum2$modal=="Might" & d2a.sum2$norm=="Descriptive"],
       d2a.sum2$mean[d2a.sum2$modal=="Might" & d2a.sum2$norm=="Prescriptive"])
cohensD(d2a.sum2$mean[d2a.sum2$modal=="Might" & d2a.sum2$norm=="Descriptive"],
        d2a.sum2$mean[d2a.sum2$modal=="Might" & d2a.sum2$norm=="Prescriptive"])
## May
var.test(d2a.sum2$mean[d2a.sum2$modal=="May" & d2a.sum2$norm=="Descriptive"],
         d2a.sum2$mean[d2a.sum2$modal=="May" & d2a.sum2$norm=="Prescriptive"])
t.test(d2a.sum2$mean[d2a.sum2$modal=="May" & d2a.sum2$norm=="Descriptive"],
       d2a.sum2$mean[d2a.sum2$modal=="May" & d2a.sum2$norm=="Prescriptive"])
cohensD(d2a.sum2$mean[d2a.sum2$modal=="May" & d2a.sum2$norm=="Descriptive"],
        d2a.sum2$mean[d2a.sum2$modal=="May" & d2a.sum2$norm=="Prescriptive"])
## Should
var.test(d2a.sum2$mean[d2a.sum2$modal=="Should" & d2a.sum2$norm=="Descriptive"],
         d2a.sum2$mean[d2a.sum2$modal=="Should" & d2a.sum2$norm=="Prescriptive"])
t.test(d2a.sum2$mean[d2a.sum2$modal=="Should" & d2a.sum2$norm=="Descriptive"],
       d2a.sum2$mean[d2a.sum2$modal=="Should" & d2a.sum2$norm=="Prescriptive"])
cohensD(d2a.sum2$mean[d2a.sum2$modal=="Should" & d2a.sum2$norm=="Descriptive"],
        d2a.sum2$mean[d2a.sum2$modal=="Should" & d2a.sum2$norm=="Prescriptive"])



###### Correlation based analyses  ###

d2.items <- cbind(
  # speeded
  ddply(d1a[d1a$condition3=="speeded",], c("trialNo","condition3","condition1"), summarize, speeded_possibility = mean(responses, na.rm=TRUE))[c(1,3:4)],
  ddply(d2[d2$condition3=="speeded" & d2$modal=="Ought",], c("trialNo","condition3","condition1"), summarize, speeded_ought = mean(responses, na.rm=TRUE))[4],
  ddply(d2[d2$condition3=="speeded" & d2$modal=="Might",], c("trialNo","condition3","condition1"), summarize, speeded_might = mean(responses, na.rm=TRUE))[4],
  ddply(d2[d2$condition3=="speeded" & d2$modal=="Could",], c("trialNo","condition3","condition1"), summarize, speeded_could = mean(responses, na.rm=TRUE))[4],
  ddply(d2[d2$condition3=="speeded" & d2$modal=="May",], c("trialNo","condition3","condition1"), summarize, speeded_may = mean(responses, na.rm=TRUE))[4],
  ddply(d2[d2$condition3=="speeded" & d2$modal=="Should",], c("trialNo","condition3","condition1"), summarize, speeded_should = mean(responses, na.rm=TRUE))[4],
  
  # reflective judgments
  ddply(d1a[d1a$condition3=="reflective",], c("trialNo","condition3","condition1"), summarize, reflective_possibility = mean(responses, na.rm=TRUE))[4],
  ddply(d2[d2$condition3=="reflective" & d2$modal=="Ought",], c("trialNo","condition3","condition1"), summarize, reflective_ought = mean(responses, na.rm=TRUE))[4],
  ddply(d2[d2$condition3=="reflective" & d2$modal=="Might",], c("trialNo","condition3","condition1"), summarize, reflective_might = mean(responses, na.rm=TRUE))[4],
  ddply(d2[d2$condition3=="reflective" & d2$modal=="Could",], c("trialNo","condition3","condition1"), summarize, reflective_could = mean(responses, na.rm=TRUE))[4],
  ddply(d2[d2$condition3=="reflective" & d2$modal=="May",], c("trialNo","condition3","condition1"), summarize, reflective_may = mean(responses, na.rm=TRUE))[4],
  ddply(d2[d2$condition3=="reflective" & d2$modal=="Should",], c("trialNo","condition3","condition1"), summarize, reflective_should = mean(responses, na.rm=TRUE))[4]
)
## if you get an error here it may be caused by the responses in d1 still being factored from running the glmers above

d.corrA <- cor(d2.items[,-c(1:2)])
d.corrA2 <- as.data.frame.table(d.corrA)
d.corrA2.S <- d.corrA2[c(2:6,15:18,28:30,41:42,54),] # all non-duplicated and non-reflexive *speeded* correlations
d.corrA2.R <- d.corrA2[c(80:84,93:96,106:108,119:120,132),] # all non-duplicated and non-reflexive *reflective* correlations

print(matrix(c("Mean","SD",mean(d.corrA2.S$Freq),sd(d.corrA2.S$Freq)),nrow=2))
print(matrix(c("Mean","SD",mean(d.corrA2.R$Freq),sd(d.corrA2.R$Freq)),nrow=2))

var.test(d.corrA2.S$Freq,d.corrA2.R$Freq)
t.test(d.corrA2.S$Freq,d.corrA2.R$Freq,equal.var=T, paired = T)
cohensD(d.corrA2.S$Freq,d.corrA2.R$Freq)

d.corrA2.S$condition <- "Speeded"
d.corrA2.S$pair <- c("Ought - Possibility","Might - Possibility", "Could - Possibility", "May - Possibility", "Should - Possibility",
                     "Might - Ought", "Could - Ought", "May - Ought", "Should - Ought", "Could - Might", "May - Might", "Should - Might",
                     "May - Could","Should - Could", "Should - May")
d.corrA2.R$condition <- "Reflective"
d.corrA2.R$pair <- c("Ought - Possibility","Might - Possibility", "Could - Possibility", "May - Possibility", "Should - Possibility",
                     "Might - Ought", "Could - Ought", "May - Ought", "Should - Ought", "Could - Might", "May - Might", "Should - Might",
                     "May - Could","Should - Could", "Should - May")

d.corrA2 <- rbind(d.corrA2.S,d.corrA2.R)
d.corrA2$condition <- factor(d.corrA2$condition, levels=c("Speeded","Reflective"))
d.corrA2$pair <- factor(d.corrA2$pair, levels = c("Should - Ought","Could - Possibility","May - Might","May - Could",
                                                  "May - Possibility","Should - Might","Could - Might","Should - May","Might - Ought",
                                                  "May - Ought","Might - Possibility","Should - Could","Could - Ought",
                                                  "Should - Possibility","Ought - Possibility"))

plot2.1 <- ggplot(d.corrA2,aes(x=condition,y=Freq)) +
  geom_line(aes(group=pair,color=pair)) +
  geom_point(aes(group=pair,color=pair)) +
  #facet_wrap(~condition) +
  ylab("Correlation between Modal Judgments") +
  xlab("") +
  #coord_cartesian(ylim=c(0,.9)) +
  theme_bw() +
  theme(
    plot.background = element_blank()
    ,panel.grid.major = element_blank()
    ,panel.grid.minor = element_blank()
    ,legend.title=element_blank()
    #,legend.position=c(.89,.5)
    ,strip.text=element_text(size=rel(1.5))
    ,legend.text=element_text(size=rel(1.5))
    ,axis.text.x=element_text(size=rel(1.5))
    ,axis.title.y=element_text(vjust=.9, size=rel(1.5))
    ,axis.ticks = element_blank()
  )
print(plot2.1)

# pdf("figs/fig2.pdf",width=8,height=6.5)
# plot(plot2.1)
# dev.off()

## Prescriptive
d.corrNorm <- cor(d2.items[d2.items$condition=="immoral" | d2.items$condition1=="irrational",-c(1:2)])

d.corrNorm2 <- as.data.frame.table(d.corrNorm)
d.corrNorm2.S <- d.corrNorm2[c(2:6,15:18,28:30,41:42,54),] # all non-duplicated and non-reflexive *speeded* correlations
d.corrNorm2.R <- d.corrNorm2[c(80:84,93:96,106:108,119:120,132),] # all non-duplicated and non-reflexive *reflective* correlations

var.test(d.corrNorm2.S$Freq,d.corrNorm2.R$Freq)
t.test(d.corrNorm2.S$Freq,d.corrNorm2.R$Freq,paired = T)
cohensD(d.corrNorm2.S$Freq,d.corrNorm2.R$Freq)


d.corrNorm2.S$condition <- "Speeded"
d.corrNorm2.S$pair <- c("Ought - Possibility","Might - Possibility", "Could - Possibility", "May - Possibility", "Should - Possibility",
                        "Might - Ought", "Could - Ought", "May - Ought", "Should - Ought", "Could - Might", "May - Might", "Should - Might",
                        "May - Could","Should - Could", "Should - May")
d.corrNorm2.R$condition <- "Reflective"
d.corrNorm2.R$pair <- c("Ought - Possibility","Might - Possibility", "Could - Possibility", "May - Possibility", "Should - Possibility",
                        "Might - Ought", "Could - Ought", "May - Ought", "Should - Ought", "Could - Might", "May - Might", "Should - Might",
                        "May - Could","Should - Could", "Should - May")

d.corrNorm2 <- rbind(d.corrNorm2.S,d.corrNorm2.R)
d.corrNorm2$condition <- factor(d.corrNorm2$condition, levels=c("Speeded","Reflective"))
d.corrNorm2$pair <- factor(d.corrNorm2$pair, levels = c("Should - Ought","Could - Possibility","May - Might","May - Could",
                                                        "May - Possibility","Should - Might","Could - Might","Should - May","Might - Ought",
                                                        "May - Ought","Might - Possibility","Should - Could","Could - Ought",
                                                        "Should - Possibility","Ought - Possibility"))

d.corrNorm2$type <- "Prescriptive Norms"

## Descriptive 
d.corrDescrip <- cor(d2.items[d2.items$condition1=="improbable" | d2.items$condition1=="impossible",-c(1:2)])

d.corrDescrip2 <- as.data.frame.table(d.corrDescrip)
d.corrDescrip2.S <- d.corrDescrip2[c(2:6,15:18,28:30,41:42,54),] # all non-duplicated and non-reflexive *speeded* correlations
d.corrDescrip2.R <- d.corrDescrip2[c(80:84,93:96,106:108,119:120,132),] # all non-duplicated and non-reflexive *reflective* correlations

var.test(d.corrDescrip2.S$Freq,d.corrDescrip2.R$Freq)
t.test(d.corrDescrip2.S$Freq,d.corrDescrip2.R$Freq,paired = T,var.equal = T)
cohensD(d.corrDescrip2.S$Freq,d.corrDescrip2.R$Freq)


d.corrDescrip2.S$condition <- "Speeded"
d.corrDescrip2.S$pair <- c("Ought - Possibility","Might - Possibility", "Could - Possibility", "May - Possibility", "Should - Possibility",
                           "Might - Ought", "Could - Ought", "May - Ought", "Should - Ought", "Could - Might", "May - Might", "Should - Might",
                           "May - Could","Should - Could", "Should - May")
d.corrDescrip2.R$condition <- "Reflective"
d.corrDescrip2.R$pair <- c("Ought - Possibility","Might - Possibility", "Could - Possibility", "May - Possibility", "Should - Possibility",
                           "Might - Ought", "Could - Ought", "May - Ought", "Should - Ought", "Could - Might", "May - Might", "Should - Might",
                           "May - Could","Should - Could", "Should - May")

d.corrDescrip2 <- rbind(d.corrDescrip2.S,d.corrDescrip2.R)
d.corrDescrip2$condition <- factor(d.corrDescrip2$condition, levels=c("Speeded","Reflective"))

d.corrDescrip2$pair <- factor(d.corrDescrip2$pair, levels = c("Should - Ought","Could - Possibility","May - Might","May - Could",
                                                              "May - Possibility","Should - Might","Could - Might","Should - May","Might - Ought",
                                                              "May - Ought","Might - Possibility","Should - Could","Could - Ought",
                                                              "Should - Possibility","Ought - Possibility"))

d.corrDescrip2$type <- "Descriptive Norms"

d.corr3 <- rbind(d.corrDescrip2,d.corrNorm2)
#d.corr3$type <- factor(d.corr3$type, levels = c("Prescriptive Norm Events","Descriptive Norm Events"))  

plot3 <- ggplot(d.corr3,aes(x=condition,y=Freq)) +
  geom_line(aes(group=pair,color=pair)) +
  geom_point(aes(group=pair,color=pair)) +
  facet_wrap(~type) +
  ylab("Correlation between Modal Judgments") +
  xlab("") +
  #coord_cartesian(ylim=c(0.65,.99)) +
  theme_bw() +
  theme(
    plot.background = element_blank()
    ,panel.grid.major = element_blank()
    ,panel.grid.minor = element_blank()
    ,legend.title=element_blank()
    #,legend.position=c(.89,.5)
    ,strip.text=element_text(size=rel(1.5))
    ,legend.text=element_text(size=rel(1.5))
    ,axis.text.x=element_text(size=rel(1.5))
    ,axis.title.y=element_text(vjust=.9, size=rel(1.5))
    ,axis.ticks = element_blank()
  )
print(plot3)

# pdf("fig3.pdf",width=9,height=7.5)
# plot(plot3)
# dev.off()

aggregate(Freq ~ condition * type, FUN = function(x) c(M = mean(x), SD = sd(x)), data=d.corr3)

lm3.0 <- lmer(Freq ~ condition * type + (1|pair),data=d.corr3)
#interaction
lm3.1 <- lmer(Freq ~ condition + type + (1|pair),data=d.corr3)
anova(lm3.0,lm3.1)

# Descriptive Norm Events
var.test(d.corrDescrip2.S$Freq,d.corrDescrip2.R$Freq)
t.test(d.corrDescrip2.S$Freq,d.corrDescrip2.R$Freq,paired = T,var.equal = T)
cohensD(d.corrDescrip2.S$Freq,d.corrDescrip2.R$Freq)
# Prescriptive Norm Events
var.test(d.corrNorm2.S$Freq,d.corrNorm2.R$Freq)
t.test(d.corrNorm2.S$Freq,d.corrNorm2.R$Freq,paired = T)
cohensD(d.corrNorm2.S$Freq,d.corrNorm2.R$Freq)


## correlation matrix for immoral items 

col2 <- colorRampPalette(c("#67001F", "#B2182B", "#D6604D", "#F4A582", "#FDDBC7",
                           "#FFFFFF", "#D1E5F0", "#92C5DE", "#4393C3", "#2166AC", "#053061"))(200)

d2.itemsReO <- cbind(d2.items[,1:2],
                     d2.items$speeded_possibility,d2.items$speeded_could,d2.items$speeded_might,d2.items$speeded_may,d2.items$speeded_should,d2.items$speeded_ought,
                     d2.items$reflective_possibility,d2.items$reflective_could,d2.items$reflective_might,d2.items$reflective_may,d2.items$reflective_should,d2.items$reflective_ought
)

d.corrM <- cor(d2.itemsReO[d2.itemsReO$condition1=="immoral",-c(1:2)])
fig4 <- corrplot(d.corrM, method="color", na.label = "1", col=col2, cl.lim=c(-.3,1))
## The figure labels were edited to created the figure in the paper

d.corrM2 <-cor(d2.items[d2.items$condition1=="immoral",-c(1:2)])
mean(as.data.frame(d.corrM2)$reflective_possibility[1:6])
mean(as.data.frame(d.corrM2)$reflective_could[1:6])
mean(as.data.frame(d.corrM2)$reflective_might[1:6])
mean(as.data.frame(d.corrM2)$reflective_may[1:6])
mean(as.data.frame(d.corrM2)$reflective_should[1:6])
mean(as.data.frame(d.corrM2)$reflective_ought[1:6])

# pdf("fig4.pdf",width=9,height=8.5)
# plot(fig4)
# dev.off()

d.corrM[d.corrM==1] <- NA
max(d.corrM[1:6,1:6],na.rm = T)
min(d.corrM[1:6,1:6],na.rm = T)

min(d.corrM[7:12,7:12], na.rm = T)
max(d.corrM[7:12,7:12], na.rm = T)

### this looks at deontic, circumstantial and metaphysical modals and asks about similarity under speed and reflection for immoral items
### Reported in the SI
# possible
cor.test(d2.items$speeded_possibility[d2.items$condition1=="immoral"|d2.items$condition1=="irrational"],
         d2.items$reflective_possibility[d2.items$condition1=="immoral"| d2.items$condition1=="irrational"])
# could
cor.test(d2.items$speeded_could[d2.items$condition1=="immoral"| d2.items$condition1=="irrational"],
         d2.items$reflective_could[d2.items$condition1=="immoral"| d2.items$condition1=="irrational"])
# might
cor.test(d2.items$speeded_might[d2.items$condition1=="immoral"| d2.items$condition1=="irrational"],
         d2.items$reflective_might[d2.items$condition1=="immoral"| d2.items$condition1=="irrational"])
# may
cor.test(d2.items$speeded_may[d2.items$condition1=="immoral"| d2.items$condition1=="irrational"],
         d2.items$reflective_may[d2.items$condition1=="immoral"| d2.items$condition1=="irrational"])
# should
cor.test(d2.items$speeded_should[d2.items$condition1=="immoral"| d2.items$condition1=="irrational"],
         d2.items$reflective_should[d2.items$condition1=="immoral"| d2.items$condition1=="irrational"])
# ought
cor.test(d2.items$speeded_ought[d2.items$condition1=="immoral"| d2.items$condition1=="irrational"],
         d2.items$reflective_ought[d2.items$condition1=="immoral"| d2.items$condition1=="irrational"])


#### Modality in high-level cognition ####

# d3 <- read.csv("data/study3force.csv")

## create matrix of average force judgment for each event
d3.sums <- data.frame(cbind(rep(1:144),rep(99,144)))
d3.sums$condition <- rep(c(rep("possible",8),rep(c("impossible","immoral","improbable","irrational"),each=4)),6)

d3.sums[1,2] <- mean(d3$force1[d3$item001==1],na.rm = T)
d3.sums[2,2] <- mean(d3$force1[d3$item002==1],na.rm = T)
d3.sums[3,2] <- mean(d3$force1[d3$item003==1],na.rm = T)
d3.sums[4,2] <- mean(d3$force1[d3$item004==1],na.rm = T)
d3.sums[5,2] <- mean(d3$force1[d3$item005==1],na.rm = T)
d3.sums[6,2] <- mean(d3$force1[d3$item006==1],na.rm = T)
d3.sums[7,2] <- mean(d3$force1[d3$item007==1],na.rm = T)
d3.sums[8,2] <- mean(d3$force1[d3$item008==1],na.rm = T)
d3.sums[9,2] <- mean(d3$force1[d3$item009==1],na.rm = T)
d3.sums[10,2] <- mean(d3$force1[d3$item010==1],na.rm = T)
d3.sums[11,2] <- mean(d3$force1[d3$item011==1],na.rm = T)
d3.sums[12,2] <- mean(d3$force1[d3$item012==1],na.rm = T)
d3.sums[13,2] <- mean(d3$force1[d3$item013==1],na.rm = T)
d3.sums[14,2] <- mean(d3$force1[d3$item014==1],na.rm = T)
d3.sums[15,2] <- mean(d3$force1[d3$item015==1],na.rm = T)
d3.sums[16,2] <- mean(d3$force1[d3$item016==1],na.rm = T)
d3.sums[17,2] <- mean(d3$force1[d3$item017==1],na.rm = T)
d3.sums[18,2] <- mean(d3$force1[d3$item018==1],na.rm = T)
d3.sums[19,2] <- mean(d3$force1[d3$item019==1],na.rm = T)
d3.sums[20,2] <- mean(d3$force1[d3$item020==1],na.rm = T)
d3.sums[21,2] <- mean(d3$force1[d3$item021==1],na.rm = T)
d3.sums[22,2] <- mean(d3$force1[d3$item022==1],na.rm = T)
d3.sums[23,2] <- mean(d3$force1[d3$item023==1],na.rm = T)
d3.sums[24,2] <- mean(d3$force1[d3$item024==1],na.rm = T)

d3.sums[25,2] <- mean(d3$force2[d3$item025==1],na.rm = T)
d3.sums[26,2] <- mean(d3$force2[d3$item026==1],na.rm = T)
d3.sums[27,2] <- mean(d3$force2[d3$item027==1],na.rm = T)
d3.sums[28,2] <- mean(d3$force2[d3$item028==1],na.rm = T)
d3.sums[29,2] <- mean(d3$force2[d3$item029==1],na.rm = T)
d3.sums[30,2] <- mean(d3$force2[d3$item030==1],na.rm = T)
d3.sums[31,2] <- mean(d3$force2[d3$item031==1],na.rm = T)
d3.sums[32,2] <- mean(d3$force2[d3$item032==1],na.rm = T)
d3.sums[33,2] <- mean(d3$force2[d3$item033==1],na.rm = T)
d3.sums[34,2] <- mean(d3$force2[d3$item034==1],na.rm = T)
d3.sums[35,2] <- mean(d3$force2[d3$item035==1],na.rm = T)
d3.sums[36,2] <- mean(d3$force2[d3$item036==1],na.rm = T)
d3.sums[37,2] <- mean(d3$force2[d3$item037==1],na.rm = T)
d3.sums[38,2] <- mean(d3$force2[d3$item038==1],na.rm = T)
d3.sums[39,2] <- mean(d3$force2[d3$item039==1],na.rm = T)
d3.sums[40,2] <- mean(d3$force2[d3$item040==1],na.rm = T)
d3.sums[41,2] <- mean(d3$force2[d3$item041==1],na.rm = T)
d3.sums[42,2] <- mean(d3$force2[d3$item042==1],na.rm = T)
d3.sums[43,2] <- mean(d3$force2[d3$item043==1],na.rm = T)
d3.sums[44,2] <- mean(d3$force2[d3$item044==1],na.rm = T)
d3.sums[45,2] <- mean(d3$force2[d3$item045==1],na.rm = T)
d3.sums[46,2] <- mean(d3$force2[d3$item046==1],na.rm = T)
d3.sums[47,2] <- mean(d3$force2[d3$item047==1],na.rm = T)
d3.sums[48,2] <- mean(d3$force2[d3$item048==1],na.rm = T)

d3.sums[49,2] <- mean(d3$force3[d3$item049==1],na.rm = T)
d3.sums[50,2] <- mean(d3$force3[d3$item050==1],na.rm = T)
d3.sums[51,2] <- mean(d3$force3[d3$item051==1],na.rm = T)
d3.sums[52,2] <- mean(d3$force3[d3$item052==1],na.rm = T)
d3.sums[53,2] <- mean(d3$force3[d3$item053==1],na.rm = T)
d3.sums[54,2] <- mean(d3$force3[d3$item054==1],na.rm = T)
d3.sums[55,2] <- mean(d3$force3[d3$item055==1],na.rm = T)
d3.sums[56,2] <- mean(d3$force3[d3$item056==1],na.rm = T)
d3.sums[57,2] <- mean(d3$force3[d3$item057==1],na.rm = T)
d3.sums[58,2] <- mean(d3$force3[d3$item058==1],na.rm = T)
d3.sums[59,2] <- mean(d3$force3[d3$item059==1],na.rm = T)
d3.sums[60,2] <- mean(d3$force3[d3$item060==1],na.rm = T)
d3.sums[61,2] <- mean(d3$force3[d3$item061==1],na.rm = T)
d3.sums[62,2] <- mean(d3$force3[d3$item062==1],na.rm = T)
d3.sums[63,2] <- mean(d3$force3[d3$item063==1],na.rm = T)
d3.sums[64,2] <- mean(d3$force3[d3$item064==1],na.rm = T)
d3.sums[65,2] <- mean(d3$force3[d3$item065==1],na.rm = T)
d3.sums[66,2] <- mean(d3$force3[d3$item066==1],na.rm = T)
d3.sums[67,2] <- mean(d3$force3[d3$item067==1],na.rm = T)
d3.sums[68,2] <- mean(d3$force3[d3$item068==1],na.rm = T)
d3.sums[69,2] <- mean(d3$force3[d3$item069==1],na.rm = T)
d3.sums[70,2] <- mean(d3$force3[d3$item070==1],na.rm = T)
d3.sums[71,2] <- mean(d3$force3[d3$item071==1],na.rm = T)
d3.sums[72,2] <- mean(d3$force3[d3$item072==1],na.rm = T)

d3.sums[73,2] <- mean(d3$force4[d3$item073==1],na.rm = T)
d3.sums[74,2] <- mean(d3$force4[d3$item074==1],na.rm = T)
d3.sums[75,2] <- mean(d3$force4[d3$item075==1],na.rm = T)
d3.sums[76,2] <- mean(d3$force4[d3$item076==1],na.rm = T)
d3.sums[77,2] <- mean(d3$force4[d3$item077==1],na.rm = T)
d3.sums[78,2] <- mean(d3$force4[d3$item078==1],na.rm = T)
d3.sums[79,2] <- mean(d3$force4[d3$item079==1],na.rm = T)
d3.sums[80,2] <- mean(d3$force4[d3$item080==1],na.rm = T)
d3.sums[81,2] <- mean(d3$force4[d3$item081==1],na.rm = T)
d3.sums[82,2] <- mean(d3$force4[d3$item082==1],na.rm = T)
d3.sums[83,2] <- mean(d3$force4[d3$item083==1],na.rm = T)
d3.sums[84,2] <- mean(d3$force4[d3$item084==1],na.rm = T)
d3.sums[85,2] <- mean(d3$force4[d3$item085==1],na.rm = T)
d3.sums[86,2] <- mean(d3$force4[d3$item086==1],na.rm = T)
d3.sums[87,2] <- mean(d3$force4[d3$item087==1],na.rm = T)
d3.sums[88,2] <- mean(d3$force4[d3$item088==1],na.rm = T)
d3.sums[89,2] <- mean(d3$force4[d3$item089==1],na.rm = T)
d3.sums[90,2] <- mean(d3$force4[d3$item090==1],na.rm = T)
d3.sums[91,2] <- mean(d3$force4[d3$item091==1],na.rm = T)
d3.sums[92,2] <- mean(d3$force4[d3$item092==1],na.rm = T)
d3.sums[93,2] <- mean(d3$force4[d3$item093==1],na.rm = T)
d3.sums[94,2] <- mean(d3$force4[d3$item094==1],na.rm = T)
d3.sums[95,2] <- mean(d3$force4[d3$item095==1],na.rm = T)
d3.sums[96,2] <- mean(d3$force4[d3$item096==1],na.rm = T)

d3.sums[97,2] <- mean(d3$force5[d3$item097==1],na.rm = T)
d3.sums[98,2] <- mean(d3$force5[d3$item098==1],na.rm = T)
d3.sums[99,2] <- mean(d3$force5[d3$item099==1],na.rm = T)
d3.sums[100,2] <- mean(d3$force5[d3$item100==1],na.rm = T)
d3.sums[101,2] <- mean(d3$force5[d3$item101==1],na.rm = T)
d3.sums[102,2] <- mean(d3$force5[d3$item102==1],na.rm = T)
d3.sums[103,2] <- mean(d3$force5[d3$item103==1],na.rm = T)
d3.sums[104,2] <- mean(d3$force5[d3$item104==1],na.rm = T)
d3.sums[105,2] <- mean(d3$force5[d3$item105==1],na.rm = T)
d3.sums[106,2] <- mean(d3$force5[d3$item106==1],na.rm = T)
d3.sums[107,2] <- mean(d3$force5[d3$item107==1],na.rm = T)
d3.sums[108,2] <- mean(d3$force5[d3$item108==1],na.rm = T)
d3.sums[109,2] <- mean(d3$force5[d3$item109==1],na.rm = T)
d3.sums[110,2] <- mean(d3$force5[d3$item110==1],na.rm = T)
d3.sums[111,2] <- mean(d3$force5[d3$item111==1],na.rm = T)
d3.sums[112,2] <- mean(d3$force5[d3$item112==1],na.rm = T)
d3.sums[113,2] <- mean(d3$force5[d3$item113==1],na.rm = T)
d3.sums[114,2] <- mean(d3$force5[d3$item114==1],na.rm = T)
d3.sums[115,2] <- mean(d3$force5[d3$item115==1],na.rm = T)
d3.sums[116,2] <- mean(d3$force5[d3$item116==1],na.rm = T)
d3.sums[117,2] <- mean(d3$force5[d3$item117==1],na.rm = T)
d3.sums[118,2] <- mean(d3$force5[d3$item118==1],na.rm = T)
d3.sums[119,2] <- mean(d3$force5[d3$item119==1],na.rm = T)
d3.sums[120,2] <- mean(d3$force5[d3$item120==1],na.rm = T)

d3.sums[121,2] <- mean(d3$force6[d3$item121==1],na.rm = T)
d3.sums[122,2] <- mean(d3$force6[d3$item122==1],na.rm = T)
d3.sums[123,2] <- mean(d3$force6[d3$item123==1],na.rm = T)
d3.sums[124,2] <- mean(d3$force6[d3$item124==1],na.rm = T)
d3.sums[125,2] <- mean(d3$force6[d3$item125==1],na.rm = T)
d3.sums[126,2] <- mean(d3$force6[d3$item126==1],na.rm = T)
d3.sums[127,2] <- mean(d3$force6[d3$item127==1],na.rm = T)
d3.sums[128,2] <- mean(d3$force6[d3$item128==1],na.rm = T)
d3.sums[129,2] <- mean(d3$force6[d3$item129==1],na.rm = T)
d3.sums[130,2] <- mean(d3$force6[d3$item130==1],na.rm = T)
d3.sums[131,2] <- mean(d3$force6[d3$item131==1],na.rm = T)
d3.sums[132,2] <- mean(d3$force6[d3$item132==1],na.rm = T)
d3.sums[133,2] <- mean(d3$force6[d3$item133==1],na.rm = T)
d3.sums[134,2] <- mean(d3$force6[d3$item134==1],na.rm = T)
d3.sums[135,2] <- mean(d3$force6[d3$item135==1],na.rm = T)
d3.sums[136,2] <- mean(d3$force6[d3$item136==1],na.rm = T)
d3.sums[137,2] <- mean(d3$force6[d3$item137==1],na.rm = T)
d3.sums[138,2] <- mean(d3$force6[d3$item138==1],na.rm = T)
d3.sums[139,2] <- mean(d3$force6[d3$item139==1],na.rm = T)
d3.sums[140,2] <- mean(d3$force6[d3$item140==1],na.rm = T)
d3.sums[141,2] <- mean(d3$force6[d3$item141==1],na.rm = T)
d3.sums[142,2] <- mean(d3$force6[d3$item142==1],na.rm = T)
d3.sums[143,2] <- mean(d3$force6[d3$item143==1],na.rm = T)
d3.sums[144,2] <- mean(d3$force6[d3$item144==1],na.rm = T)

colnames(d3.sums) <- c("item","force","condition")

d3.sums$scenario <- rep(c("scenario1","scenario2","scenario3","scenario4","scenario5","scenario6"),each=24)

d1.sum2 <- ddply(d1a, c("condition1","condition2","condition3","trialNo"), summarise,
                 mean = mean(responses, na.rm=TRUE))

#assign force judgments to each possibility
for (i in 1:144){
  d1.sum2$force[d1.sum2$trialNo==i] <- d3.sums$force[d3.sums$item==i] 
}

d1.sum2$subjectGroup <- factor(c("Reflective","Speeded")[d1.sum2$condition3])

d3.sum <- cbind(d1.sum2[d1.sum2$subjectGroup=="Reflective",c(1:2,4:6)],d1.sum2[d1.sum2$subjectGroup=="Speeded",5])
names(d3.sum)[c(4,6)] <- c("reflectPoss","speededPoss")

## simple test of default representation
anova(lm(force ~ speededPoss, data=d3.sum))
etaSquared(lm(force ~ speededPoss, data=d3.sum))

# comparison of speeded and reflective
lm3.0 <- lmer(force ~ reflectPoss + speededPoss + (1|condition2), data=d3.sum)
## test of explicit representation of possibility
lm3.1 <- lmer(force ~ speededPoss + (1|condition2), data=d3.sum)
anova(lm3.0,lm3.1)
## test of implicit representation of possibility
lm3.2 <- lmer(force ~ reflectPoss + (1|condition2), data=d3.sum)
anova(lm3.0,lm3.2)

## effect of morality
var.test(d3.sum$force[d3.sum$condition1=="immoral"],d3.sum$force[d3.sum$condition1=="possible"])
t.test(d3.sum$force[d3.sum$condition1=="immoral"],d3.sum$force[d3.sum$condition1=="possible"],var.equal = T)
cohensD(d3.sum$force[d3.sum$condition1=="immoral"],d3.sum$force[d3.sum$condition1=="possible"])

#mediation analysis
b.1 <- lm(speededPoss ~ condition1 , data=d3.sum[d3.sum$condition1!="impossible",])
c.1 <- lm(force ~ speededPoss + condition1, data=d3.sum[d3.sum$condition1!="impossible",])
summary(mediate(b.1, c.1, sims=1000, treat="condition1", mediator="speededPoss", control.value = "possible", treat.value = "immoral"))

## effect of rationality
var.test(d3.sum$force[d3.sum$condition1=="irrational"],d3.sum$force[d3.sum$condition1=="possible"])
t.test(d3.sum$force[d3.sum$condition1=="irrational"],d3.sum$force[d3.sum$condition1=="possible"],var.equal = T)
cohensD(d3.sum$force[d3.sum$condition1=="irrational"],d3.sum$force[d3.sum$condition1=="possible"])
summary(mediate(b.1, c.1, sims=1000, treat="condition1", mediator="speededPoss", control.value = "possible", treat.value = "irrational"))


### Desire study ####

substrRight <- function(x, n){
  substr(x, nchar(x)-n+1, nchar(x))
}

items <- c(14:33,39:58,64:83,89:108,114:133,139:158) #columns with response data
d3b.items <- gather(d3b, item, want, items, na.rm=T) 
d3b.items$want[d3b.items$item=="item002"] <- d3b.items$want[d3b.items$item=="item002"]-7 #this fixes the error in the qualtrics coding
d3b.items$want <- d3b.items$want -14
d3b.items$item <- as.numeric(substrRight(d3b.items$item,3))
d3b.items <- aggregate(want~item, FUN=mean,data=d3b.items)

b.items <- d3b.items$item
d3.sum$want <- NA
for (i in b.items){
  d3.sum$want[d3.sum$trialNo==i] <- d3b.items$want[d3b.items$item==i] 
}

anova(lm(want~condition1,data=d3.sum[d3.sum$condition1!="impossible",]))
etaSquared(aov(lm(want~condition1,data=d3.sum[d3.sum$condition1!="impossible",])))

aggregate(want~condition1,FUN=function(x) c(M = mean(x),SD = sd(x)),data=d3.sum[d3.sum$condition1!="impossible",])
# immoral vs. ordinary
var.test(d3.sum$want[d3.sum$condition1=="immoral"],d3.sum$want[d3.sum$condition1=="possible"])
t.test(d3.sum$want[d3.sum$condition1=="immoral"],d3.sum$want[d3.sum$condition1=="possible"])
cohensD(d3.sum$want[d3.sum$condition1=="immoral"],d3.sum$want[d3.sum$condition1=="possible"])

# irrational vs. ordinary
var.test(d3.sum$want[d3.sum$condition1=="irrational"],d3.sum$want[d3.sum$condition1=="possible"])
t.test(d3.sum$want[d3.sum$condition1=="irrational"],d3.sum$want[d3.sum$condition1=="possible"])
cohensD(d3.sum$want[d3.sum$condition1=="irrational"],d3.sum$want[d3.sum$condition1=="possible"])

# improbable vs. ordinary
var.test(d3.sum$want[d3.sum$condition1=="improbable"],d3.sum$want[d3.sum$condition1=="possible"])
t.test(d3.sum$want[d3.sum$condition1=="improbable"],d3.sum$want[d3.sum$condition1=="possible"],var.equal = T)
cohensD(d3.sum$want[d3.sum$condition1=="improbable"],d3.sum$want[d3.sum$condition1=="possible"])


#overall model
lm3.3 <- lmer(force ~ want + speededPoss + (1|condition2), data=d3.sum[d3.sum$condition1!="impossible",])
## test of wanting 
lm3.4 <- lmer(force ~ speededPoss + (1|condition2), data=d3.sum[d3.sum$condition1!="impossible",])
anova(lm3.3,lm3.4)
## test of implicit representation of possibility
lm3.5 <- lmer(force ~ want + (1|condition2), data=d3.sum[d3.sum$condition1!="impossible",])
anova(lm3.3,lm3.5)

d3.sum$condition2 <- factor(c("Context 1","Context 2","Context 3","Context 4","Context 5","Context 6")[d3.sum$condition2])

figS3 <- ggplot(d3.sum[d3.sum$condition1!="impossible",], aes(x=speededPoss, y=want, label=trialNo)) +
  geom_point(aes(color=condition2),stat = "identity", position = "jitter") +
  #geom_text(stat = "identity", position = "jitter") +
  facet_wrap(~condition2) +
  stat_smooth(formula = y ~ x, method=lm) +
  #stat_smooth(aes(y=force, x=reflectPoss), formula=y~x, method=lm, se=FALSE, color="black", linetype=2 ) +
  #scale_color_manual(values=c("black","black")) + 
  theme_bw() +
  xlab("Implicit Representation of Impossibility") +
  ylab("Judgment That Agent Desired Alternative") + 
  theme(
    plot.background = element_blank()
    ,panel.grid.major = element_blank()
    ,panel.grid.minor = element_blank()
    ,legend.position="null"
    ,axis.text=element_text(size=rel(1))
    ,strip.text=element_text(size=rel(1.5))
    ,axis.title.y=element_text(vjust=.95)
    ,axis.ticks = element_blank()
    ,axis.title=element_text(size=rel(1.5))
  )

print(figS3)

#mediation analysis for desire rather than possibility
d.1 <- lm(want ~ condition1 , data=d3.sum[d3.sum$condition1!="impossible",])
e.1 <- lm(force ~ want + condition1, data=d3.sum[d3.sum$condition1!="impossible",])
#morality
summary(mediate(d.1, e.1, sims=1000, treat="condition1", mediator="want", control.value = "possible", treat.value = "immoral"))
#irrationality
summary(mediate(d.1, e.1, sims=1000, treat="condition1", mediator="want", control.value = "possible", treat.value = "irrational"))


### END ###
