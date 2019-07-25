####Data Analysis Script for Iron Dome
library(lme4)
library(dplyr)
library(tidyr)
library(ggpubr)
library(fitdistrplus)
library(car)
library(MuMIn)
library(sjPlot)
library(sjstats)
library(ggplot2)
setwd("~/Google Drive File Stream/My Drive/Projects/Iron Dome/Data")
load("~/Google Drive File Stream/My Drive/Projects/Iron Dome/Data/maindata.rda")

colnames(maindata) <- c("stim", "resp", "respkey", "resptime", "participant", "block", "horiz", "vert",
                        "targets","samediff","sport", "mrperf", "horrad", "vertrad", "rotlength")

###making a last change to the data set to correct a target typo
maindata$targets <- as.character(maindata$targets)

for (i in 1:length(maindata$targets)){
  maindata$targets[which(maindata$targets == "3t")] <- "3T"
}

###making the correct/incorrect column
for (i in 1:length(maindata$respkey)){
  if (maindata$respkey[i] == "f" && maindata$samediff[i] == "Same"){
    maindata$correct[i] <- TRUE
  } else if (maindata$respkey[i] == "j" && maindata$samediff[i] == "Diff"){
    maindata$correct[i] <- TRUE
  } else{
    maindata$correct[i] <- FALSE
  }
}

## making a magnitude of rotation value
maindata$rot_total <- (as.numeric(as.character(maindata$horiz)) + as.numeric(as.character(maindata$vert)))
## making rotation values numeric to make life easier
maindata$horiz <- as.numeric(as.character(maindata$horiz))
maindata$vert <- as.numeric(as.character(maindata$vert))
save(maindata, file = "maindata.rda")

###remove NA values
maindata <- drop_na(maindata)

####standardizing mrperf and sport
maindata$stdmr <- zscore(maindata$mrperf)
maindata$stdsport <- zscore(maindata$sport)

####Response Time####
##looking at the mean response times for horizontal and veritcal rotation
hormeansrt <- aggregate(maincor$resptime, by = list(maindata$horiz, maindata$vert), FUN = "mean")
vertmeans <- aggregate(maindata$resptime, by = list(maindata$vert), FUN = "mean")
blockmeans <- aggregate(maincor$resptime, by = (list(maincor$targets, maincor$rotlength, maincor$block)),
                        FUN = "mean")

colnames(blockmeans) <- c("Targets", "Rotation", "Block", "RT")

##means for target density
targetmeans <- aggregate(maindata$resptime, by = list(maindata$targets), FUN = "mean")

###Checking the distribution of the response time data
maincor <- maindata[which(maindata$correct == TRUE),]

dist <- descdist(maincor$resptime, boot = 500,000) ### generates cullen frey graph, data are not normal

#plotting data against a normal distribution
normplot <- ggqqplot(maincor$resptime)
norm <- fitdist(maincor$resptime, "norm")
plotnorm <- plot(norm)
#definitely not normal

#plotting data against a log normal distribution
lognorm <- fitdist(maincor$resptime, "lnorm")
plotlnorm <- plot(lognorm)

#definitely log normal most likely
maincor$resplog <- log(maincor$resptime)

#null model and ICC for response time
rtnull <- lmer(resplog ~ (1|participant), data = maincor)
summary(rtnull)

icc_null <- icc(rtnull)
#26% of the total variance in response time is between participant, 74% within participant. 

#level 1 model for response time
rt.model1 <- lmer(resplog ~ rotlength + targets + block + (1|participant), 
                  data = maincor)
summary(rt.model1)
aovmodel1 <- Anova(rt.model1)
aovmodel1
r.squaredGLMM(rt.model1)

rt.model2 <- lmer(resplog ~ rotlength + targets + block + rotlength*targets + rotlength*block + 
                    (1|participant), data = maincor) 
summary(rt.model2)
aovmodel2 <- Anova(rt.model2)
aovmodel2
r.squaredGLMM(rt.model2)

#adding in the sport and mr measures
rtmodel3 <- lmer(resplog ~ rotlength + targets + block + stdmr*stdsport +
                   (1|participant), data = maincor)
summary(rtmodel3)
aovmodel3 <- Anova(rtmodel3)
aovmodel3
r.squaredGLMM(rtmodel3)

#####graphing#####
colnames(hormeansrt) <- c("hor", "vert", "rt")
rotationplot <- ggplot(blockmeans, aes(x = Rotation, y = RT, group = as.factor(Block), 
                                       color = as.factor(Block))) + 
                  geom_line(stat = "identity") +
                  geom_point() + facet_grid(.~Targets) 


spatialplot <- ggplot(maindata, aes(x = mrperf, y = sport)) +
                  geom_point()






