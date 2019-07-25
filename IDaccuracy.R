#####Accuracy Analysis#####
library(lme4)
library(ggplot2)
library(dplyr)
library(tidyr)
library(mosaic)

setwd("~/Google Drive File Stream/My Drive/Projects/Iron Dome/Data")
load("~/Google Drive File Stream/My Drive/Projects/Iron Dome/Data/maindata.rda")

maindata$stdhoriz <- zscore(maindata$horiz)
maindata$stdvert <- zscore(maindata$vert)

hormeans <- aggregate(maindata$correct, by = list(maindata$rotlength, maindata$block,maindata$targets), 
                      FUN = "mean")
vertmeans <- aggregate(maindata$correct, by = list(maindata$vert), FUN = "mean")
rotmeans <- aggregate(maindata$correct, by = list(maindata$rot_total, maindata$targets), FUN = "mean")

####Null for Accuracy####
accuracy.null <- glmer(correct ~ 1 + (1|participant), data = maindata, family = binomial, 
                       control = glmerControl(optimizer = "bobyqa"), nAGQ = 10)
summary(accuracy.null)

####Model 1 for Accuracy####
accuracy.1 <- glmer(correct ~ targets*as.factor(block)
                    + (as.factor(block)|participant), 
                    data = maindata, family = binomial, 
                    control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000)),
                    nAGQ = 1, REML = FALSE)
summary(accuracy.1)

####Model 2 for Accuracy####
accuracy.2 <- glmer(correct ~ rotlength + targets + as.factor(block) + stdmr + stdsport +
                    (rotlength|participant),
                    data = maindata, family = binomial, 
                    control = glmerControl(optimizer = "bobyqa"),
                    nAGQ = 1, REML = FALSE)
summary(accuracy.2)


####Graphing Accuracy
colnames(hormeans) <- c("rotlength", "block", "targets", "accuracy")

accplot <- ggplot(data = hormeans, aes(x = rotlength, y = accuracy, group = as.factor(block), 
                                       color = as.factor(block))) + 
                      geom_line(stat = "identity") + geom_point() + facet_grid(.~targets)


              







