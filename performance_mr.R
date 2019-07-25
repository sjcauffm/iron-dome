####Mental Rotation Analysis####
library(lme4)
##need to get performance variable from mrdata

##load in file
load("/Volumes/GoogleDrive/My Drive/Projects/Iron Dome/Data/mentrot.rda")

####conducting multiple linear models per participant
mrcor <- mrdata[which(mrdata$correct == TRUE),]

mranal <- lmList(key_resp_3.rt ~ abs(rot-180) | participant, data = mrcor)
summary(mranal)

mrcoeff <- coefficients(mranal)

mrcoeff$participant <- rownames(mrcoeff) 
colnames(mrcoeff) <- c("rot", "participant")

mrcoeff <- mrcoeff[,2:3]
mrcoeff <- mrcoeff[c(1:2, 4:23, 25:38),]

####adding it into the main data file
maindata <- maindata[,c(1:5, 8:12)]
maindata$mrperf <- 3

pvec <- levels(as.factor(maindata$participant))

freddy <- length(as.factor(maindata$mrperf))

for (i in 1:length(mrcoeff$participant)){
  maindata$mrperf[which(maindata$participant == mrcoeff$participant[i])] <- mrcoeff$rot[i]
}

maindata$mrperf <- lapply(maindata$mrperf, mrbind(maindata, mrcoeff))

save(maindata, file ="maindata.rda")







