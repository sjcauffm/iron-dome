###Scoring Sport

library(tidyr)
library(dplyr)

setwd("H:/My Drive/Projects/Iron Dome/Data") ## if windows
setwd("~/Google Drive File Stream/My Drive/Projects/Iron Dome/Data") ## if Mac

load("sportresponses.rda")
load("~/Google Drive File Stream/My Drive/Projects/Iron Dome/maindata.rda")

responses$response <- as.numeric(as.character(responses$response))

responses$error <- (responses$Key - responses$response)

sporterror <- data.frame(Participant = c(levels(responses$Participant)))

score <- aggregate(responses$error, by = list(responses$Participant), FUN = "mean")

colnames(score) <- c("participant", "score")

maindata$sport <- 3

for (i in 1:length(score$participant)){
  maindata$sport[which(maindata$participant == score$participant[i])] <- score$score[i]
}

save(maindata, file ="maindata.rda")


