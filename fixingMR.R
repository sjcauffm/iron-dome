load("~/Google Drive File Stream/My Drive/Projects/Iron Dome/Data/maindata.rda")

for (i in 1:length(maindata$mrperf)){
  if (maindata$mrperf[i] == 3){
    maindata$mrperf[i] <- NA
  }
}

maindata <- drop_na(maindata)

save(maindata, file = "maindata.rda")
