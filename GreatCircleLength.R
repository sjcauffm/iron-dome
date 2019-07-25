####Computing Great Circle Distance for Rotation####

library(dplyr)
library(tidyr)
library(REdaS)

setwd("~/Google Drive File Stream/My Drive/Projects/Iron Dome")
load("~/Google Drive File Stream/My Drive/Projects/Iron Dome/Data/maindata.rda")

##weird extra columns appeared
maindata <- maindata[,c(1:11,14:15)]

#####Have to convert degree values to radians
maindata$horrad <- deg2rad(as.numeric(maindata$horizontal))
maindata$vertrad <- deg2rad(as.numeric(maindata$vertical))


x <- 3
####calculating the central angle
for (i in 1:length(maindata$vertical)){
  if(as.numeric(maindata$vertical[i]) == 0){
    maindata$rotlength[i] <- 2 * pi * 0.24 * maindata$horizontal[i]/360
  } else if (as.numeric(maindata$vertical[i]) > 0){
   x[i] <- acos(sin(0)*sin(maindata$horrad[i]) + 
            cos(0)*cos(maindata$vertrad[i])*cos(maindata$vertrad[i]))
    maindata$rotlength[i] <- (0.24 * x[i])
  }
}

save(maindata, file = "maindata.rda")
