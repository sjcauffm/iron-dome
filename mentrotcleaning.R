library(tidyr)

#### Cleaning Mental Rotation Data####

mentrot <- setwd("/Volumes/GoogleDrive/My Drive/Projects/Iron Dome/Data/mentrot")

MR <- list.files(mentrot) 

####getting all of the files for mental rotation data####

mrdata <- do.call(rbind, lapply(MR, read.csv))
mrdata <- mrdata[,c(1:8,10:16)]
mrdata$image <- as.character(mrdata$image)

####removing keyboard responses####

mrdata <- drop_na(mrdata)


####Making sure participant names matchup
mrdata$participant <- as.character(mrdata$participant)

for (i in 1:length(mrdata$participant)){
  if (mrdata$participant[i] == "ID01"){
    mrdata$participant[i] <- "P001"
  }
}

for (i in 1:length(mrdata$participant)){
  if (mrdata$participant[i] == "p002"){
    mrdata$participant[i] <- "P002"
  }
}

####Creating correct vs incorrect columns

Mirror <- grep("Lreverse", mrdata$image) ##Creates vector of indices where image is mirror
mrdata$is.same <- TRUE ## Initiates new column with TRUE as default
mrdata$is.same[Mirror] <- FALSE ##Changes to true if mirror along the Mirror vector

#Splitting based on correct or incorrect responses
mrdata$correct <- FALSE
##For loop assigns TRUE or FALSE to correct cells. 
for(i in 1:nrow(mrdata)){
  if((mrdata$key_resp_3.keys[i] == "f" & mrdata$is.same[i] == TRUE) || 
     (mrdata$key_resp_3.keys[i] == "j" & mrdata$is.same[i] == FALSE)){
    mrdata$correct[i] <- TRUE
  }else{mrdata$correct[i] <- FALSE}
}

###total mean of correct responses
mean(mrdata$correct)

###mean of correct responses by participant
propcorrect <- aggregate(mrdata$correct, by = list((as.factor(mrdata$participant))), 
                         FUN = mean)

pcsd <- sd(propcorrect$x)
pcm <- mean(propcorrect$x)

####Saving the MR data set as a .rda####
save(mrdata, file = "/Volumes/GoogleDrive/My Drive/Projects/Iron Dome/Data/mentrot.rda")




