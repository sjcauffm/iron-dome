####Cleaning Spatial Orientation Data####

library(tidyr)
library(dplyr)

####Need to import the .csv####
setwd("~/Google Drive File Stream/My Drive/Projects/Iron Dome/Data") ## if it wd isnt already set. 
sport <- read.csv("sportdata.csv")

sample <- t(sport)
sample <- as.data.frame(sample)
sample[] <- lapply(sample, as.character)
colnames(sample) <- sample[1, ]
sample <- sample[-1 ,]

####isolate 1 row to test solutions####

testrow <- sport[1:2,]

####need to find the cell with each "On" state. 
questions <- strsplit(rownames(sample), "...")
questions <-unlist(questions)
colnames(questions) = c("question", "response")

sample <- cbind(sample, questions) ###binds the information together

sample$question <- c(rep("test", 20), rep(1, 20), rep(2, 20), rep(3, 20), rep(4, 20), rep(5, 20),
                     rep(6, 20), rep(7, 20), rep(8, 20), rep(9, 20), rep(10, 20), rep(11, 20), 
                     rep(12,20))

sample$response <- c(rep(c(15,45,60,75,90,105,130,150,165,180,240,195,225,255,270,285, 315,330,345,0),13))

save(sample, file = "sportdata.rda")

##converting from wide to long ya dingus
test <- gather(sample, key = colnames(sample), question)

#gather is dumb so you subset to get the values for the potential responses. 
x <- test[10401:10660,]

test <- test[1:10400,]

#adding in the responses for each participant

test$response <- rep(x$question, 40)
test$Q <- rep(c(rep("test", 20), rep(1, 20), rep(2, 20), rep(3, 20), rep(4, 20), rep(5, 20),
                         rep(6, 20), rep(7, 20), rep(8, 20), rep(9, 20), rep(10, 20), rep(11, 20), 
                         rep(12,20)), 40)

colnames(test) <- c("participant", "Response", "Answer", "Question")

save(test, file = "sportdata.rda")

test <- as.data.frame(test)
test$participant <- as.factor(test$participant)
test$Question <- as.factor(test$Question)
test$Answer <- as.factor(test$Answer)
test$Question <- na_if(test$Question, "test")
test <- na.omit(test)

#####Need to test for duplicate responses
test$participant <- as.character(test$participant)

for (i in 1:length(test$Response)){
  if (test$Response[i] == "On"){
    test$Response[i] <- TRUE
  } else if (test$Response[i] == "Off"){
    test$Response[i] <- FALSE
  }
}

test$Response <- as.logical(test$Response)

####Create an empty data frame to store the response value

responses <- data.frame(Participant = rep(levels(as.factor(test$participant)), 12))

responses <- arrange(responses, Participant)
responses$Participant <- as.factor(responses$Participant)

responses$Question <- rep(c(1:12), 40)
responses$Question <- as.factor(responses$Question)

get_responses <- function (data, target){
  responses$response <- if (test$Question == responses$Question && test$participant == responses$Participant){
      responses$response <- test$Answer[which(test$Response == TRUE)]}
}

x <- lapply(responses$response, get_responses(test,responses))

test$Response <- as.character(test$Response)

responses <- responses[,c(1:2, 4)]

responses$Key <- rep(c(125, 235, 85, 155, 319, 235, 335, 260, 280, 50, 25, 150), 40)

###participant 11 needs to be removed
responses <- responses[c(1:120, 133:480),]

save(responses, file = "sportresponses.rda")




  
 



