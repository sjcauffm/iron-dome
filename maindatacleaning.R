#####Cleaning Main Task Data####
library(tidyr)
library(dplyr)
library(janitor)

####need to read in the CSVs from the folder. 
mainwd <- setwd("/Volumes/GoogleDrive/My Drive/Projects/Iron Dome/Data/main")

files <- list.files(mainwd)

#testing a read.csv to see which rows need to be removed. 
test <- read.csv("P001_IDmain_2018_Apr_20_1245.csv")

####Going to read all files together and then grep for practice trials. 
maindata <- do.call(rbind, lapply(files, read.csv))

prac <- grep("Practice", maindata$Stimulus)

block <- c(rep(1, 48), rep(2, 48))

maindata$block <- c(rep(block, 39))

maindata <- maindata[,c(1:2,17:18,23)] ##subset columns of interest. 
maindata <- drop_na(maindata) ##practice values will only have NA so we drop them.                 
maindata <- maindata[-prac,]

maindata <- remove_empty(maindata, "rows")

blank <- grep(" ", maindata$Stimulus)
maindata <- maindata[blank,]

#Need to change one participant name
maindata$participant <- as.character(maindata$participant)

for (i in 1:length(maindata$participant)){
  if (maindata$participant[i] == "ID01"){
    maindata$participant[i] <- "P001"
  } else if(maindata$participant[i] == "p006"){
    maindata$participant[i] <- "P006"
  } else if(maindata$participant[i] == "p008"){
    maindata$participant[i] <- "P008"
  }
}

####Creating an empty column name for horizontal and vertical rotation
maindata$horizontal <- 0
maindata$vertical <- 0

###converting stimulus and response columns to character just in case
maindata$Stimulus <- as.character(maindata$Stimulus)
test <- maindata$stim
###splitting the Stimulus column in order to get the variable values. 
x <- maindata$Stimulus ### creating a test vector
x <- as.character(x)

x <- unlist(strsplit(x ,"/"))

test_slice <- maindata$Stimulus[1:4]

##Creating a function to extract the variable information from the image file names. 
extract_variables_from_image <- function(strings) { ##creates function, assigns arguments used
  strings_vector <- sapply(strings, function(x) { ##creates a vector of character strings
    temp <- unlist(strsplit(x,'/')) ##sapplies anonymous funciton to list of image names, splits by "/'
    temp <- temp[length(temp)] ##takes the last value from Temp that is split by a "/"
    return(temp) ## returns a weirdly shaped list of the string with the character vectors we want
  })
  target_strings <- lapply(strings_vector, function(x) { ##applies anonymous function to the strings
                                                         ## ector (aka temp output)
    temp <- unlist(strsplit(x,'_')) ##takes the strings veector and unlists again by "_"
    output <- temp[1:length(temp)-1] ##Takes all values of temp except the last one
    return(output) ## shows the output of the variable values isolated. 
  })
  output <- as.data.frame(t(do.call('cbind',target_strings))) ##binds together each set of values from each row
  rownames(output) = NULL ##gets rid of those bitchass column names
  colnames(output) <- c("horizontal", "vertical", "targets", "samediff")## assigns row names before transpose
  ##output <- apply(output, 2, as.factor) ##converts each row of output to factor data type because miichael is a butt 
  return(output)##gives the isolated variable values as a matrix
}

##Testing the values
test <- extract_variables_from_image(maindata$Stimulus)
head(test)
str(test)

###binding the output dataframe to the main dataframe
maindata <- cbind(maindata, test)

maindata <- maindata[,c(1:6, 9:12)]

save(maindata, file = "/Volumes/GoogleDrive/My Drive/Projects/Iron Dome/maindata.rda")
                    










