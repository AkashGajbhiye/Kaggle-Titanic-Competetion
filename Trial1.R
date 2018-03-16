#############################################################
#   INITALIZATION
#############################################################

setwd("C:/Users/AKASH/Documents/DS/Titanic")
trainData <- read.csv("train.csv", stringsAsFactors = FALSE)
testData <- read.csv("test.csv", stringsAsFactors = FALSE)




############################################################
# SUMMARIZING DATA
############################################################

library(dplyr)

summary(trainData)
summary(testData)




############################################################
# CREATING DISTINCTION BETWEEN TRAIN AND TEST DATA
############################################################

testData$Survived <- NA
testData$IsTrainData <- FALSE
trainData$IsTrainData <- TRUE





############################################################
# JOIN THE DATA
############################################################

DF <- rbind(trainData,testData)




###########################################################
# CLEANING THE MISSING VALUES
###########################################################

# Replace Embarked with Mode
DF[DF$Embarked == "", "Embarked"] <- 'S'

# Replace Age with mean of ages of similar people 
for (i in 1:dim(DF[1])) {
  if (is.na(DF[i,"Age"])) {
    zx <- DF %>% filter(Pclass == DF[i,"Pclass"] & Survived == DF[i,"Survived"] & Sex == DF[i,"Sex"])
    DF[i,"Age"] <- mean(zx$Age, na.rm = TRUE)
    
  }
}


for (i in 1:dim(DF[1])) {
  if (is.na(DF[i,"Age"])) {
    zx <- DF %>% filter(Pclass == DF[i,"Pclass"] & Sex == DF[i,"Sex"])
    DF[i,"Age"] <- mean(zx$Age, na.rm = TRUE)
    
  }
}

# Replace Fare with mean fares
meanFare <- mean(DF$Fare, na.rm = TRUE)
DF[is.na(DF$Fare), "Fare"] <- meanFare






#######################################################################
#   BACK DATA CONVERSION
#######################################################################

DF$PassengerId <- as.factor(DF$PassengerId)
DF$Survived <- as.factor(DF$Survived)
DF$Sex <- as.factor(DF$Sex)
DF$Embarked <- as.factor(DF$Embarked)






############################################################
# SPLIT DATA INTO TEST AND TRAIN
############################################################

newTestData <- DF[DF$IsTrainData == FALSE, ]
newTrainData <- DF[DF$IsTrainData == TRUE, ]
newTrainData$Survived <- as.factor(newTrainData$Survived)




###########################################################
# MODEL TRAINING
###########################################################

survi <- "Survived ~ Pclass + Age + Sex + SibSp + Parch + Embarked"
SurvFomrula <- as.formula(survi)

library(randomForest)

Modol <- randomForest(formula = SurvFomrula, data = newTrainData)

featr <- "Pclass + Age + Sex + SibSp + Parch + Embarked"

Survvd <- predict(Modol, newdata = newTestData)




###################################################################
# WRITING OUTPUT
####################################################################

PassengerId <- newTestData$PassengerId
opt.df <- as.data.frame(PassengerId)
opt.df$Survived <- Survvd

write.csv(opt.df, file = "Kaggle_Submission.csv", row.names = FALSE)