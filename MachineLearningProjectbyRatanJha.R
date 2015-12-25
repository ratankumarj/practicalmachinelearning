#Machine Learning Project Work
#Author Ratan Jha
#First set directory to downloaded folder
# Source of this data is "http://groupware.les.inf.puc-rio.br/har"
#http://groupware.les.inf.puc-rio.br/har#sbia_paper_section
# Download and save the file in a working directory
setwd("~/RforRatan/Machine Learning")
# read training and test data
maindata <- read.csv("pml-training.csv")
#set seed fro repeatable results
set.seed(32343)
#load caret library
library(caret)
library(randomForest)
library(e1071)
# Upon inspecting the data, we find that first 5 columns which contains
# use name, time stamp, window fields which is not a predictor variable
# first let us remove the first 5 fields
sourcedata <- maindata [,6:160]
# further investigation shows that data has blanks, NA and "#Div/o!"
# Let us remove these values with "NA"
sourcedata[sourcedata == ""] <- "NA"
sourcedata[sourcedata == "#DIV/0!"]  <- "NA"
# Only keep columns that have less than 40% NA in the columns
sourcedata <-
  sourcedata[, colSums(is.na(sourcedata)) < nrow(sourcedata) * 0.40]
#sourcedata <- sapply(sourcedata,as.numeric)
sourcedata$classe <- maindata$classe
write.csv(sourcedata,"InterimResults.csv")
#Split the training data into training and test set (75 to 25 ratio - since the sample size is large)
inTrain <- createDataPartition(sourcedata$classe,p = 0.75,list = FALSE)
training <- sourcedata[inTrain,]
testing <- sourcedata[-inTrain,]
# Set the class as a factor for analysis
training$classe <- as.factor(as.character(training$classe))
testing$classe <- as.factor(as.character(testing$classe))
# used randomForest function instead of train function as it happened to be many times faster
RFModel <- randomForest(classe ~ .,data = training)
RFR <- predict(RFModel,testing)
RFC <- confusionMatrix(RFR,testing$classe)$overall[1]
# Check Accuracy
print(RFC)
#random forest provided 99.6% accurate model while rpart gave only 49.8%. (removed code for rpart)
#Hence we would leverage random forest for predicting
#Read test Data
testdata <- read.csv("pml-testing.csv")
newtestdata <- training
#Only keep columns that are present in trainign data
newtestdata <- testdata[,names(testdata) %in% names(training)]
#since test data doesn't have classe column defined, we will add the columne
newtestdata$classe <- training$classe[1:1]
levels(newtestdata) <- levels(training)
#This code is to ensure that new_window has 2 factor variable as the training data has two values
levels(newtestdata$new_window) <- c("yes","no")
RPartFinalResult <- predict(RFModel,newtestdata)
#getoutput in just problem_id and Classes
answers <- data.frame()
# get problem_id from testdata & results from RPartFinalResult
answers <- subset(testdata,select = (problem_id))
answers$classes <- RPartFinalResult
print(answers)
# End of Programming




