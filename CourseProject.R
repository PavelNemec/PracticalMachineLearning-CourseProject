
#Main function
main <- function()
{
        #Loading packages
        library(caret)
        library(randomForest)
        
        #Loading data
        train <- read.csv("http://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv", na.strings = c("", "NA", "#DIV/0!"))
        test <- read.csv("https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv", na.strings = c("", "NA", "#DIV/0!"))
        
        #Cleaning data
        train <- cleanData(train)
        test <- cleanData(test)
        
        #Split training set into two subsets - 60% training set, 40% testing set
        subSamples <- createDataPartition(y=train$classe, p=0.60, list=FALSE)
        trainTraining <- train[subsamples, ] 
        trainTesting <- train[-subsamples, ]
        
        #Model creation - using random forest
        model <- randomForest(classe ~. , data=trainTraining, method="class")
        
        # Measuring quality of random forest model
        prediction <- predict(model, trainTesting, type = "class")
        print('Overall statistics shows more than 99.5% of accuracy of this model')
        print('------------------------------------------------------------------')
        print(confusionMatrix(prediction, trainTesting$classe))
        
        predictForSubmission <- predict(model, test, type="class")
        
        print('Model prediction for testing data:')
        print('----------------------------------')
        predictForSubmission
}

cleanData <- function (dataFrame)
{
        ## Remove first seven columns which are useless for prediction
        dataFrame   <-dataFrame[,-c(1:7)]
        ## Remove columns with a lot of NA values
        dataFrame <- filterNAColums(dataFrame)
        ## Remove zero covarietes
        nzv<- nearZeroVar(dataFrame,saveMetrics=TRUE)
        dataFrame <- dataFrame[,nzv$nzv==FALSE]
        
        dataFrame
}

## Filter columns with more NA values than given threshold
filterNAColums <- function (dataFrame, NAthreshold = 0.6)
{
        result <- dataFrame[,apply(dataFrame, 2, function(x,y) isVectorNA(x,NAthreshold) == FALSE), drop=FALSE]
        result
}

##True in case that given vector has more NA values than given threshold
isVectorNA <- function (vector, NAthreshold = 0.6)
{
        naCount <- sum(is.na(vector))
        vectorNARatio  <- naCount/length(vector)
        
        removeVector <- FALSE
        if (vectorNARatio > NAthreshold)
        {
                removeVector = TRUE
        }
        
        removeVector
}