if (!file.exists("data")) {
  dir.create("data")
}

trainUrl <- "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv"
testUrl <- "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv"

download.file(trainUrl, "./data/train.csv", method="curl")
download.file(testUrl, "./data/test.csv", method="curl")

train <- read.csv("./data/train.csv", header=TRUE)
test <- read.csv("./data/test.csv", header=TRUE)

# We will be using the `caret` package exclusively the build our model. 

# remove unnecessary columns
colsToRemove <- c("X", "user_name", "raw_timestamp_part_1", "new_window", 
                  "num_window", "raw_timestamp_part_2", "cvtd_timestamp")
processedTrain <- train[, !names(train) %in% colsToRemove]
processedTest <- test[, !names(test) %in% colsToRemove]

# Lets remove any rows that are all NA's. We start with a total count of `r format(nrow(train), big.mark=",", scientific=FALSE)` rows:
naRow <- (rowSums(is.na(train)) == 0) # locate rows that are all NA's
processedTrain <- processedTrain[!naRow,]
   
# We removed `r nrow(train[naRow == TRUE,])` rows of NA's with `r format(nrow(train), big.mark=",", scientific=FALSE)` rows remaining. 
# no NA's were found in the test data set.

# remove any variables that are all NA
#naCol <- (colSums(is.na(processedTrain)) == 0)
#processedTrain <- processedTrain[,naCol]
#processedTest <- processedTest[,naCol]

# We want to weed out  
# variables with low, unique values--less than 20%. Also, we will check the skewness of the frequency distribution of each variable. 
# If the ratio of most frequent value to the second most frequent value is greater than 20, the distribution 
# of the predictor may be highly skewed. Caret has a function called `nearZeroVar` that we can use to remove those columns
# that meet either criteria:
  
stopifnot(require(caret, quietly=TRUE))
nzv <- nearZeroVar(processedTrain)
processedTrain <- processedTrain[,-nzv]
processedTest <- processedTest[,-nzv]

# After this step, we have `r ncol(processedTrain)` variables remaining.


#processedTrain[is.na(processedTrain)] <- 0

#str(processedTrain)
#summary(processedTrain)

# Since some predictive models are susceptible to multicollinearity (high correlations between predictors) Other models, such as classifcation or
# regression trees, might be resistant to highly correlated predictors, but multicollinearity may
# negatively affect interpretability of the model.

descrCorr <- cor(processedTrain[,sapply(processedTrain, is.numeric)]) # locate all numeric columns and coorelate
highCorr <- findCorrelation(descrCorr, 0.90)
processedTrain <- processedTrain[, -highCorr]
processedTest <- processedTest[, -highCorr]

# After this step, we have `r ncol(processedTrain)` variables remaining.

?sapply
str(processedTrain)
str(processedTest)
summary(processedTrain)
summary(processedTest)
class(processedTest)
str(processedTrain)
table(sapply(processedTrain, is.na))
table(sapply(processedTest, is.na))

#xTrans <-preProcess(processedTrain[,-46])
#predict(xTrans, processedTrain)



# Cross-validation
subset <- createDataPartition(y=processedTrain$classe, p=0.6, list=FALSE)
processedTrain.trainSet <- processedTrain[subset, ]
processedTrain.validationSet <- processedTrain[-subset, ]

set.seed(777)
fitRF <- train(classe~., method='rf', data=processedTrain.trainSet, trControl=trainControl(method="cv"))

predVS <- predict(fitRF, processedTrain.validationSet)
confusionMatrix(table(predVS, processedTrain.validationSet$classe))

confusionMatrix(table(predVS, processedTrain.validationSet$classe))$overall

install.packages("broom")
library(broom)
tidy(lmfit)


predTest <- predict(fitRF, processedTest)

class(predTest)
plot(fitRF)
plot(varImp(fitRF, scale=FALSE), top=30)
varImp(fitRF, scale=FALSE)
plot(fitRF, metric = "Kappa")
plot(fitRF, metric = "Level")
resampleHist(fitRF)

summary(fitRF)$coef



cbind(processedTest$problem_id, as.factor(predTest)

rbind(processedTest$problem_id, predTest)

##  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20 
##  B  A  B  A  A  E  D  B  A  A  B  C  B  A  E  E  A  B  B  B 
## Levels: A B C D E


answers = c('B' , 'A' , 'B' , 'A' , 'A' , 'E' , 'D' , 'B' , 'A' , 'A' , 'B' , 'C' , 'B' , 'A' , 'E' , 'E' , 'A' , 'B' , 'B' , 'B' )
pml_write_files = function(x){
  n = length(x)
  for(i in 1:n){
    filename = paste0("F:\\Documents\\eInvasion\\School\\Coursera\\John Hopkins University\\Data Science\\9 - Developing Data Products\\Course Project\\problem_id_",i,".txt")
    write.table(x[i],file=filename,quote=FALSE,row.names=FALSE,col.names=FALSE)
  }
}
pml_write_files(answers)
