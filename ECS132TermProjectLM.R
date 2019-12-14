#install.packages("devtools")
#install.packages("tidyverse")
#library(tidyverse)
#library(devtools)
#install_github("matloff/regtools")
#library(regtools)

k <- 2
numberOfRowsToSaveForTestSet <- 100

justWeather <- function()
{
  data(day1)
  weather <- day1[, c(3, 9:13)] # season + variables we want to predict
  weather[,7] <- weather$season == 1
  weather[,8] <- weather$season == 2
  weather[,9] <- weather$season == 3 # if all false, season == 4
  weather[,10] <- weather$weathersit == 1
  weather[,11] <- weather$weathersit == 2
  weather[,12] <- weather$weathersit == 3 # if all false, weathersit == 4
  colnames(weather)[7:12] <- c("seasonIsWinter", "seasonIsSpring", "seasonIsSummer", "weathersitIsOne", "weathersitIsTwo", "weathersitIsThree")
  
  weather
}

# Take careful note that this drops the first k rows! You should drop the first k rows of the original matrix afterwards!
makeXMatrix <- function(originalMatrix, columnsToUseNames) # e.g. makeXMatrix(c(4, 5, 8))
{
  originalColnames <- colnames(originalMatrix)
  xmatrix <- originalMatrix
  for (i in 1:k) {
    for (j in 1:length(columnsToUseNames))
    {
      newColumnIndex <- ncol(originalMatrix) + (i-1)*length(columnsToUseNames) + j # we have already added (i-1)*length(columnsToUse) columns
      xmatrix <- cbind(xmatrix, 0)
      for (h in (k+1):nrow(originalMatrix)) # we will throw away the first k rows, because they don't have datapoints from k days prior
      {
        iDaysAgo <- h - i
        xmatrix[h, newColumnIndex] <- originalMatrix[iDaysAgo, columnsToUseNames[j]]
      }
      colnames(xmatrix)[newColumnIndex] <- paste(columnsToUseNames[j], i, "DaysAgo", sep="")
    }
  }
  xmatrix <- xmatrix[, !(names(xmatrix) %in% originalColnames)] # We only keep predictors from days ago, getting rid of originals
  xmatrix <- tail(xmatrix, -1*k) # drop first k rows
  as.matrix(xmatrix)
}

findValueError <- function(testXMatrix, testResponseVar, model)
{
  predictions <- testXMatrix %*% model
  avgError <- 0 # TODO compare to testResponseVar here
  avgError
}

modelValue <- function(originalMatrix, responseVarColName, columnNamesOfPredictors)
{
  xMatrix <- makeXMatrix(originalMatrix, columnNamesOfPredictors)
  originalMatrix <- tail(originalMatrix, -1*k) # get rid of first k rows, since they are not part of the xmatrix
  trainingSet <- head(originalMatrix, -1*numberOfRowsToSaveForTestSet)
  testSet <- tail(originalMatrix, numberOfRowsToSaveForTestSet)
  trainingXMatrix <- head(xMatrix, -1*numberOfRowsToSaveForTestSet) # this is time-series data. By choosing the last rows as our test set, we avoid letting the test set influence training
  testXMatrix <- tail(xMatrix, numberOfRowsToSaveForTestSet)
  
  trainingResponseVar <- trainingSet[,responseVarColName]
  model <- lm(trainingResponseVar ~ trainingXMatrix)$coefficients
  model
}

# in order, colnamesOfPredictors should contain lists of predictors that: weathersit == 1, weathersit == 2, weathersit == 3, and weathersit == 4
# each list should be a ROW, not a column
modelWeathersit <- function(originalMatrix, colnamesOfPredictors)
{
  doMakeXMatrix <- function(columnNames)
  {
    makeXMatrix(originalMatrix, columnNames)
  }
  doMakeTrainingXMatrices <- function(xMatrix)
  {
    head(xMatrix, -1*numberOfRowsToSaveForTestSet)
  }
  doMakeTestXMatrices <- function(xMatrix)
  {
    tail(xMatrix, numberOfRowsToSaveForTestSet)
  }
  xMatrices <- apply(colnamesOfPredictors, c(1,2), makeXMatrix)
  originalMatrix <- tail(originalMatrix, -1*k)
  trainingSet <- head(originalMatrix, -1*numberOfRowsToSaveForTestSet)
  testSet <- tail(originalMatrix, numberOfRowsToSaveForTestSet)
  trainingXMatrices <- apply(xMatrices, 1, doMakeTrainingXMatrices) # this is time-series data. By choosing the last rows as our test set, we avoid letting the test set influence training
  testXMatrices <- apply(xMatrices, 1, doMakeTestXMatrices)
  
  isWeathersitFourNumeric <- function(weatherRow)
  {
    as.numeric(weatherRow$weathersitIsOne == 0 && weatherRow$weathersitIsTwo == 0 && weatherRow$weathersitIsThree == 0)
  }
  weathersitIsFour <- apply(1, isWeathersitFourNumeric)
  trainingResponseVars <- c(trainingSet[, weathersitIsOne], trainingSet[, weathersitIsTwo], trainingSet[, weathersitIsThree], weathersitIsFour)
  models <- c()
  for (i in 1:4)
  {
    models[i] <- glm(trainingResponseVars[i] ~ trainingXMatrices[i], family=binomial)$coefficients
  }
  models
}
  
  