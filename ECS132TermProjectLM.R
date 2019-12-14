#install.packages("devtools")
#install.packages("tidyverse")
library(tidyverse)
library(devtools)
#install_github("matloff/regtools")
library(regtools)

k <- 1
numberOfRowsToSaveForTestSet <- 100

justWeather <- function()
{
  data(day1)
  weather <- day1[, c(3, 9:13)] # season, month, + variables we want to predict
  weather[,7] <- weather$season == 1
  weather[,8] <- weather$season == 2
  weather[,9] <- weather$season == 3 # if all false, season == 4
  weather[,10] <- day1[,"mnth"]**2
  weather[,11] <- weather$weathersit == 1
  weather[,12] <- weather$weathersit == 2 # if both false, weathersit == 3
  colnames(weather)[7:12] <- c("seasonIsWinter", "seasonIsSpring", "seasonIsSummer", "monthSquared", "weathersitIsOne", "weathersitIsTwo")
  
  weather
}

day1Expanded <- justWeather()

# Take careful note that this drops the first k rows! You should drop the first k rows of the original matrix afterwards!
makeXMatrix <- function(columnsToUseNames) # e.g. makeXMatrix(c(4, 5, 8))
{
  originalColnames <- colnames(day1Expanded)
  xmatrix <- day1Expanded
  for (i in 1:k) {
    for (j in 1:length(columnsToUseNames))
    {
      newColumnIndex <- ncol(day1Expanded) + (i-1)*length(columnsToUseNames) + j # we have already added (i-1)*length(columnsToUse) columns
      xmatrix <- cbind(xmatrix, 0)
      for (h in (k+1):nrow(day1Expanded)) # we will throw away the first k rows, because they don't have datapoints from k days prior
      {
        iDaysAgo <- h - i
        xmatrix[h, newColumnIndex] <- day1Expanded[iDaysAgo, columnsToUseNames[j]]
      }
      colnames(xmatrix)[newColumnIndex] <- paste(columnsToUseNames[j], i, "DaysAgo", sep="")
    }
  }
  xmatrix <- xmatrix[, !(names(xmatrix) %in% originalColnames)] # We only keep predictors from days ago, getting rid of originals
  xmatrix <- tail(xmatrix, -1*k) # drop first k rows
  as.matrix(xmatrix)
}

findValueError <- function(testXMatrix, testResponseVar, modelCoefficients)
{
  predictions <- cbind(1, testXMatrix) %*% modelCoefficients
  avgSquaredError <- mean(abs(predictions - testResponseVar))
  avgSquaredError
}

findWeathersitError <- function(testXMatrices, testResponseVar, models)
{
  likelihoodsOfEachWeathersit <- list()
  for (i in 1:3) {
    testXMatrix <- cbind(1, testXMatrices[[i]])
    likelihoodsOfEachWeathersit[[i]] <- testXMatrix %*% models[[i]]$coefficients
  }
  weathersitProbabilities <- cbind(likelihoodsOfEachWeathersit[[1]], likelihoodsOfEachWeathersit[[2]],likelihoodsOfEachWeathersit[[3]])
  chooseBestWeathersit <- function(probabilityWeathersitIs)
  {
    if (probabilityWeathersitIs[1] >= probabilityWeathersitIs[2] && probabilityWeathersitIs[1] >= probabilityWeathersitIs[3])
    {
      return(1)
    }
    else if (probabilityWeathersitIs[2] >= probabilityWeathersitIs[3])
    {
      return(2)
    }
    return(3)
  }
  bestWeathersitPrediction <- apply(weathersitProbabilities, 1, chooseBestWeathersit)
  return(mean(testResponseVar == bestWeathersitPrediction))
}

modelValue <- function(responseVarColName, columnNamesOfPredictors)
{
  xMatrix <- makeXMatrix(columnNamesOfPredictors)
  day1ExpandedChopped <- tail(day1Expanded, -1*k) # get rid of first k rows, since they are not part of the xmatrix
  trainingSet <- head(day1ExpandedChopped, -1*numberOfRowsToSaveForTestSet)
  testSet <- tail(day1ExpandedChopped, numberOfRowsToSaveForTestSet)
  trainingXMatrix <- head(xMatrix, -1*numberOfRowsToSaveForTestSet) # this is time-series data. By choosing the last rows as our test set, we avoid letting the test set influence training
  testXMatrix <- tail(xMatrix, numberOfRowsToSaveForTestSet)
  
  trainingResponseVar <- trainingSet[,responseVarColName]
  model <- lm(trainingResponseVar ~ trainingXMatrix)
  betaHat <- model$coefficients
  print(paste("AdjRSquared:", summary(model)$adj.r.squared))
  print(paste("Average Error:", findValueError(testXMatrix, testSet[, responseVarColName], betaHat)))
  
  
  #model
}

# in order, colnamesOfPredictors should be a list of vectors of predictors that: weathersit == 1, weathersit == 2, weathersit == 3, and weathersit == 4
# syntax: list(c("predictor1ForWeathersitBeing1", "predictor2ForWeathersitBeing1"), ..., c("predictor1ForWeathersitBeing4"))
modelWeathersit <- function(colnamesOfPredictors) # weathersit == 4 has never happened in the data; we won't even consider it as a possibility when predicting the next day's weathersit
{
  xMatrices <- list()
  trainingXMatrices <- list()
  testXMatrices <- list()
  for (i in 1:3)
  {
    xMatrix <- makeXMatrix(colnamesOfPredictors[[i]])
    xMatrices[[i]] <- xMatrix
    trainingXMatrices[[i]] <- head(xMatrix, -1*numberOfRowsToSaveForTestSet) # this is time-series data. By choosing the last rows as our test set, we avoid letting the test set influence training
    testXMatrices[[i]] <- tail(xMatrix, numberOfRowsToSaveForTestSet)
  }
  day1ExpandedChopped <- tail(day1Expanded, -1*k)
  trainingSet <- head(day1ExpandedChopped, -1*numberOfRowsToSaveForTestSet)
  testSet <- tail(day1ExpandedChopped, numberOfRowsToSaveForTestSet)
  
  isWeathersitThreeNumeric <- function(weatherRow)
  {
    as.numeric(weatherRow["weathersit"] == 3)
  }
  trainingWeathersitIsThree <- apply(trainingSet, 1, isWeathersitThreeNumeric)
  trainingResponseVars <- list(trainingSet[, "weathersitIsOne"], trainingSet[, "weathersitIsTwo"], trainingWeathersitIsThree)
  models <- list()
  for (i in 1:3)
  {
    models[[i]] <- glm(trainingResponseVars[[i]] ~ trainingXMatrices[[i]], family=binomial)
    #print(summary(models[[i]]))
  }
  testResponseVars <- testSet[, "weathersit"]
  findWeathersitError(testXMatrices, testResponseVars, models)
}

modelAll <- function()
{
  print("windspeed:")
  modelValue("windspeed", c("windspeed"))#, "weathersitIsOne", "weathersitIsTwo", "monthSquared"))
  print("temp:")
  modelValue("temp", c("temp"))
  print("atemp:")
  modelValue("atemp", c("atemp"))
  print("hum:")
  modelValue("hum", c("hum"))
  weathersitPredictors <-
    list(
      c("weathersitIsOne"),#, "weathersitIsTwo"),
      c("weathersitIsOne"),#, "weathersitIsTwo"),
      c("weathersitIsOne", "weathersitIsTwo")
    )
  paste("Proportion of time we predict weathersit correctly:", modelWeathersit(weathersitPredictors))
}
  