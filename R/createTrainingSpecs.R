#' Run Training Functions
#'
#' Using only the data from the start to the day before testing day, train the ADPCA model using functions from mvMonitoring
#' @param data Dataframe of process data
#' @param testingDay Date object, all dates before this date will be included in the training analysis
#' @param rollingWindowDays Number of observations used in the rolling window
#' @param alpha Accuracy of analysis, defaults to 0.01
#' @param faultsToTriggerAlarm Number of abnormal observations to be considered an alarm, defaults to five
#' @export
#' @examples
#' trainingSpecHolder <- createTrainingSpecs(data = dataBR[[i]],
#' testingDay = testingDay,
#' trainObs = rollingWindowObs,
#' updateFreq = updateObs,
#' alpha = alphaN,
#' faultsToTriggerAlarm = faultsToTriggerAlarm)

createTrainingSpecs <- function(data, testingDay, rollingWindowDays, alpha = 0.01, faultsToTriggerAlarm = 5) {
  trainObs <- nrow(data[paste("/",as.Date(index(data[1])) + rollingWindowDays, sep="")])
  updateFreq <- nrow(data[paste(as.Date(testingDay - 1), "/", as.Date(testingDay-1), sep="")])
  training <- data[paste("/",testingDay-1, sep='')]
  #training <- xts(data, order.by = as.POSIXct(index(data)))[paste("/",(testingDay-1), sep='')]
  labelVec <- which(colnames(training) == "labelCol")
  subsetList <- colnames(training[,-labelVec])
  trainingDataResults_ls <- mspTrain(data = training[,-labelVec], # xts data matrix
                                     #labelVector = training[,labelVec], # multistate
                                     labelVector = rep(1,nrow(training)),
                                     subsetList = subsetList,
                                     trainObs = trainObs, # number of observations to train algorithm
                                     #trainObs = length(training),
                                     updateFreq = updateFreq, # alorithum update frequency (once a day)
                                     #updateFreq = length(xts(data, order.by = as.POSIXct(index(data)))[paste(testingDay,"/", sep='')]),
                                     alpha = alpha,
                                     faultsToTriggerAlarm = faultsToTriggerAlarm)

  return(trainingDataResults_ls)
}
