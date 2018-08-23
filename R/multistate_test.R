#' Test based on multistate data
#'
#' This function tests data for multistate AD-PCA
#' @param data
#' @param trainingSpec_ls
#' @param testingDay
#' @param faultsToTriggerAlarm
#' @return alarms.xts
#' @export


multistate_test <- function(data, trainingSpec_ls, testingDay, faultsToTriggerAlarm) {

  # Creates list of the size of the number of states from stateGenerator
  alarmData_ls <- list()
  if (is.list(trainingSpec_ls)) {
    l <- length(trainingSpec_ls)
  } else {
    l <- 1
  }

  # Test new observations for each state
  for (i in 1:l) {
    alarmDataHolder <- testNewObs(data = data[[j[i]]],
                                  trainingSpecs = trainingSpec_ls[[i]],
                                  testingDay = testingDay,
                                  faultsToTriggerAlarm = faultsToTriggerAlarm)
    alarmData_ls <- c(alarmData_ls, list(alarmDataHolder))
  }

  # Compile all states
  alarmData <- data.frame()
  for (i in 1:length(alarmData_ls)) {
    if (i == 1) {
      alarmData <- as.data.frame(alarmData_ls[[i]])
    } else {
      alarmData <- fastmerge(alarmData, as.data.frame(alarmData_ls[[i]]))
    }
  }

  alarms.xts <- as.xts(alarmData, order.by = as.POSIXct(rownames(alarmData)))

  # Return xts with test data
  return(alarms.xts)

}
