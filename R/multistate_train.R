#' Train based on multistate data
#'
#' This function trains a multistate AD-PCA based on data
#' @param rawData
#' @param vars
#' @param stateVars
#' @param testingDat
#' @param rollingWindowDays
#' @param alphaN
#' @param faultsToTriggerAlarm
#' @return alarms.xts
#' @export


multistate_train <- function(rawData, vars, stateVars, testingDay, rollingWindowDays, alphaN, faultsToTriggerAlarm) {
  # Divide into subsystems (i.e., BR, MT)
  data <- rawData[,vars]

  # Generate 'labelCol' with state identifiers, function returns list of xts objects (one for each state)
  data <- stateGenerator(data = data, stateVars = stateVars, testingDay = testingDay, rollingWindowDays = rollingWindowDays)

  # Creates list of the size of the number of states from stateGenerator
  trainingSpec_ls <- list()
  j <- numeric()
  if (is.list(data)) {
    l <- length(data)
  } else {
    l <- 1
  }

  # Create training specifications for each state
  for (i in 1:l) {
    tryCatch({
      trainingSpecHolder <- createTrainingSpecs(data = data[[i]],
                                                testingDay = testingDay,
                                                rollingWindowDays = rollingWindowDays,
                                                alpha = alphaN,
                                                faultsToTriggerAlarm = faultsToTriggerAlarm)
      trainingSpec_ls <- c(trainingSpec_ls, list(trainingSpecHolder))
      j <- c(j,i)

    }, error = function(e){})

  }

  results <- list("data" = data, "trainingSpec_ls" = trainingSpec_ls, "testingDay" = testingDay, "faultsToTriggerAlarm" = faultsToTriggerAlarm)
  return(results)
}


