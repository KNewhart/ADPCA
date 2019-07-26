#' Train based on multistate data
#'
#' This function trains a multistate AD-PCA based on data. Data are separated into a subsystem based on the variables specified. A label column is created by concatenating all of the state variables (which are integers) into a unique (string) identifier. If there are sufficient instances of a particular identifier (by default p^2/2 where p is the number of process variables including lag), then the state is included. createTrainingSpecs() function will calculate the principal components used for testing.
#' @param rawData
#' @param vars
#' @param stateVars
#' @param testingDat
#' @param rollingWindowDays
#' @param alphaN
#' @param faultsToTriggerAlarm
#' @param statistic
#' @param by
#' @return alarms.xts
#' @export


multistate_train <- function(rawData, vars, stateVars, testingDay, rollingWindowDays, alphaN, faultsToTriggerAlarm, statistic = "T2", by = "Time") {
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
  # nCores <- detectCores(logical = FALSE)
  # nThreads<- detectCores(logical = TRUE)
  # cluster = makeCluster(nThreads, type = "SOCK")
  # class(cluster)
  # registerDoSNOW(cluster)

  # numCores <- detectCores()
  # registerDoParallel(numCores)
  # trainingSpec_ls <- foreach(i=1:l) %dopar% {
    tryCatch({
      trainingSpecHolder <- createTrainingSpecs(data = data[[i]],
                                                testingDay = testingDay,
                                                rollingWindowDays = rollingWindowDays,
                                                alpha = alphaN,
                                                faultsToTriggerAlarm = faultsToTriggerAlarm,
                                                statistic = statistic,
                                                by = by)
      j <- c(j,i)
      trainingSpec_ls <- c(trainingSpec_ls, list(trainingSpecHolder))


    }, error = function(e){})

  }
  # stopImplicitCluster()
  # stopCluster(cluster)
  # registerDoSEQ()
  # invisible(gc); remove(nCores); remove(nThreads); remove(cluster);

  results <- list("data" = list(data), "trainingSpec_ls" = list(trainingSpec_ls), "testingDay" = testingDay, "faultsToTriggerAlarm" = faultsToTriggerAlarm, "statistic" = statistic)
  return(results)
}


