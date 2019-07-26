#' Test observations for abnormal conditions
#'
#' This function compares distance of new observations to the subspace identified by ADPCA in trainingSpecs() by calling mspMonitor() and mspWarning() from mvMonitoring package
#' @param data xts object
#' @param trainingSpecs object returned from createTrainingSpecs()
#' @param testingDay Date object of testing observations
#' @param faultsToTriggerAlarm Defaults to 5
#' @export
#'


testNewObs <- function(data, trainingSpecs, testingDay, faultsToTriggerAlarm = 5) {
  library(xts)
  testing <- xts(data, order.by = as.POSIXct(index(data), "%Y-%m-%d %H:%M:%S", origin = "1970-01-01"))[paste(testingDay, "/", sep='')]

  # Check for NA
  if (anyNA(testing)) {
    testing <- na.omit(testing)
  }

  # Lag data
  laggedtestingData <- lag.xts(testing,0:1)[2:nrow(testing),]

  # Check that columns in testing set are the same as training
  cols2keep <- colnames(as.data.frame(trainingSpecs$Non_Alarmed_Obs))
  cols2remove <- which(!(colnames(laggedtestingData) %in% cols2keep))
  laggedtestingData <- laggedtestingData[,-cols2remove]

  #  Identify the label vector
  labelVec <- which(colnames(testing) == "labelCol")

  # Run mspMonitor
  realDataandFlags <- mspMonitor(observations = laggedtestingData,
                                 #labelVector = testing[2:nrow(testing),labelVec],
                                 labelVector = rep(1,nrow(laggedtestingData)),
                                 trainingSummary = trainingSpecs$TrainingSpecs)

  # These lines will test each line as if it was just received:
  realAlarmData <- realDataandFlags


  # numCores <- detectCores()
  # registerDoParallel(numCores)
  # realAlarmData <- foreach(i=1:nrow(realDataandFlags), .combine = rbind) %dopar% {
  # for(i in 1:nrow(realDataandFlags)){
  # sapply(seq(1,nrow(realDataandFlags)), function(i){
    realAlarmData <- mspWarning(mspMonitor_object = realDataandFlags, faultsToTriggerAlarm = faultsToTriggerAlarm)
  # }
  # )
  # realAlarmData <- apply(realAlarmData,1,function(i) mvMonitoringv2::mspWarning(mspMonitor_object = realDataandFlags[i,], faultsToTriggerAlarm = faultsToTriggerAlarm))
  # stopImplicitCluster()

  realAlarmData <- cbind(realAlarmData, rep(as.numeric(trainingSpecs$TrainingSpecs$`1`$SPE_threshold)))
  colnames(realAlarmData)[length(colnames(realAlarmData))] <- "SPE_threshold"
  realAlarmData <- cbind(realAlarmData, rep(as.numeric(trainingSpecs$TrainingSpecs$`1`$T2_threshold)))
  colnames(realAlarmData)[length(colnames(realAlarmData))] <- "T2_threshold"
  if (!is.null(testing$labelCol[1])) {
    realAlarmData <- cbind(realAlarmData, rep(as.numeric(testing$labelCol[1])))
    colnames(realAlarmData)[length(colnames(realAlarmData))] <- "labelCol"
  }

  return(realAlarmData)
}
