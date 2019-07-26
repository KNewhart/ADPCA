#' State Generator
#'
#' Add a column to the dataset with a unique indentifier for the operating conditions
#' @param data Dataframe with column names
#' @param stateVars Vector of character strings to identify state variables by their column names in \code{data}
#' @param minObs Number of minimum observations required to define a "state." Defaults to statistically required p^2/2 where p is the number of process variables (including lag)
#' @param testingDay Date object of the day to be evaluated for faults.
#' @param rollingWindowDays Integer with the number of days of data to be included in the training window.
#' @return A list of xts objects with data, a merged "labelCol," and separated by operating state
#' @export
#' @examples
#' dataBR <- stateGenerator(data = dataBR, stateVars = stateVarsBR, minObs = rollingWindowObs)
#'

stateGenerator <- function(data,
                           stateVars,
                           minObs = round((2*length(which(!(colnames(data) %in% stateVars))))^2/2),
                           testingDay = NULL,
                           rollingWindowDays = 0) {

  # Determine the column numbers of the state variables
  colNo <- which(colnames(data) %in% stateVars)

  # Returns character string of all state combinations
  data$labelCol <- as.numeric(do.call(paste,c(as.data.frame(data[,colNo]),sep="")))

  # Frequency distribution of each state?
  if (rollingWindowDays == 0) {
    freqDistribution <- as.data.frame(table(data$labelCol))
  } else {
    freqDistribution <- as.data.frame(table(data$labelCol[paste((testingDay-rollingWindowDays),"/",(testingDay-1),sep="")]))
  }

  # Is the frequency of each state within the subset greater than the minimum during the training period?
  statesToKeep <- as.numeric(as.character(freqDistribution[which(freqDistribution[,2] >= minObs),1]))

  # Test to ensure at least one state is included
  if (identical(statesToKeep, numeric(0))) {
    print("Insufficient datapoints to train. Increase rolling window size.")
    return(data)
  } else {
    # Only keep the states with sufficient datapoints
    dataIndex <- which(data$labelCol %in% statesToKeep)
    dataNew <- data[dataIndex,-colNo]
    dataNew <- na.omit(dataNew)
    dataNew <- dataNew[apply(dataNew, 1, function(x) all(is.finite(x))),]
    data_ls <- list()
    for (i in 1:length(statesToKeep)) {
      dataHolder <- dataNew[which(dataNew$labelCol == statesToKeep[i]),]
      if (rollingWindowDays == 0) {
        data_ls <- c(data_ls, list(dataHolder))
      } else {
        data_ls <- c(data_ls, list(uniquenessCheck(dataHolder)))
      }

    }
    return(data_ls)
  }


}
