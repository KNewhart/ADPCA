#' Uniqueness Check
#'
#' Check for changing process variables
#' @param data An xts object
#' @param N Number of unique observations required. Defaults to 20.
#' @export




uniquenessCheck <- function(data, N = 20) {

  # newData <- apply.daily(x = data, function(x) {
  #                       apply(x, 2, function(y) length(unique(y)))
  #            })
  # if (nrow(newData) == 1) {
  #   r <- 1
  # } else {
  #   r <- nrow(newData) - 1
  # }
  #
  # newData <- data[,(which(newData[r,] > N))]

  uniqueData <- apply(data, 2, function(x) length(unique(x)))
  newData <- data[,(which(uniqueData > N))]
  if (any(colnames(data) == "labelCol")) { # If labelCol exists in original data
    if (!any(colnames(newData) == "labelCol")) { # But was removed in uniqueness check
      labelCol <- data[,"labelCol"]
      newData <- cbind(newData, labelCol)
    }
  }

  return(newData)
}
