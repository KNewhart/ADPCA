#' Construct contribution plot
#'
#' This function tests data for multistate AD-PCA
#' @param rawData where obs is found
#' @param trainingData ADPCA training specifications
#' @param obs observation to plot
#' @return difference.normalized
#' @export


contribution_plot <- function(rawData, trainingData, obs, multistate = TRUE) {

  # 1. Find which state contains the observation
  if (multistate) {
    for (i in 1:length(trainingData)) {
      trainingData.times <- index(trainingData[[i]][[1]])
      if (length(which(!is.na(match(trainingData.times, obs)))) > 0) {
        trainingData.index <- i
        noc.data <- trainingData[[trainingData.index]][[2]]
        obs.data <- NULL
        obs.data <- trainingData[[i]][[2]][which(!is.na(match(trainingData.times, obs))),]
        if (is.null(obs.data)) {
          obs.data <- trainingData[[i]][[3]][which(!is.na(match(trainingData.times, obs))),]
        }
        break
      }
    }
  }
  if (!multistate) {
      trainingData.times <- index(trainingData[[1]])
        noc.data <- trainingData[[2]]
        obs.data <- NULL
        obs.data <- trainingData[[2]][which(!is.na(match(trainingData.times, obs))),]
        if (is.null(obs.data)) {
          obs.data <- trainingData[[3]][which(!is.na(match(trainingData.times, obs))),]
        }
  }


  # 2. Match columns
    obs.data <- obs.data[,-1]
    noc.data <- noc.data[,-1]

  # 3. Save NOC statistical parameters
  mu <- mean(noc.data)
  sigma <- stats::sd(noc.data)

  # 4. Calculate change
  difference <- obs.data - mu
  difference.normalized <- difference/sigma
  colnames(difference.normalized) <- sapply(colnames(difference.normalized), function(x) unlist(strsplit(x, ".PROCESS_VALUE"))[1])

  # # 5. plot
  # barplot(as.matrix(difference.normalized[,1:(ncol(difference.normalized)/2)], row.names = NULL))
  return(difference.normalized)
}
