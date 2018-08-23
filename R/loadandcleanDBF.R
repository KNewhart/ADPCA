#' Import and Prepare Data
#'
#' This function compiles raw data and column names and elimates extraneous columns
#' @param dataLocation String excluding ending backslashes of LogAllData folder
#' @param testingDay Date object with the last date to be included
#' @param nDays Number of days of data to compile
#' @return rawData, Dataframe including all compiled data
#' @examples
#' dataLocation <- "MP SB-MBR Data\\"
#' testingDay <- as.Date("2017 08 31", format = "%Y %m %d")
#' nDays <- 30
#' rawData <- loadandcleanDBF(dataLocation, testingDay, nDays)
#' @export

loadandcleanDBF <- function (dataLocation, testingDay, nDays) {

  # Compile raw data from data files (no column names, extraneous empty columns)
  rawData <- loadDBF(dataLocation, testingDay, nDays)

  # Read in data labels from 'Tagname' file
  columnNames <- read.dbf(paste(dataLocation,"LogAllData\\",format(testingDay, format = "%Y %m %d")," 0000 LogAllData (Tagname).DBF",sep=''))

  # Run Cleaner function (rename columns, remove extraneous columns)
  rawData <- cleanDBF(rawData, columnNames)

  return(rawData)
}
