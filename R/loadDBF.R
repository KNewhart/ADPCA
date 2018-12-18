#' Load all original data files
#'
#' This function loops through all filenames within the date range, checks that the file exists, and if it does exist, is imported and compiled to a dataframe
#' @param dataLocation String excluding ending backslashes of LogAllData folder
#' @param endDay Date object with the last date to be included
#' @param nDays Number of days of data to compile
#' @param folder LogAllData or Log60sec
#' @return compiledData, Dataframe with all original data from .DBF files.
#' @examples
#' rawData <- loadDBF(dataLocation, testingDay, nDays)
#' @export


# This function takes in the location of the 1-min SB-MBR data files
# (where "LogAllData" is located including fial backslashes),
# the last day to be compiled, and the number of previous days to include

loadDBF <- function(dataLocation, endDay, nDays, folder = "LogAllData") {
  # Check if package is included already, if not then load
  library("foreign")

  # Create an object that includes numeric day, month, and year values to iterate through
  dates <- setDates((endDay - nDays), endDay)

  # Create empty dataframe
  compiledData <- NULL

  # Loop through all files within the date range
  # Loop through years
  while (dates$currentYear <= dates$finalYear) {
    # Loop through months of the year
    while (((dates$currentMonth <= dates$finalMonth) && (dates$currentYear == dates$finalYear)) || ((dates$currentMonth <= 12) && (dates$currentYear < dates$finalYear))) {
      # Loop through days of the month
      while (((dates$currentDay <= 31) && (dates$currentMonth != dates$finalMonth)) || ((dates$currentDay <= dates$finalDay) && (dates$currentMonth == dates$finalMonth))) {

        # Create a string from the current date to access file
        dateString <- paste(dates$currentYear,sprintf("%02d",dates$currentMonth),sprintf("%02d",dates$currentDay),sep=' ')

        ### Check to see if file exists:
        if (!file.exists(paste(dataLocation,folder,"\\",dateString," 0000 ",folder," (Wide).DBF",sep=''))) {
          # If the file does not exist, and the date is the 31st...
          if (dates$currentDay > 31) {
            # ... the end of the month has been reached, so break from the daily 'while' loop
            break
          }
          # If the file does not exist, and it is not the end of the month...
          else {
            #... there is a missing file. Progress to the next day.
            dates$currentDay <- dates$currentDay + 1
            next
          }
        }
        # File exists, therefore end the if statement
        else {}





        ### Read in raw data from 'Wide' file:
        # Create an empty dataframe for the raw, imported data
        rawData <- NULL
        # Attempt to open file. If file can be opened, write to dataframe.
        tryCatch(rawData <- read.dbf(paste(dataLocation,folder, "\\",dateString," 0000 ",folder," (Wide).DBF",sep='')),error=function(e){})
        # If the file could not be opened, the dataframe is still empty.
        if (length(rawData) == 0) {
          # Since file could not be opened, move onto the next day
          dates$currentDay <- dates$currentDay + 1
          next
        }

        ### Compile raw data from previous days:
        # If this is the first day, create the bulk data file
        if (is.null(compiledData)) {
          compiledData <- rawData
        }
        # If it is any other day in the series, add the day's data to the bulk, raw data file
        else {
          compiledData <- rbind(compiledData,rawData)
        }



        ### Progress through loop:
        # Go to next day in the month
        dates$currentDay <- dates$currentDay + 1
      }
      # Go to next month in the year
      dates$currentMonth <- dates$currentMonth + 1
      # Go to the first day of the next month
      dates$currentDay <- 1
    }
    # Go to the next year
    dates$currentYear <- dates$currentYear + 1
    # Reset the month to the beginning of the next year
    dates$currentMonth <- 1
  }


  return(compiledData)

}
