#' Graph raw data
#'
#' Function to plot all varaibles in original .DBF file
#' @param data compiled xts object
#' @export
#'

graph_rawData <- function (data, keyword="", trueFaultTime = NULL) {
  packageLoad("scales")
  cols <- grep("PROCESS_VALUE", colnames(data))
  data <- data[,cols]
  colnames(data) <- sapply(colnames(data), function(x) unlist(strsplit(x, "PROCESS_VALUE"))[1])
  n <- ncol(data)
  tmp <- grep("TRANS_PRESS", colnames(data))
  do <- grep("DO", colnames(data))
  dates <- index(data)
  r <- as.POSIXct(range(dates), format = "%Y-%m-%d %H:%M:%S")
  r.hours <- as.numeric(difftime(r[2], r[1], units = "hours"))
  if (keyword=="") {
    filename <- paste("allGraphs",as.Date(r[1]), as.Date(r[2]))
    } else {
      filename <- paste(keyword, "allGraphs",as.Date(r[1]), as.Date(r[2]))
    }
  # png(file = paste(filename, ".png", sep=""), units = "in", res = 96, width = 11, height =n*3)
  # pdf(file = paste(filename,".pdf", sep=''),
  #     width = 30,
  #     height = (n)*2)
  svg(filename = paste(filename, ".svg", sep=""), width = 8, height =n*2, family = "serif")

  par(mfrow=c((n),1), mar = c(2.1, 4.1, 4.1, 2.1))

  for (i in 1:n) {

    if (i %in% tmp) {
      plot(x = dates,
           y = data[,i],
           xaxt="n",
           ylim=c(0,7),
           xlab = "",
           ylab = "",
           main = colnames(data)[i],
           type = "p",
           col = "black")
      if (r.hours > 24) {
        axis.POSIXct(1, at = seq(r[1], r[2], by = "days"), cex.axis = 1, format = "%m/%d")
      } else {
        axis.POSIXct(1, at = seq(r[1], r[2], by = "hours"), cex.axis = 1, format = "%H:%M")
      }

      if (!is.null(trueFaultTime)) abline(v=trueFaultTime, col="blue")

      } else {
      if (i %in% do) {
        plot(x = dates,
             y = data[,i],
             xaxt="n",
             ylim=c(0,3),
             xlab = "",
             ylab = "",
             main = colnames(data)[i],
             type = "p",
             col = "black")
        # line.ewma <- ewma(data[,i], n = 1440, alpha = 0.01)
        # points(x = index(line.ewma),
        #        y = line.ewma, col = "red")

        if (r.hours > 24) {
          axis.POSIXct(1, at = seq(r[1], r[2], by = "days"), cex.axis = 1, format = "%m/%d")
        } else {
          axis.POSIXct(1, at = seq(r[1], r[2], by = "hours"), cex.axis = 1, format = "%H:%M")
        }

        if (!is.null(trueFaultTime)) abline(v=trueFaultTime, col="blue")

      } else {
        plot(x = dates,
             y = data[,i],
             xaxt="n",
             xlab = "",
             ylab = "",
             main = colnames(data)[i],
             type = "p",
             col = "black")
        # line.ewma <- ewma(data[,i])
        # line(x = index(line.ewma),
        #      y = line.ewma)
        if (r.hours > 24) {
          axis.POSIXct(1, at = seq(r[1], r[2], by = "days"), cex.axis = 1, format = "%m/%d")
        } else {
          axis.POSIXct(1, at = seq(r[1], r[2], by = "hours"), cex.axis = 1, format = "%H:%M")
        }
        # axis.POSIXct(1, at = seq(r[1], r[2], by = "days"), cex.axis = 1, format = "%m/%d")
        if (!is.null(trueFaultTime)) abline(v=trueFaultTime, col="blue")
      }
    }
  }

  dev.off()

}
