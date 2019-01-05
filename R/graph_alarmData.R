#' Graph alarm data
#'
#' Function to plot all varaibles in original .DBF file
#' @param data compiled xts object
#' @export
#'

graph_alarmData <- function (data, keyword="", trueFaultTime = NULL, trueAlarmTime = NULL) {
  packageLoad("scales")

  alarms <- data[,which(colnames(data) == "Alarm")]
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

  plot.it <- function() {
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
             pch = 16,
             #col = "black")
             col = ifelse(alarms == 0, "black", ifelse (alarms == 1, "red", ifelse (alarms == 2, "orange", "purple"))))
        if (r.hours > 24) {
          axis.POSIXct(1, at = seq(r[1], r[2], by = "days"), cex.axis = 1, format = "%m/%d")
        } else {
          axis.POSIXct(1, at = seq(r[1], r[2], by = "hours"), cex.axis = 1, format = "%H:%M")
        }

        if (!is.null(trueFaultTime)) {
          abline(v=trueFaultTime, lty = 1, col = "blue")
        } else {}

        if (!is.null(trueAlarmTime)) {
          abline(v=trueAlarmTime, lty = 2, col = "red")
        } else {}

        legend("bottomleft",
               legend = c("IC", "T2", "SPE", "Both"),
               col = c("black", "red", "orange", "purple"),
               pch = 16,
               bty = "n")
        legend("topleft",
               legend = c("Fault Occurred", "System Alarm"),
               col = c("blue", "red"),
               lty=1:2,
               bty = "n")

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
               pch = 16,
               #col = "black")
               col = ifelse(alarms == 0, "black", ifelse (alarms == 1, "red", ifelse (alarms == 2, "orange", "purple"))))

          # line.ewma <- ewma(data[,i], n = 1440, alpha = 0.01)
          # points(x = index(line.ewma),
          #        y = line.ewma, col = "red")

          if (r.hours > 24) {
            axis.POSIXct(1, at = seq(r[1], r[2], by = "days"), cex.axis = 1, format = "%m/%d")
          } else {
            axis.POSIXct(1, at = seq(r[1], r[2], by = "hours"), cex.axis = 1, format = "%H:%M")
          }

          if (!is.null(trueFaultTime)) {
            abline(v=trueFaultTime, lty = 1, col = "blue")
          } else {}

          if (!is.null(trueAlarmTime)) {
            abline(v=trueAlarmTime, lty = 2, col = "red")
          } else {}

          legend("bottomleft",
                 legend = c("IC", "T2", "SPE", "Both"),
                 col = c("black", "red", "orange", "purple"),
                 pch = 16,
                 bty = "n")
          legend("topleft",
                 legend = c("Fault Occurred", "System Alarm"),
                 col = c("blue", "red"),
                 lty=1:2,
                 bty = "n")

        } else {
          plot(x = dates,
               y = data[,i],
               xaxt="n",
               xlab = "",
               ylab = "",
               main = colnames(data)[i],
               type = "p",
               pch = 16,
               #col = "black")
               col = ifelse(alarms == 0, "black", ifelse (alarms == 1, "red", ifelse (alarms == 2, "orange", "purple"))))

          # line.ewma <- ewma(data[,i])
          # line(x = index(line.ewma),
          #      y = line.ewma)
          if (r.hours > 24) {
            axis.POSIXct(1, at = seq(r[1], r[2], by = "days"), cex.axis = 1, format = "%m/%d")
          } else {
            axis.POSIXct(1, at = seq(r[1], r[2], by = "hours"), cex.axis = 1, format = "%H:%M")
          }
          # axis.POSIXct(1, at = seq(r[1], r[2], by = "days"), cex.axis = 1, format = "%m/%d")
          if (!is.null(trueFaultTime)) {
            abline(v=trueFaultTime, lty = 1, col = "blue")
          } else {}

          if (!is.null(trueAlarmTime)) {
            abline(v=trueAlarmTime, lty = 2, col = "red")
          } else {}

          legend("bottomleft",
                 legend = c("IC", "T2", "SPE", "Both"),
                 col = c("black", "red", "orange", "purple"),
                 pch = 16,
                 bty = "n")
          legend("topleft",
                 legend = c("Fault Occurred", "System Alarm"),
                 col = c("blue", "red"),
                 lty=1:2,
                 bty = "n")
        }
      }
    }

  }
  # png(file = paste(filename, ".png", sep=""), units = "in", res = 96, width = 11, height =n*3)
  # pdf(file = paste(filename,".pdf", sep=''),
  #     width = 30,
  #     height = (n)*2)
  svg(filename = paste(filename, ".svg", sep=""), width = 8, height =n*2, family = "serif")
  plot.it()
  dev.off()
  # plot.it() # Margins too large to plot
}
