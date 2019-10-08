#' Graph alarm data
#'
#' Function to plot all varaibles in original .DBF file
#' @param data compiled xts object
#' @export
#'

graph_alarmData <- function (data,
                             keyword="",
                             trueFaultTime = NULL,
                             trueAlarmTime = NULL,
                             output = FALSE,
                             w = 8,
                             h = 2,
                             inset.delta = 0,
                             filetype = "pdf",
                             fault.interval = NULL) {
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
    if (is.null(trueAlarmTime)) par(mfrow=c((n),1), mar = c(2.1, 3.1, 3.6, 5.6))
    if (!is.null(trueAlarmTime)) par(mfrow=c((n),1), mar = c(2.1, 3.1, 3.6, 9.1))

    for (i in 1:n) {

      if (i %in% tmp) {
        plot(x = dates,
             y = data[,i],
             xaxt="n",
             #ylim=c(0,7),
             xlab = "",
             ylab = "",
             main = colnames(data)[i],
             type = "p",
             pch = 16,
             #col = "black")
             col = ifelse(alarms == 0, "black", ifelse (alarms == 1, "red", ifelse (alarms == 2, "orange", "purple"))))
        if (r.hours > 49) {
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

        if (!is.null(fault.interval)) {
          for(i in 1:length(fault.interval)) {
            abline(v=fault.interval[i]@start, col="red")
            abline(v=fault.interval[i]@start+fault.interval[i]@.Data,col="red")
          }
        }

        if(is.null(trueAlarmTime)) {
          legend("right", inset = c((-.075+inset.delta),0),
                 legend = c("IC", "T2", "SPE", "Both"),
                 col = c("black", "red", "orange", "purple"),
                 pch = 16,
                 bty = "n",
                 xpd=NA)
        }

        if (!is.null(trueAlarmTime)) {
          legend("right", inset = c((-.2+inset.delta),0),
                 legend = c("IC", "T2", "SPE", "Both","Fault Occurred", "System Alarm"),
                 col = c("black", "red", "orange", "purple","blue", "red"),
                 pch = c(16,16,16,16,NA,NA),
                 lty = c(NA,NA,NA,NA,1,2),
                 bty = "n",
                 xpd=NA)
        }


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

          if (r.hours > 49) {
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

          if (!is.null(fault.interval)) {
            for(i in 1:length(fault.interval)) {
              abline(v=fault.interval[i]@start, col="red")
              abline(v=fault.interval[i]@start+fault.interval[i]@.Data,col="red")
            }
          }

          if(is.null(trueAlarmTime)) {
            legend("right", inset = c((-.075+inset.delta),0),
                   legend = c("IC", "T2", "SPE", "Both"),
                   col = c("black", "red", "orange", "purple"),
                   pch = 16,
                   bty = "n",
                   xpd=NA)
          }

          if (!is.null(trueAlarmTime)) {
            legend("right", inset = c((-.2+inset.delta),0),
                   legend = c("IC", "T2", "SPE", "Both","Fault Occurred", "System Alarm"),
                   col = c("black", "red", "orange", "purple","blue", "red"),
                   pch = c(16,16,16,16,NA,NA),
                   lty = c(NA,NA,NA,NA,1,2),
                   bty = "n",
                   xpd=NA)
          }

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
          if (r.hours > 49) {
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

          if (!is.null(fault.interval)) {
            for(i in 1:length(fault.interval)) {
              abline(v=fault.interval[i]@start, col="red")
              abline(v=fault.interval[i]@start+fault.interval[i]@.Data,col="red")
            }
          }

          if(is.null(trueAlarmTime)) {
            legend("right", inset = c((-.075+inset.delta),0),
                   legend = c("IC", "T2", "SPE", "Both"),
                   col = c("black", "red", "orange", "purple"),
                   pch = 16,
                   bty = "n",
                   xpd=NA)
          }

          if (!is.null(trueAlarmTime)) {
            legend("right", inset = c((-.2+inset.delta),0),
                   legend = c("IC", "T2", "SPE", "Both","Fault Occurred", "System Alarm"),
                   col = c("black", "red", "orange", "purple","blue", "red"),
                   pch = c(16,16,16,16,NA,NA),
                   lty = c(NA,NA,NA,NA,1,2),
                   bty = "n",
                   xpd=NA)
          }


        }
      }
    }

  }


  if (filetype == "pdf") {
    pdf(file = paste(filename,".pdf", sep=''),
        width = w,
        height = n*h)
    plot.it()
    dev.off()
  }

  if (filetype == "emf") {
    library(devEMF)
    emf(file = paste(filename, ".emf", sep=""), width = w, height = n*h, family = "serif")
    plot.it()
    dev.off()
  }

  if (filetype == "png") {
    png(file = paste(filename, ".png", sep=""), units = "in", res = 300, width = w, height = n*h)
    plot.it()
    dev.off()
  }

  if (filetype == "svg") {
    svg(filename = paste(filename, ".svg", sep=""), width = w, height =n*h, family = "serif")
    plot.it()
    dev.off()
  }


  if (output) plot.it()

}
