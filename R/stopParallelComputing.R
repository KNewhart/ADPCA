# End parallel computing

# ----------------------------------------------
# Deluxe doSNOW cluster setup in R
# Tobias Kind (2015)
# https://github.com/tobigithub/R-parallel/wiki/
# ----------------------------------------------
stopParallelComp <- function() {
  # stop cluster and remove clients
  stopCluster(cluster); print("Cluster stopped.")

  # insert serial backend, otherwise error in repetetive tasks
  registerDoSEQ()

  # clean up a bit.
  invisible(gc); remove(nCores); remove(nThreads); remove(cluster);

}

