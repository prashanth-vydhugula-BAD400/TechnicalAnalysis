# Student Name: Prashanth Vydhugula
# Course: BDA400
# Assignment 5 - Crossover Function

crossover <- function(arr1, arr2) {

  # Check if both arrays have the same length
  if (length(arr1) != length(arr2)) {
    stop("Both arrays should have the same length")
  }

  # Initialize crossover signals
  crossover_signals <- rep("None", length(arr1))

  # Check for crossover events
  if (length(arr1) >= 2) {

    for (i in 2:length(arr1)) {

      if (arr1[i] > arr2[i] &&
          arr1[i - 1] <= arr2[i - 1]) {

        crossover_signals[i] <- "Up"

      } else if (arr1[i] < arr2[i] &&
                 arr1[i - 1] >= arr2[i - 1]) {

        crossover_signals[i] <- "Down"

      } else {

        crossover_signals[i] <- "None"
      }
    }
  }

  return(crossover_signals)
}
