#' @title print.swapBL-Output

#' This function takes the output of a 'blepd::swapBL' analysis and prints a summary
#' of the area with the best fit for the dispersal model. It includes the area 
#' name, frequency of occurrences in that area, and the calculated percentage.

#' @param swapBL.output A list containing the output of a 'blepd' analysis, 
#' typically of class "blepd".
#'
#' @return This function does not return a value, it prints the summary to the console.
#'
#' @examples
#' # Assuming you have your 'blepd' analysis output in 'my_blepd_output'
#' print_best_area_summary(my_blepd_output)
#'
#' @export
printswapBL <- function(swapBL.output) {
#~   # Check if the input is a valid 'blepd' analysis output (list with class "blepd")
  if (!is.list(swapBL.output) || !attr(swapBL.output, "class") %in% "blepd1") {
    stop("Input must be a list of class 'blepd1 (object created with swapBL)")
  }
  
  # Extract the 'bestModifiedArea' data frame from the output structure
  best.area <- swapBL.output$bestModifiedArea
  
  # Calculate the percentage of occurrences for each area
  best.area$Percent <- (best.area$Freq / sum(best.area$Freq)) * 100
  
  # Print "BestInitial:" followed by the area with the highest initial PD
  cat("BestInitial:", swapBL.output$bestInitialArea, "\n")
  
  # Print a rounded version (2 decimal places) of the modified 'bestArea' data frame
  # This includes AreaSelected, Freq (frequency of occurrences), and the calculated Percent
  print(round(best.area[, c("AreaSelected", "Freq", "Percent")], 2))
}
