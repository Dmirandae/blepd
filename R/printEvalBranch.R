#' @title print.evalBranchAll
#' 
#' @description
#' Prints information from an `EvalBranch` object or a list containing 
#' `EvalBranch` objects. It summarizes the branch evaluated, initial area, 
#' modified area after evaluation, and delta value (percentage change). 
# 
#' @param object0 An `EvalBranch` object or a list containing `EvalBranch` objects.
#' 
#' @param compact A logical value indicating whether to print the information 
#' in a compact format (e.g., branch number instead of full branch string) 
#' (default = TRUE).
#' 
#' @return Prints the formatted information to the console. 
#' 
#' @examples
#' library(blepd)  
#' 
#' # Create an EvalBranch object
#' evalBranch <- EvalBranch(branchToEval = "Branch1", 
#                           bestInitialArea = 0.3, 
#                           bestModifiedArea = 0.4, 
#                           delta = 10)
#' 
#' # Print information from the object
#' print.evalBranchAll(evalBranch)
#' 
#' # Create a list of EvalBranch objects
#' multiEvalResults <- list(evalBranch, evalBranch)
#' 
#' # Print information from the list
#' print.evalBranchAll(multiEvalResults, compact = FALSE)



print.evalBranchAll <- function(object0, compact = TRUE) {

  # Check object class
  if (!any(class(object0) %in% c("EvalBranch", "EvalBranch0", "EvalBranch1"))) {
    return(print("Wrong class, object must be EvalBranch"))
  }



  # Helper function to prepare output based on compactness
  prepareOutput <- function(x, object) {
    branch <- if (compact) {
      strsplit(object[[x]]$branchToEval, split = ":")[[1]][2]
    } else {
      unlist(object[[x]]$branchToEval)
    }
    cat(branch,
         unlist(object[[x]]$bestInitialArea),
         unlist(object[[x]]$bestModifiedArea),
         unlist(object[[x]]$delta), "\n", sep = "\t")
}



 # Print information for single EvalBranch object
  if (any(class(object0) == "EvalBranch1")) {
    if (any(class(object0[[1]]) == "EvalBranch")) {
      Entries <- 1:length(object0)
      inT <- if (compact) "\nBranchNumber" else "\nTerminalsInBranch"

      cat("\nApproach:", unlist(object0[[1]]$approach), inT, "initArea", "Modified", "Delta%\n", sep = "\t")
      cat(unlist(sapply(Entries, prepareOutput, object = object0)), "\n\n")
    }
  }



  # Helper function to prepare output for list of EvalBranch objects with upper/lower branches
  prepareOutput2 <- function(x, object) {
    branch <- if (compact) {
      strsplit(object$upper[[x]]$branchToEval, split = ":")[[1]][2]
    } else {
      unlist(object$upper[[x]]$branchToEval)
    }
    cat(branch,
         unlist(object$upper[[x]]$bestInitialArea),
         unlist(object$upper[[x]]$bestModifiedArea),
         unlist(object$upper[[x]]$delta),
         unlist(object$lower[[x]]$bestModifiedArea),
         unlist(object$lower[[x]]$delta), "\n", sep = "\t")
  }

	


  # Print information for list of EvalBranch objects with upper/lower branches
  if (any(names(object0) %in% c("lower", "upper"))) {
    Entries <- 1:length(object0$lower)
    inT <- if (compact) "\nBranchNumber" else "\nTerminalsInBranch"

    cat(inT, "initArea", "Mod-upper", "Delta%", "Mod-lower", "Delta%\n", sep = "\t")
    cat(unlist(sapply(Entries, prepareOutput2, object = object0)), "\n\n")
  }
}



