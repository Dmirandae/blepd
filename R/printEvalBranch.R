#' @title printevalBranch
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



printEvalBranch <- function(objectToPrint, compact = TRUE) {

#~   # Check object class
#~   if (!any(class(object0) %in% c("EvalBranch", "EvalBranch0", "EvalBranch1"))) {
#~     return(print("Wrong class, object must be EvalBranch"))
#~   }
 

  salida <- as.data.frame(matrix(unlist(objectToPrint), ncol=5, byrow = T))

  colnames(salida) <- c("node","initialArea","FinalArea","Aproach","%Delta")

  if(compact){
	  
	  salida <- salida[as.numeric(salida$"%Delta") !=  0,]
	  
	  }

  return(salida)

}

