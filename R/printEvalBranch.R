#' @title printEvalBranch
#'
#' @description
#' Prints a formatted summary of information from an `EvalBranch` object or a list containing `EvalBranch` objects. 
#' 
#' @param object An `EvalBranch` object or a list containing `EvalBranch` objects.
#' 
#' @param compact Logical indicating a concise format (default = TRUE).
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
#' printEvalBranch(evalBranch)
#' 
#' @author Miranda-Esquivel Daniel R.

printEvalBranch <- function(objectToPrint, compact = TRUE) {

#~   # Check object class
#~   if (!any(class(object0) %in% c("EvalBranch", "EvalBranch0", "EvalBranch1"))) {
#~     return(print("Wrong class, object must be EvalBranch"))
#~   }
 

  salida <- as.data.frame(matrix(unlist(objectToPrint), ncol=5, byrow = T))

  colnames(salida) <- c("node","initialArea","FinalArea","Aproach","%Delta")

  salida <- salida[!is.na(as.numeric(salida$"%Delta")),]



  if(compact){
	  
	  salida <- salida[as.numeric(salida$"%Delta") !=  0,]
	  
	  }

  return(salida)

}

