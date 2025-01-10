#' @title Print Information from an EvalBranch Object
#'
#' @description
#' Prints a formatted summary of information from an `EvalBranch` object or a list 
#' containing `EvalBranch` objects. 
#' 
#' @param object An `EvalBranch` object or a list containing `EvalBranch` objects.
#'
#' @param compact Logical indicating whether to print a concise format (default = TRUE).
#'
#' @return Invisibly returns the `object`.
#' 
#' @examples
#' library(blepd)  
#' 
#' # Create an EvalBranch object: Evaluate the effect of increasing the length of terminal branches
#'
#' result_upper <- evalBranch(tree = tree, distribution = distribution, 
#'                          branchToEval = "terminals", approach = "upper") 
#' 
#' # Print information from the object
#' 
#' printEvalBranch(result_upper)
#' 
#' @author Miranda-Esquivel Daniel R.
#' 

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

