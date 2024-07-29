#' @title printevalBranch
#' 
#' @description
#' Prints information from an `EvalBranch` object or a list containing 
#' `EvalBranch` objects. It summarizes the branch evaluated, initial area, 
#' modified area after evaluation, and delta value (percentage change). 
# 
#' @param object0 An `EvalBranch` object.
#' 
#' @param compact A logical value indicating whether to print the information 
#' in a compact format (e.g., branch number instead of full branch string) 
#' (default = TRUE).
#' 
#' @param tabular A logical value indicating whether to print the information 
#' in as a data.frame (default = TRUE).
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


printEvalBranch <- function(objectToPrint, compact = FALSE, tabular= TRUE) {

#~   # Check object class
#~   if (!any(class(object0) %in% c("EvalBranch", "EvalBranch0", "EvalBranch1"))) {
#~     return(print("Wrong class, object must be EvalBranch"))
#~   }
 


#### staRt PRINTING FUNCTION


initialMinimal <- objectToPrint$bestInitialArea ## compaCT

initialExtended <- data.frame(         ## no compact
  Initial = objectToPrint$bestInitialArea,
  Branch = objectToPrint$branch,
  Model = objectToPrint$model)

bestModified <- objectToPrint$bestModifiedArea

nameAreas     <- bestModified[,1]
valueAreas    <- bestModified[,2]
percentAreas  <- round(valueAreas/objectToPrint$nTimes*100,2)

# bestModifiedTabularValue   <- data.frame(t(valueAreas))
bestModifiedTabular <- data.frame(t(percentAreas))

names(bestModifiedTabular) <- nameAreas 


totalTabular <- merge(initialExtended,bestModifiedTabular)


if(tabular  & compact)  print(bestModifiedTabular)

if(tabular  & !compact)	print(totalTabular)

if(!tabular & compact)	print(bestModified)

if(!tabular & !compact)	print(t(totalTabular))
						

} ## end printing



