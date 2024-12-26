#' @title printSwapBL
#' 
#' @description
#' Prints information from an `swapBL` object. 
#' The output format can be controlled using the `compact` and `tabular` arguments.
#'
#' @param objectToPrint An `swapBL` object.
#' @param compact Logical indicating whether to print a compact output.
#' @param tabular Logical indicating whether to print a tabular output.
#' @return No return value, but prints information to the console.
#' 
#' @examples
#' library(blepd)  
#' 
#' # Create a swapBL object
#' evalBranch <-  swapBL(tree=tree,
#'                       distribution = distribution,
#'                           model = "simpleswap",
#'                           branch = "terminals",
#'                           verbose = TRUE
#'                          )
#' 
#' # Print information from the object
#' printSwapBL(evalBranch)
#' 
#' @author Miranda-Esquivel Daniel R.


printSwapBL <- function(objectToPrint, compact = FALSE, tabular= TRUE) {

#~   # Check object class
#~   if (!any(class(object0) %in% c("EvalBranch", "EvalBranch0", "EvalBranch1"))) {
#~     return(print("Wrong class, object must be EvalBranch"))
#~   }
 

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



