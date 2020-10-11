#'
#' @title evalTerminalvsInternal
#'
#' @description This function tests whether the internal/terminal branch lengths impact the results.  
#' 
#' @return A list of four data.frame objects, branch swapping eval for: 1. All brach lengths set to valueAssign (default=1).3. Only internal braches set to valueAssign (default=1). 4. Only terminal braches set to valueAssign (default=1).
#'
#' @param tree is a single tree with T terminals, an APER phylo object. 
#' 
#' @param distribution is a labeled matrix object, with the distribution of T terminals (rows) in A areas (columns).
#' @param  root is boolean (default=TRUE). 
#' @param valueAssign is a real value to place for internal and terminal branches (default=0).
#' @param  nTimes is an integer, number of times to run the branch swapping function.
#' 
#' @examples
#' library(blepd)
#' data(tree)
#' data(distribution)
#' ## calculate values
#' 
#' t3 <- evalTerminalvsInternal(tree, distribution, nTimes=10, valueAssign=0.5, branch="terminals")
#' print.multiBlepd(t3,tabular=FALSE)
#' 
#'
#'@author Miranda-Esquivel Daniel R.
#'
#'


evalTerminalvsInternal <- function( tree = tree,
                                   distribution = distribution,
                                   root = TRUE,
                                   valueAssign = 1,
                                   branch = "terminals",
                                   model  = "allswap" ,
                                   nTimes = 100){


  terminals <- getTerminals(tree)


  initialLongs <- tree$edge.length
  
  casosArboles <- list()
  
  newTree <- tree
  newTree$edge.length <- rep(valueAssign,length(initialLongs))
  casosArboles[[1]] <- newTree
  
  newTree <- tree
  newTree$edge.length[terminals] <- valueAssign
  casosArboles[[2]] <- newTree
  

  newTree <- tree
  newTree$edge.length[!terminals] <- valueAssign
  casosArboles[[3]] <- newTree
  


  PDvals <- lapply(casosArboles,swapBL,
                   distribution = distribution , 
                   model  = model ,
                   nTimes = nTimes ,  
                   root   = root ,
                   branch = branch
                   )
     

  names(PDvals) <- c("All","Terminals","Internals")
  
  class(PDvals) <- "multiBlepd"

  return(PDvals)

  }



