#'
#' @title evalTerminalvsInternal
#'
#' @description This function tells the impact of a given internal/terminal branch length in the results, when a fixed value is assigned to terminal or internal branch lengths.  
#' 
#' @return A list of three data.frame objects, branch swapping eval for: 1. All brach lengths set to valueAssign (default=1). 2. Only internal braches set to valueAssign (default=1). 3. Only terminal braches set to valueAssign (default=1).
#'
#' @param tree is a single tree with T terminals, an APER phylo object. 
#' 
#' @param distribution is a labeled matrix object, with the distribution of T terminals (columns) in A areas (rows).
#' 
#' @param root is use.root in PD function (default=TRUE). 
#' 
#' @param valueAssign is a real value to place for internal and terminal branches (default=0).
#' 
#' @param  nTimes is an integer, number of times to run the branch swapping function (default=100).
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
 
  PDvals[[1]]$evaluated <- "All equal"
 
  PDvals[[2]]$evaluated <- "Terminals"
 
  PDvals[[3]]$evaluated <- "Internals"
  

  class(PDvals) <- "multiBlepd"

  return(PDvals)

  }



