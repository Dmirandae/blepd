#'
#' @title evalTerminalvsInternal
#'
#' @description This function tests whether the internal branch lengths impacts the results.  
#' 
#' @return A list of four data.frame objects, branch swapping eval for: 1. Empirical branch lengths. 2. All branches equal to 1. 3. Internal braches set to valueAssign (default=0). 4. Terminal braches set to valueAssign (default=0).
#'
#' @param tree is a single tree with T terminals, an APER phylo object. 
#' 
#' @param distribution is a labeled matrix object, with the distribution of T terminals (rows) in A areas (columns).
#' @param  useRoot is boolean (default=TRUE). 
#' @param valueAssign is  real value to place for internal and terminal branches (default=0).
#' @param  nTimes is an integwer, number of times to run the branch swapping function.
#' 
#' @examples
#' library(blepd)
#' data(tree)
#' data(distribution)
#' ## calculate values
#' 
#' data1 <- evalTerminalvsInternal(tree,distribution,nTimes=10)
#' plot1 <- lapply(data1,graficar)
#' 
#' library(gridExtra)
#' 
#' grid.arrange(
#' plot1[[1]]+ggtitle(names(data1)[1]),
#' plot1[[2]]+ggtitle(names(data1)[2]),
#' plot1[[3]]+ggtitle(names(data1)[3]),
#' plot1[[4]]+ggtitle(names(data1)[4]),
#' nrow = 2)
#'
#'
#'@author Miranda-Esquivel Daniel R.
#'
#'



evalTerminalvsInternal <- function( tree = tree,
                                   distribution = distribution,
                                   useRoot = TRUE,
                                   valueAssign = 0,
                                   nTimes = 100){

## internal functions

    .putBL    <-  function(x){
        arbol2 <- tree ; arbol2$edge.length <- x;
        return(arbol2)}

    .getPDbs <- function(x){
        swapBL(x, distribution, root=useRoot, nTimes=nTimes)}



  terminals <- getTerminals(tree)

  initialLongs <- tree$edge.length
  
  longRamas <- list()
  
  longRamas[[1]] <- initialLongs
  
  longRamas[[2]] <- rep(1,length(initialLongs))
  
  terminal1 <- initialLongs

  terminal1[terminals] <- valueAssign
  
  longRamas[[3]] <- terminal1


  terminal1 <- initialLongs

  terminal1[!terminals] <- valueAssign

  longRamas[[4]] <- terminal1


  newArboles <- lapply(longRamas,.putBL)

  PDvals <- lapply(newArboles,.getPDbs)

  t3 <- paste("Terminals ",valueAssign,collapse="")

  t4 <- paste("Internals ",valueAssign,collapse="")

  names(PDvals) <- c("Empirical","All 1",t3,t4)

  return(PDvals)
  }



