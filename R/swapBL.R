#'
#' @title swapBL
#'
#' @description The function calculates the effect on PD of swapping two terminal brach lengths.  
#' 
#' @return Returns a data frame with the frequency of each area recovered.
#'
#' @param tree is a single tree with T terminals, an APER phylo object.
#' 
#' @param distribution is a labeled matrix object, with the distribution of T terminals (rows) in A areas (columns).
#' 
#' @param number to times the swap is performed.
#' 

#' 
#' 
#' @examples
#' library(blepd)
#' data(tree)
#' data(distribution)
#' swapBL(tree = tree , distribution = distribution , 
#'        times = 100)
#'
#'
#'@author Miranda-Esquivel Daniel R.
#'
#'

swapBL <- function(tree = tree , distribution = distribution , 
                   nTimes = 100, root = TRUE){

## potential errors

  
        if(!all(colnames(distribution) %in% tree$tip.label)){stop("Check names in tree / distribution. Mind the closing door")}


## in house functions

bestVal <- function(distribution = distribution, initialVal){ 

   best <- row.names(distribution)[which(initialVal == max(initialVal))]
        
   resp <- tmpBest <- gsub("area","",best)
   
   if(length(tmpBest) > 1){
   resp <- paste(tmpBest,collapse="")
   }
   
   return(resp)
}



## initial stuff from  initial tree

#        initialPD <- myPD(tree = tree, distribution = distribution, root = root)
        
        
#        bestInitialArea <- c(bestVal(distribution,initialPD))
        
        
        numberTerminals <- length(tree$tip.label)

        
        AreaSelected <- vector()
## modified tree

    for(repeticiones in 1:nTimes){
        
        numberOfTerminalSwap <- 0

        while (length(numberOfTerminalSwap) != 2){
        terminalsToSwap <- as.integer(runif(2,1,numberTerminals))
        numberOfTerminalSwap <- which(tree$edge[,2] %in% terminalsToSwap )
        }

        
        newTree <- tree 
        
        rama1 <- newTree$edge.length[numberOfTerminalSwap[1]]
        rama2 <- newTree$edge.length[numberOfTerminalSwap[2]]
        
        newTree$edge.length[numberOfTerminalSwap[1]] <- rama2
        newTree$edge.length[numberOfTerminalSwap[2]] <- rama1
        
        modifiedPD <- myPD(tree = newTree, distribution = distribution, root = root)
        
        AreaSelected[repeticiones] <-  c(bestVal(distribution,modifiedPD))

}

finaldf <- as.data.frame(table(AreaSelected))


return(finaldf)

}