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
#' @param model type of swap to be performed. Valid values are "simpleswap" -two terminal branch lengths are swapped- ,"allswap" -all terminal branch lengths are swapped-, and "uniform"-all terminal branch lengths are replaced by uniform distributed values; min and max values are extracted from the actual lengths-.
#' 
#' @param number of times to repeat the swap.
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
                   model= "simpleswap" ,
                   nTimes = 100,  root = TRUE, index = "PD"){

## potential errors

`%nin%` = Negate(`%in%`)
  
        if(!all(colnames(distribution) %in% tree$tip.label)){stop("Check names in tree / distribution. Mind the closing door.")}
        
        if (model %in% c("simpleswap","allswap","uniform") ){cat("model to test",model,"reps",nTimes,"\n")}else{stop("Check models. Mind the closing door.")}
        



## initial stuff from  initial tree

        initialPD <- PDindex(tree = tree, distribution = distribution, root = root)
               
        bestInitialArea <- c(.bestVal(distribution,initialPD))
                
        numberTerminals <- length(tree$tip.label)
        
        AreaSelected <- vector()
        
## modified tree

    for(repeticiones in 1:nTimes){
        
        if (model == "simpleswap"){ 
        
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

	}
	        
        if (model == "allswap"){ 
                
        newTree <- tree 
                
        numberOfTerminalSwap <- which(tree$edge[,2] %in% 1:numberTerminals )
        
        blOriginal <- newTree$edge.length[c(numberOfTerminalSwap)]
        
        blSampled <- sample(blOriginal)
        
        newTree$edge.length[c(numberOfTerminalSwap)] <- blSampled
	}
	
        if (model == "uniform"){ 
						
        newTree <- tree 
        
        numberOfTerminalSwap <- which(tree$edge[,2] %in% 1:numberTerminals )
        
        minLong <- min(newTree$edge.length[c(numberOfTerminalSwap)])
        
        maxLong <- max(newTree$edge.length[c(numberOfTerminalSwap)])
        
        valoresUnif <- runif(numberTerminals,minLong,maxLong)
        
        newTree$edge.length[c(numberOfTerminalSwap)] <- valoresUnif 
        
	}
		
        modifiedPD <- PDindex(tree = newTree, distribution = distribution, root = root)
        
        AreaSelected[repeticiones] <-  c(.bestVal(distribution,modifiedPD))

}

    finaldf <- as.data.frame(table(AreaSelected))

    finaldf <- as.data.frame(table(AreaSelected))

    niveles <- levels(finaldf$AreaSelected)

    numero <- which(niveles == bestInitialArea)

    niveles[numero] <- paste(niveles[numero],"*")

    levels(finaldf$AreaSelected) <- niveles



return(finaldf)
}
