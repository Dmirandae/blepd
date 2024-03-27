#' @title swapBL

#' @description 
#' The function calculates the effect on PD of swapping terminal or internal branch lengths.

#' @return
#' Returns a `blepd` object containing:
#'   * `initialPD`: The PD value of the initial tree.
#'   * `bestInitialArea`: The area(s) with the highest PD value in the initial tree.
#'   * `bestModifiedArea`: A data frame summarizing the frequency of each area recovered after swaps.
#'   * `tree`: The original tree object.
#'   * `distribution`: The distribution matrix used for PD calculations.
#'   * `model`: The type of swap performed ("simpleswap", "allswap", or "uniform").
#'   * `nTimes`: The number of times the swap was repeated.
#'   * `root`: Whether the root was used in PD calculations.
#'   * `index`: The PD index used (currently always "PD").
#'   * `branch`: The type of branches swapped ("terminals", "internals", or "all").

#' @param tree A single phylogenetic tree with terminal labels, an `ape` phylo object.

#' @param distribution A labeled matrix object, with the distribution of terminal taxa (columns) across areas (rows).

#' @param model The type of swap to be performed. Valid values are:
#   * "simpleswap" - Two terminal branch lengths are swapped.
#   * "allswap" (default) - Branch lengths are swapped within the specified branch type, using the `sample` function.
#   * "uniform" - Branch lengths are replaced by uniformly distributed values between the minimum and maximum values of the original lengths.

#' @param nTimes The number of times to repeat the swap.

#' @param branch The type of branches to swap: "terminals" (default), "internals", or "all".

#' @param root Whether to use the root node in PD calculations (default = FALSE).

#' @param verbose Logical indicating whether to print messages during execution (default = TRUE).

#' @examples
#' library(blepd)
#' data(tree)
#' data(distribution)
#' swapBL(tree = tree, distribution = distribution, model = "allswap", nTimes = 100, branch = "terminals")

#' @author Miranda-Esquivel Daniel R.



swapBL <- function( tree = tree , 
                    distribution = distribution , 
                    model  = "allswap" ,
                    nTimes = 100 ,  
                    root   = TRUE , 
                    index  = "PD" ,
                    branch = "terminals", 
                    percentual = FALSE,
                    verbose = TRUE
                    ){					   
					
					  model   <- tolower(model)
					  
					  branch  <- tolower(branch) 

## potential errors
  
        if( !all(colnames(distribution) %in% tree$tip.label) ){
			stop("Check names in tree / distribution. Mind the closing door.")
			}
        
        if ( (model %in%  c("simpleswap","allswap","uniform")) &
             (branch %in% c("terminals","internals","all"))
           ){
			if( verbose ){cat("model to test",model,"reps",nTimes,"\n")}
			}else{
				stop("Check models/branch selection. Mind the closing door.")
				}
				
		if( any(apply(distribution,2,sum)==1) ){
			root = TRUE
			}		
        

## initial stuff from the initial tree

        initialPD          <-  PDindex(tree = tree, distribution = distribution, root = root)
               
        bestInitialArea    <-   c(.bestVal(distribution,initialPD))
                 
        numberTerminals    <-   length(tree$tip.label)
        
        AreaSelected       <-   vector()
        
        terminals          <-   getTerminals(tree)
        
     
			if ( branch == "terminals" ) {nodos <- terminals}
			if ( branch == "internals" ) {nodos <- !terminals}
			if ( branch == "all" )       {nodos <- terminals 
				                          nodos[1:length(terminals)] <- TRUE}

			
## modified tree

    for(repeticiones in 1:nTimes){
        
        if (model == "simpleswap"){ 
        
          newTree              <-   tree

          twoEdges             <-   sample(which(nodos),2)

          newTree$edge.length  <-   .swtch(tree$edge.length,twoEdges[1],twoEdges[2])        

		}
		
	        
        if (model == "allswap"){ 

          newTree                      <-    tree

		  newTree$edge.length[nodos]   <-    sample(tree$edge.length[nodos])
                     
	}
	
        if (model == "uniform"){ 
						
        newTree                    <-  tree 
           
        minLong                    <-  min(newTree$edge.length[nodos])
        
        maxLong                    <-  max(newTree$edge.length[nodos])
        
        valoresUnif                <-  runif(sum(nodos),minLong,maxLong)
        
        newTree$edge.length[nodos] <-  valoresUnif 
        
	}
			
        modifiedPD                   <-  PDindex(tree = newTree, distribution = distribution, root = root)
        
        AreaSelected[repeticiones]   <-  c(.bestVal(distribution,modifiedPD))

}

        finaldf                        <-  as.data.frame(table(AreaSelected))

        niveles                        <-  levels(finaldf$AreaSelected)

        levels(finaldf$AreaSelected)   <-  niveles
        
        resultados                     <-  list()
        
          resultados$initialPD         <-   initialPD
          resultados$bestInitialArea   <-   bestInitialArea
          resultados$bestModifiedArea  <-   finaldf
          resultados$tree              <-   tree 
          resultados$distribution      <-   distribution  
          resultados$model             <-   model 
          resultados$nTimes            <-   nTimes 
          resultados$root              <-   root
          resultados$index             <-   index
          resultados$branch            <-   branch
        
        class(resultados) <- "blepd"

  return(resultados)

}

