#' @title swapBL: Analyze Effect of Branch Length Swapping on Phylogenetic Diversity

#' @description
#' This function evaluates how swapping terminal or internal branch lengths affects Phylogenetic Diversity (PD) in a phylogenetic tree. It achieves this by simulating branch length modifications and assessing the resulting changes in PD.
#' 
#' @param tree (required): A phylogenetic tree object in the APER format, containing terminal labels (tips).
#' @param distribution (required): A matrix representing the distribution of terminal taxa across sets (rows represent sets, columns represent terminal taxa). Ensure the column names in this matrix match the terminal labels (tips) in the tree.
#' @param model (optional, default="allswap"): The type of branch length swapping to perform. Valid options are:
#'    "simpleswap": Swaps the lengths of two randomly chosen branches.
#'    "allswap" (default): Randomly permutes the branch lengths within the specified branch scope (defined by the `branch` argument).
#'    "uniform": Replaces the branch lengths of chosen nodes with random values drawn from a uniform distribution between the minimum and maximum values of the original lengths.
#' @param nTimes (optional, default=100): The number of times to repeat the swapping process (iterations).
#' @param branch (optional, default="terminals"): The type of branches to swap lengths for. Valid options are:
#'    "terminals": Swaps lengths among terminal branches.
#'    "internals": Swaps lengths among internal branches.
#'    "all": Swaps lengths among all branches (terminals and internals).
#' @param root (optional, default=TRUE): A logical value indicating whether to use the root node in PD calculations.
#' @param verbose (optional, default=TRUE): A logical value indicating whether to print informative messages during function execution.
#' @param compact (optional, default=TRUE): A logical value determining the output content. If `FALSE`, all available information is returned. If `TRUE` (default), a more concise output is provided.

#' @return
# The function returns a list containing the following information:
#' 
#'  `initialPD`: The PD value of the original tree.
#' 
#'  `bestInitialArea`: The set(s) with the highest PD in the initial tree.
#' 
#'  `bestModifiedArea`: A data frame summarizing the frequency of each set identified with the highest PD across iterations.
#' 
#'  `model`: The type of branch length swapping performed ("simpleswap", "allswap", or "uniform").
#' 
#'  `nTimes`: The number of times the swapping process was repeated.
#' 
#'  `branch`: The type of branches swapped ("terminals", "internals", or "all").
#' 
#'  `root` (optional): A logical value indicating whether the root node was used during PD calculations (default: TRUE).
#' 
#'  `index` (optional): The PD index used (currently always "PD").
#' 
#' @examples
#' 
#' library(blepd)
#' 
#' data(tree)
#' 
#' data(distribution)
#' 
#' # Evaluate the effect of swapping all terminal branch lengths 100 times
#' 
#' swapBL(tree = tree, distribution = distribution, model = "allswap", nTimes = 100, branch = "terminals") 
#' 
#' @author Miranda-Esquivel Daniel R.
#' 

swapBL <- function( tree = tree , 
                    distribution = distribution , 
                    model  = "allswap" ,
                    nTimes = 100 ,  
                    root   = TRUE , 
                    index  = "PD" ,
                    branch = "terminals", 
                    percentual = FALSE,
                    verbose = TRUE,
                    compact = TRUE
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
				
#~ 		if( any(apply(distribution,2,sum)==1) ){
#~ 			root = TRUE
#~ 			}		
        

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
          resultados$model             <-   model 
          resultados$nTimes            <-   nTimes 
          resultados$branch            <-   branch
          
          if (!compact){
            resultados$tree              <-   tree 
            resultados$root              <-   root
            resultados$distribution      <-   distribution  
            resultados$index             <-   index
	      }
        
        class(resultados) <- "blepd1"

  return(resultados)

}

