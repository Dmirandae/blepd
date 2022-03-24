#'
#' @title swapBL
#'
#' @description The function calculates the effect on PD of swapping terminal branch lengths.  
#' 
#' @return Returns a blepd object with the frequency of each area recovered.
#'
#' @param tree is a single tree with T terminals, an APER phylo object.
#' 
#' @param distribution is a labeled matrix object, with the distribution of T terminals (columns) in A areas (rows).
#' 
#' @param model is the type of swap to be performed. Valid values are: 1. "simpleswap" -two terminal branch lengths are swapped-. 2. "allswap" (default) -branch lengths are swapped, using the sample function-, 3. "uniform" -branch lengths are replaced by uniform distributed values; min and max values are extracted from the actual lengths-.
#' 
#' @param nTimes number of times to repeat the swap.
#' 
#' @param branches to swap "terminals" (default), "internals", or "all".
#' 
#' @param root is use.root in PD function (default = FALSE).
#' 
#' 
#' @examples
#' library(blepd)
#' data(tree)
#' data(distribution)
#' swapBL(tree = tree , distribution = distribution , 
#'         model  = "allswap" , nTimes = 100 , 
#'         branch = "terminals")
#'
#'
#'@author Miranda-Esquivel Daniel R.
#'
#'

swapBL <- function( tree = tree , 
                    distribution = distribution , 
                    model  = "allswap" ,
                    nTimes = 100 ,  
                    root   = TRUE , 
                    index  = "PD" ,
                    branch = "terminals", 
                    percentual = FALSE
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
			cat("model to test",model,"reps",nTimes,"\n")
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

