#'
#' @title swapBL
#'
#' @description The function calculates the effect on PD of swapping terminal branch lengths.  
#' 
#' @return Returns a data frame with the frequency of each area recovered.
#'
#' @param tree is a single tree with T terminals, an APER phylo object.
#' 
#' @param distribution is a labeled matrix object, with the distribution of T terminals (rows) in A areas (columns).
#' 
#' @param model is the type of swap to be performed. Valid values are "simpleswap" -two terminal branch lengths are swapped- ,"allswap" -default, all terminal branch lengths are swapped-, and "uniform" -all terminal branch lengths are replaced by uniform distributed values; min and max values are extracted from the actual lengths-.
#' 
#' @param number of times to repeat the swap.
#' 
#' @param branch to swap "terminals" (default) or "internals".
#' 

#' 
#' 
#' @examples
#' library(blepd)
#' data(tree)
#' data(distribution)
#' swapBL(tree = tree , distribution = distribution , 
#'        times = 100 , branch = "terminals")
#'
#'
#'@author Miranda-Esquivel Daniel R.
#'
#'

swapBL <- function(tree = tree , 
                   distribution = distribution , 
                   model  = "allswap" ,
                   nTimes = 100 ,  
                   root   = TRUE , 
                   index  = "PD" ,
                   branch = "terminals"
                   ){

## potential errors
  
        if(!all(colnames(distribution) %in% tree$tip.label)){
			stop("Check names in tree / distribution. Mind the closing door.")
			}
        
        if ( (model %in%  c("simpleswap","allswap","uniform")) &
             (branch %in% c("terminals","internals"))
           ){
			cat("model to test",model,"reps",nTimes,"\n")
			}else{
				stop("Check models/branch selection. Mind the closing door.")
				}
				
		if(any(apply(distribution,2,sum)==1)){
			root = TRUE
			}		
        



## initial stuff from  initial tree

        initialPD         <-  PDindex(tree = tree, distribution = distribution, root = root)
               
        bestInitialArea   <-  c(.bestVal(distribution,initialPD))
                
        numberTerminals   <-  length(tree$tip.label)
        
        AreaSelected      <-  vector()
        
        terminals         <-   getTerminals(tree)
        
     
			if (branch == "terminals") {
				nodos <- terminals
				}else{
				nodos <- !terminals
				}



        
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
        
        AreaSelected[repeticiones]    <-  c(.bestVal(distribution,modifiedPD))

}

        finaldf                        <-  as.data.frame(table(AreaSelected))

        niveles                        <-  levels(finaldf$AreaSelected)

      ##  numero                       <-  which(niveles == bestInitialArea)

      ##  niveles[numero]              <-  paste(niveles[numero],"*")

        levels(finaldf$AreaSelected)   <-  niveles
        
        resultados  <- list()
        
        resultados$initialPD           <-   initialPD
        resultados$bestInitialArea     <-   bestInitialArea
        resultados$selected            <-   finaldf
        resultados$tree                <-   tree 
        resultados$distribution        <-   distribution  
        resultados$model               <-   model 
        resultados$nTimes              <-   nTimes 
        resultados$root                <-   root
        resultados$index               <-   index
        resultados$branch              <-   branch

  return(resultados)

}
