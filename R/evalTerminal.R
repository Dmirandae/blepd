#'
#' @title evalTerminal
#'
#' @description This function swaps the lengths of two edges in a phylogenetic tree and evaluates the resulting change in the selected area based on a specific index (e.g., PD). It performs multiple repetitions (nTimes) to assess the potential impact of edge length swapping on the chosen index.
#'
#' @param tree A phylogenetic tree object (e.g., from the `ape` package).
#' 
#' @param distribution A labeled matrix object representing the distribution of 
# terminals across areas.
#' 
#' @param model The type of edge swapping to perform:
#'   * "simpleswap": Swaps the lengths of two randomly selected edges.
#'   * "allswap": Swaps the lengths of all possible edge pairs (computationally 
#     expensive for large trees).
#'   * "uniform": Randomly assigns new edge lengths from a uniform distribution 
#     within the range of existing edge lengths.
#' 
#' @param nTimes The number of repetitions to perform for the chosen swapping model.
#' 
#' @param root A logical value indicating whether to use the root edge in the swapping #' process (default: TRUE). 
#' 
#' @param verbose A logical value indicating whether to print progress information  during the execution (default: TRUE).
#' 
#' @return A list containing the following elements:
#'   * initialPD: The initial value of the chosen index (e.g., PD) calculated for the original tree.
#'   * bestInitialArea: The area with the highest value in the initial PD calculation.
#'   * bestModifiedArea: A data frame summarizing the results of each repetition, including the modified PD, selected area, and delta (percentage change) compared to the initial area.
#'   * tree: The original tree object.
#'   * distribution: The original distribution matrix.
#'   * model: The model used for edge swapping.
#'   * nTimes: The number of repetitions performed.
#'   * root: The logical value indicating whether the root edge was included in swapping.
#'   * index: The index used for evaluation (e.g., "PD").
#'
#' @examples
#' library(blepd)  # Assuming blepd provides PDindex function
#' data(tree)
#' data(distribution)
#' 
#' # Run swapBL with different models
#' result1 <- swapBL(tree, distribution, model = "simpleswap", nTimes = 10)
#' result2 <- swapBL(tree, distribution, model = "allswap", nTimes = 10)
#' result3 <- swapBL(tree, distribution, model = "uniform", nTimes = 10)
#'
#' # Print information from the results
#' print(result1)
#' print(result2)
#' print(result3)
#'
#'@author Miranda-Esquivel Daniel R.
#'
#'

evalTerminal <- function(tree          = tree , 
                         distribution  = distribution , 
                         tipToEval     = "taxB" , 
                         approach      = "lower" , 
                         root          = FALSE ,
                         index         = "PD",
                         maxMultiplier = 1.01,
                         redondeo      = 2,
                         verbose       = FALSE  ){

if(any(apply(distribution,2,sum)==1)){root = TRUE}

##if(debugDRME){cat("\n inicio en terminal:",tipToEval,":",approach,"\n")}

## potential errors

        .checkInput(tree = tree , distribution = distribution)

        numberTipToEval <- which(tree$tip.label %in% tipToEval) 



## loop for all

     if (tipToEval == "all"){
		 
		 nombres <- tree$tip.label
		 
		 resultadosTotales <- list()
		 
		 for(nombre in nombres){
			 
			 numeroNombre <- which(nombres == nombre)
			 
			 resultadosTotales[[numeroNombre]]  <-  evalTerminal(tree = tree , 
		    	 		                            distribution = distribution , 
			 		                                tipToEval = nombre , 
			 		                                approach = approach ,
			 		                                root = root ,
			 		                                index = index ,
			 		                                maxMultiplier = maxMultiplier )
			}
		
		resultadosTotales[[numeroNombre]]$terminalEvaluated <- nombre 
					 
		class(resultadosTotales) <- "multiBlepd"		 
		
		return(resultadosTotales)		 
		 
		}  ## end loop for all
		 

        
        if (is.na(numberTipToEval)){
			stop("Check names in tree / distribution. Mind the closing door")
			}

        if(!all(colnames(distribution) %in% tree$tip.label)){
			stop("Check names in tree / distribution. Mind the closing door")
			}

        
## initial stuff

        initialPD <- PDindex( tree = tree, 
                              distribution = distribution, 
                              root = root, 
                              index = index )
        
#~         initialPD[is.na(initialPD)] <-   0.0
        
                
        bestInitialArea <- c(.bestVal(distribution,initialPD))
        
        initialLength <- round(tree$edge.length[which(.createTable(tree)[,2] %in% numberTipToEval)],4)
       
               
## initial test, branch length equal to zero
        
        newTree <- tree
    
        if (tolower(approach) %in% c("lower") ){
			
            newTree$edge.length[which(.createTable(tree)[,2] %in% numberTipToEval)] <-  0
            
            maxPD <- 0 ##+ 0.001
            
			}
			
                
        if (tolower(approach) %in% c("upper") ){
			
			maxVal <- maxMultiplier * round(sum(tree$edge.length),6)
			
			newTree$edge.length[which(.createTable(tree)[,2] %in% numberTipToEval)] <-  maxVal
                        
            maxPD <- max(initialPD) - min(initialPD)
            	
			}
        
            
                
        modifiedPD <- PDindex( tree = newTree, 
                               distribution = distribution, 
                               root = root, 
                               index = index )
          
        bestModifiedArea <-  c(.bestVal(distribution,modifiedPD))
        
              
        if(all(bestInitialArea %in% bestModifiedArea) &
           all(bestModifiedArea %in% bestInitialArea)){
			   
            
			   promedio <- initialLength

            
            ans <- list (maxPD            =   maxPD, 
                         areas            =   rownames(distribution),
                         terminals        =   colnames(distribution),
                         bestInitialArea  =   bestInitialArea, 
                         bestModifiedArea =   bestModifiedArea,
                         modifiedPD       =   modifiedPD,
                         initialPD        =   initialPD,
                         initialLength    =   initialLength,
                         root             =   root,
                         tipToEval        =   tipToEval, 
                         approach         =   approach, 
                         index            =   index,
                         finalLength      =   promedio,
                         delta            =   round((( promedio - initialLength ) /
                                                       initialLength ) * 100 , redondeo )
                         )
            
            if (verbose){
				cat("\nTerminal: ",tipToEval,
				     ", has NO effect on branch length (0 or Max)",sep="")
				 }
            
            class(ans) <- c("multiBlepd","EvalTerminal") 
                                      
            return(ans)
            
            ## break("got it")
        }
                        
        ## end of estimate branch  zero or max. length         
    
        
        ## let's continue
        
        ## divide and conquer loop 
        
    if (tolower(approach) == "lower"){
    
        ValorPrevio    <-  9999999999
                
        initial        <-  0.0
        
        final          <- initialLength
        }


    if (tolower(approach) == "upper"){
    
        ValorPrevio    <-  9999999999
                
        initial        <-  initialLength+(initialLength/100)
        
        final          <-   maxVal
        }


    repeat{ promedio <- mean(c(final,initial))
                  
            newTree$edge.length[which(.createTable(tree)[,2] %in% numberTipToEval)] <-  promedio
            
            reCalculatedPD  <- PDindex( tree = newTree, 
                                        distribution = distribution, 
                                        root = root, 
                                        index = index )
            
            bestModifiedArea <-  c(.bestVal(distribution,reCalculatedPD))
        

     
    if(round(promedio,6) != round(ValorPrevio,6)) { 
        
        ValorPrevio <- promedio
        
     if((all(bestInitialArea %in% bestModifiedArea)) &
        (all(bestModifiedArea  %in%  bestInitialArea))){
   
        if (tolower(approach) == "lower"){
			final <- promedio
			}
      
        if (tolower(approach) == "upper"){
			inicial <- promedio            
            }
        
        }else{
              
        if (tolower(approach) == "lower"){
			initial <- promedio
			}

        if (tolower(approach) == "upper"){
			final <- promedio
			}
            
            }
    
        }else{
         
        if (tolower(approach) == "lower"){
            
            promedio <- promedio - (promedio/100)
            
            newTree$edge.length[which(.createTable(tree)[,2] %in% numberTipToEval)] <-  promedio
            
            reCalculatedPD  <- PDindex( tree = newTree, 
                                        distribution = distribution, 
                                        root = root, 
                                        index = index )
            
            bestModifiedArea <-  c(.bestVal(distribution,reCalculatedPD))
            
            
            ans <- list (maxPD            =   maxPD , 
                         areas            =   rownames(distribution),
                         terminals        =   colnames(distribution),
                         bestInitialArea  =   bestInitialArea, 
                         bestModifiedArea =   bestModifiedArea,
                         modifiedPD       =   modifiedPD,
                         initialPD        =   initialPD,
                         initialLength    =   initialLength,
                         root             =   root,
                         tipToEval        =   tipToEval, 
                         approach         =   approach, 
                         index            =   index,
                         finalLength      =   promedio,
                         delta            =   round((( promedio - initialLength ) / 
                                                       initialLength ) * 100 , redondeo )      
                         )
            
            class(ans) <- c("multiBlepd","EvalTerminal")                       
            
            return(ans)
            
            
            ## break("got it")
        }
        
        if ((tolower(approach) == "upper")  &
        !all(bestInitialArea %in% bestModifiedArea)){
            
            promedio <- promedio + (promedio/100)
            
            ans <- list (maxPD            =   maxPD , 
                         areas            =   rownames(distribution),
                         terminals        =   colnames(distribution),
                         bestInitialArea  =   bestInitialArea, 
                         bestModifiedArea =   bestModifiedArea,
                         modifiedPD       =   modifiedPD,
                         initialPD        =   initialPD,
                         initialLength    =   initialLength,
                         root             =   root,
                         tipToEval        =   tipToEval, 
                         approach         =   approach, 
                         finalLength      =   promedio,
                         delta            =   round((( promedio - initialLength ) / 
                                                       initialLength ) * 100 , redondeo )                  
                         )
      
            
            class(ans) <- c("multiBlepd","EvalTerminal")                       
            
            return(ans)
            
            ## break("got it")
            
        }else{
			initial <- promedio
			}
                      
    }
    
    } 
    ## end  repeat
            
    }
        
## end best
    
