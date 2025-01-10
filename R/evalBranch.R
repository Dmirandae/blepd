#' @title evalBranch
#'
#' @description 
#' This function evaluates the effect of modifying a single branch length on 
#' Phylogenetic Diversity (PD) and the selection of sets with the highest PD. 
#' It allows for both decreasing (to zero) and increasing (to a user-defined maximum). 
#'
#' @param tree A single phylogenetic tree in APER format.
#' @param distribution A matrix indicating the distribution of terminal taxa across sets.
#' @param branchToEval The branch(es) to evaluate (can be a numeric vector of node 
#'                    numbers, "terminals", "internals", or "all", default = "terminals").
#' @param approach The type of branch length modification: 
#'                    "lower" (decrease to zero), "upper" (increase to maximum), 
#'                    or "all" for both (default = "upper").
#' @param root Logical indicating whether to use the root in PD calculation (default = FALSE).
#' @param index The index used for PD calculation (e.g., "PD").
#' @param maxMultiplier Multiplier for determining the upper limit of branch length 
#'                    modification (BL_sum * maxMultiplier, default = 1.01).
#' @param redondeo Number of decimal places for rounding.
#' @param verbose Logical indicating whether to print verbose output (default = FALSE).
#' @param compact Logical indicating whether to return a compact output (default = TRUE).
#' @param printNames Logical indicating whether to print terminal names in output.
#'
#' @return 
#'  The function returns a list containing information about the effect of branch length modifications on set selection, including:
#'
#'   `branchToEval`: The evaluated branch(es).
#'
#'   `bestInitialArea`: The set(s) with the highest PD in the initial tree.
#'
#'   `bestModifiedArea`: The set(s) with the highest PD in the modified tree.
#'
#'   `approach`: The type of branch length modification used.
#'
#'   `delta`: The percentage change in branch length.
#'
#'   `initialPD`: The initial PD value.
#'
#'   `modifiedPD`: The PD value after the branch length modification.
#'
#'   `initialLength`: The initial branch length.
#'
#'   `finalLength`: The final branch length after modification.
#'
#'   `areas`: A vector of set names.
#'
#'   `terminals`: A vector of terminal taxa names.
#'
#'   `root`: Logical indicating whether the root was used in PD calculation.
#'
#'   `index`: The PD index used.
#'
#'
#' @examples
#' library(blepd) 
#' data(tree)
#' data(distribution)
#' 
#' # Evaluate the effect of increasing the length of terminal branches
#'
#' result_upper <- evalBranch(tree = tree, distribution = distribution, 
#'                          branchToEval = "terminals", approach = "upper") 
#'

#' @author Miranda-Esquivel Daniel R.
#' @export
#'

evalBranch   <- function(tree          = tree , 
                         distribution  = distribution , 
                         branchToEval  = "terminals" , 
                         approach      = "upper" , 
                         root          = FALSE ,
                         index         = "PD" ,
                         maxMultiplier = 1.01 ,
                         redondeo      = 3 ,
                         verbose       = FALSE ,
                         compact       = TRUE ,
                         printNames    = FALSE){

#~ cat ("\names",printNames,"\n")
## potential errors

#~         if (is.na(branchToEval)){
#~ 			stop("Check names in tree / distribution. Mind the closing door")
#~ 			}

        if(!all(colnames(distribution) %in% tree$tip.label)){
			stop("Check names in tree / distribution. Mind the closing door")
			}


       #!!! .checkInput(tree = tree , distribution = distribution)


if(any(apply(distribution,2,sum)==1)){root = TRUE}

       
## loop for all approach

     if (tolower(approach) == "all"){
		 
		 resultadosParciales <- list()
		 
		 for (approaches in c("lower","upper")){
			 
          if(verbose){cat("\n\tApproach used **",approaches,"**\n")}
          
			 resultadosParciales[[approaches]]  <-  evalBranch(tree = tree , 
		    	 		                            distribution = distribution , 
			 		                                branchToEval = branchToEval  , 
			 		                                approach = approaches,
			 		                                root = root ,
			 		                                index = index ,
			 		                                maxMultiplier = maxMultiplier,
			 		                                printNames = printNames )			 
			 }

#~ 		class(resultadosParciales) <- c("multiBlepd","EvalBranch0")		 
		
		return(resultadosParciales)	
	 } 
	 
	 ## end loop for all approach
 
 
     if (class(branchToEval) == "character"){
		 
     if (length(branchToEval) == 1){
		 	 
		 terminalB <- which(getTerminals(tree=tree))
		 
		 internalB <- which(!getTerminals(tree=tree))
		 
		 if(tolower(branchToEval) == "all"){ branchToUse <- sort(c(internalB,terminalB)) }
		 
		 if(tolower(branchToEval) == "terminals"){ branchToUse <- terminalB }
		 
		 if(tolower(branchToEval) == "internals"){ branchToUse <- internalB }
		 		 
		 
		 }
			 if(all(branchToEval %in% tree$tip.label)){

			 esta <- function(x){which(tree$edge[,2] == which(x == tree$tip.label))}

			 branchToUse <- as.numeric(sapply(branchToEval, esta))
			 
			 }
		 
		 resultadosTotales <- list()
		 
		 for(conteo in 1:length(branchToUse)){
			 	 
		 
			 	 
			 resultadosTotales[[conteo]]  <-  evalBranch(tree = tree , 
		    	 		                            distribution = distribution , 
			 		                                branchToEval = branchToUse[conteo] , 
			 		                                approach = approach ,
			 		                                root = root ,
			 		                                index = index ,
			 		                                maxMultiplier = maxMultiplier,
			 		                                printNames = printNames )
			}
					 
#~ 		class(resultadosTotales) <- c("multiBlepd","EvalBranch1")		 
		
		
		return(resultadosTotales)		 

		 
		}  ## end loop for all branchToEval
		 

     
## initial stuff

        initialPD <- PDindex( tree = tree, 
                              distribution = distribution, 
                              root = root, 
                              index = index )
                                              
        bestInitialArea <- c(bestValue(distribution,initialPD))
        
        initialLength <- round(tree$edge.length[branchToEval],3)
       
       ##initialTreeLength <- tree$edge.length ## ?? so 
        
        totalTreeLength <- sum(tree$edge.length)
        
               
## initial test, branch lengths equal to zero
        
        newTree <- tree
    
        if (tolower(approach) %in% c("lower") ){
			
			##cat("using Lower ***\n")
			
            newTree$edge.length[branchToEval] <-  0
            
            maxPD <- 0 ##+ 0.001
            
			}
			
                
        if (tolower(approach) %in% c("upper") ){
			
			maxVal <- maxMultiplier * round(totalTreeLength,redondeo)
			
			newTree$edge.length[branchToEval] <-  maxVal
                        
            maxPD <- max(initialPD) - min(initialPD) 
            	
			}
                   
                
        modifiedPD <- PDindex( tree = newTree, 
                               distribution = distribution, 
                               root = root, 
                               index = index )
          
        bestModifiedArea <-  c(bestValue(distribution,modifiedPD))
        
              
        if(all(bestInitialArea %in% bestModifiedArea) &
           all(bestModifiedArea %in% bestInitialArea)){
			   
            
			   promedio <- initialLength
			                 
                         
                        ans <- list () 
            
                         ans$branchToEval     =   getTerminalLabels(tree,
                                                                    branchToEval,
                                                                    printNames)
                         
                         ans$bestInitialArea  =   bestInitialArea
                         
                         ans$bestModifiedArea =   bestModifiedArea
                         
                         ans$approach         =   approach 
                         
                         ans$delta            =   round((( promedio - initialLength ) / 
                                                       initialLength ) * 100 , 0 )  ## rounding to 0
                         
                         
                         if (!compact){
						 ans$maxPD            =   maxPD 
                         ans$initialPD        =   initialPD
                         ans$modifiedPD       =   modifiedPD
                         
                         ans$initialLength    =   initialLength
                         ans$finalLength      =   promedio
                         
							 
                         ans$areas            =   rownames(distribution)
                         ans$terminals        =   colnames(distribution)
                         ans$root             =   root
                         ans$index            =   index
					 }
                                     
            
            if (verbose){
				cat("\nTerminal: ",branchToEval,
				     ", has NO effect on branch length (0 or Max)",sep="")
				 }
            
#~             class(ans) <- c("multiBlepd","EvalBranch") 
                                      
            if(!is.null(ans))return(as.data.frame(ans))
            
            
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
                  
            newTree$edge.length[branchToEval] <-  promedio
            
            reCalculatedPD  <- PDindex( tree = newTree, 
                                        distribution = distribution, 
                                        root = root, 
                                        index = index )
            
            bestModifiedArea <-  c(bestValue(distribution,reCalculatedPD))
        

     
    if(round(promedio,redondeo) != round(ValorPrevio,redondeo)) { 
        
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
            
            newTree$edge.length[branchToEval] <-  promedio
            
            reCalculatedPD  <- PDindex( tree = newTree, 
                                        distribution = distribution, 
                                        root = root, 
                                        index = index )
            
            bestModifiedArea <-  c(bestValue(distribution,reCalculatedPD))
            
            
        ans <- list () ## rev
            
                         ans$branchToEval     =   getTerminalLabels(tree,
                                                                    branchToEval,
                                                                    printNames)
                         
                         ans$bestInitialArea  =   bestInitialArea
                         
                         ans$bestModifiedArea =   bestModifiedArea
                         
                         ans$approach         =   approach 
                         
                         ans$delta            =   round((( promedio - initialLength ) / 
                                                       initialLength ) * 100 , 0 )  ## rounding to 0
                         
                         
                         if (!compact){
						 ans$maxPD            =   maxPD 
                         ans$initialPD        =   initialPD
                         ans$modifiedPD       =   modifiedPD
                         
                         ans$initialLength    =   initialLength
                         ans$finalLength      =   promedio
                         
							 
                         ans$areas            =   rownames(distribution)
                         ans$terminals        =   colnames(distribution)
                         ans$root             =   root
                         ans$index            =   index
					 }
                         
            
                         
            
#~             class(ans) <- c("multiBlepd","EvalBranch")                       
            
            if(!is.null(ans)){
				return(as.data.frame(ans))
				}
            
            ## break("got it")
        }
        
        if ((tolower(approach) == "upper")  &
        !all(bestInitialArea %in% bestModifiedArea)){
            
#~             promedio <- promedio + (promedio/100000)
            
             ans <- list ()
            
                         ans$branchToEval     =   getTerminalLabels(tree,
                                                                    branchToEval,
                                                                    printNames)
                         
                         ans$bestInitialArea  =   bestInitialArea
                         
                         ans$bestModifiedArea =   bestModifiedArea
                         
                         ans$approach         =   approach 
                         
                         ans$delta            =   round((( promedio - initialLength ) / 
                                                       initialLength ) * 100 , 0 )  ## rounding to 0
                         
                         
                         if (!compact){
						 ans$maxPD            =   maxPD 
                         ans$initialPD        =   initialPD
                         ans$modifiedPD       =   modifiedPD
                         
                         ans$initialLength    =   initialLength
                         ans$finalLength      =   promedio
                         
							 
                         ans$areas            =   rownames(distribution)
                         ans$terminals        =   colnames(distribution)
                         ans$root             =   root
                         ans$index            =   index
					 }
      
            
#~             class(ans) <- c("multiBlepd","EvalBranch")                       
            
            if(!is.null(ans)){
				return(as.data.frame(ans))
				}
            
            ## break("got it")
            
        }else{
			initial <- promedio
			}
                      
    }
    
    } 
    ## end  repeat
            
    }
        
## end best
    
