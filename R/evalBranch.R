#'
#' @title evalBranch
#'
#' @description The function calculates whether a change in the terminal branch length generates a change in the area selected; and when applies, the terminal branch length value for that change.  
#' 
#' @return Returns a S3 object [class blepd] with all the relevant information: whether there is no-change/change in area as we change the terminal branch length, the maxPD difference for the upper/lower limit, the branch length of the change, the best Initial Area, the actual (initial) branch length, and the area selected.
#'
#' @param tree is a single tree with T terminals, an APER phylo object.
#' 
#' @param distribution is a labeled matrix object, with the distribution of T terminals (columns) in A areas (rows).
#' 
#' @param branchToEval is the number/name of the branch to evaluate, "terminals" or "internals" evaluate only those named, while "all", evaluates all terminals/internals and will generate a multiBlepd object (S3).
#' 
#' @param approach is the type of limit to evaluate, "upper": from the actual length to maxVal [*maxMultiplier], or "lower": from the actual length to 0.0, or "all" (default = "upper"). 
#' 
#' @param maxMultiplier is the value to multiply the sum of the branch length values. The upper limit to evaluate will be BL_sum * maxMultiplier (default = 1.01). 
#' 
#' @param root is use.root in PD function (default = FALSE). 
#'
#' @param verbose is the length of the speech (default = FALSE). 
#' 
#' 
#' @examples
#' library(blepd)
#' data(tree)
#' data(distribution)
#' evalBranch(tree = tree , distribution = distribution , 
#'            branchToEval = "all" ,  approach = "lower" , 
#'            root = TRUE)
#'
#'
#'@author Miranda-Esquivel Daniel R.
#'
#'

evalBranch   <- function(tree          = tree , 
                         distribution  = distribution , 
                         branchToEval  = branchToEval , 
                         approach      = "lower" , 
                         root          = FALSE ,
                         index         = "PD",
                         maxMultiplier = 1.01,
                         redondeo      = 2,
                         verbose       = FALSE  ){



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
			 		                                maxMultiplier = maxMultiplier )			 
			 }

		class(resultadosParciales) <- c("multiBlepd","EvalBranch0")		 
		
		return(resultadosParciales)	
	 } 
	 
	 ## end loop for all approach
 
 
     if (class(branchToEval) == "character"){
		 
     if (length(branchToEval) == 1){

		 if(branchToEval == "all"){branchToUse <- 1:length(tree$edge.length)}
		 
		 if(branchToEval == "terminals"){branchToUse <- which(getTerminals(tree=tree)) }
		 
		 if(branchToEval == "internals"){branchToUse <- which(!getTerminals(tree=tree)) }
		 
#~ 		 if(all(branchToEval  %in% c("internals","all","terminals"))){
		 
		 }
			 if(all(branchToEval %in% tree$tip.label)){

			 esta <- function(x){which(tree$edge[,2] == which(x == tree$tip.label))}

			 branchToUse <- as.numeric(sapply(branchToEval, esta))
			 
			 }
#~ 		 }
		 
		 resultadosTotales <- list()
		 
		 for(conteo in 1:length(branchToUse)){
			 	 
			 	 
			 resultadosTotales[[conteo]]  <-  evalBranch(tree = tree , 
		    	 		                            distribution = distribution , 
			 		                                branchToEval = branchToUse[conteo]  , 
			 		                                approach = approach ,
			 		                                root = root ,
			 		                                index = index ,
			 		                                maxMultiplier = maxMultiplier )
			}
					 
		class(resultadosTotales) <- c("multiBlepd","EvalBranch1")		 
		
		
		return(resultadosTotales)		 

		 
		}  ## end loop for all branchToEval
		 

        

        
## initial stuff

        initialPD <- PDindex( tree = tree, 
                              distribution = distribution, 
                              root = root, 
                              index = index )
                              
        initialLength <- round(tree$edge.length[branchToEval],4)
       
        
#~         initialPD[is.na(initialPD)] <-   0.0
        
                
        bestInitialArea <- c(bestValue(distribution,initialPD))
        
        initialLength <- round(tree$edge.length[branchToEval],4)
       
        initialTreeLength <- tree$edge.length
        
        totalTreeLength <- sum(tree$edge.length)
        
               
## initial test, branch lengths equal to zero
        
        newTree <- tree
    
        if (tolower(approach) %in% c("lower") ){
			
			##cat("using Lower ***\n")
			
            newTree$edge.length[branchToEval] <-  0
            
            maxPD <- 0 ##+ 0.001
            
			}
			
                
        if (tolower(approach) %in% c("upper") ){
			
			maxVal <- maxMultiplier * round(totalTreeLength,6)
			
			newTree$edge.length[branchToEval] <-  maxVal
                        
            maxPD <- max(initialPD) - min(initialPD) ## por que la diferencia ?
            	
			}
                   
                
        modifiedPD <- PDindex( tree = newTree, 
                               distribution = distribution, 
                               root = root, 
                               index = index )
          
        bestModifiedArea <-  c(bestValue(distribution,modifiedPD))
        
              
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
                         branchToEval     =   getTerminalLabels(tree,branchToEval), 
#~                          branchToEval     =   branchToEval, 
                         approach         =   approach, 
                         index            =   index,
                         finalLength      =   promedio,
                         delta            =   round((( promedio - initialLength ) /
                                                       initialLength ) * 100 , redondeo )
                         )
            
            if (verbose){
				cat("\nTerminal: ",branchToEval,
				     ", has NO effect on branch length (0 or Max)",sep="")
				 }
            
            class(ans) <- c("multiBlepd","EvalBranch") 
                                      
            if(!is.null(ans))return(ans)
            
            
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
            
            newTree$edge.length[branchToEval] <-  promedio
            
            reCalculatedPD  <- PDindex( tree = newTree, 
                                        distribution = distribution, 
                                        root = root, 
                                        index = index )
            
            bestModifiedArea <-  c(bestValue(distribution,reCalculatedPD))
            
            
            ans <- list (maxPD            =   maxPD , 
                         areas            =   rownames(distribution),
                         terminals        =   colnames(distribution),
                         bestInitialArea  =   bestInitialArea, 
                         bestModifiedArea =   bestModifiedArea,
                         modifiedPD       =   modifiedPD,
                         initialPD        =   initialPD,
                         initialLength    =   initialLength,
                         root             =   root,
                         branchToEval     =   getTerminalLabels(tree,branchToEval), 
#~                          branchToEval     =   branchToEval, 
                         approach         =   approach, 
                         index            =   index,
                         finalLength      =   promedio,
                         delta            =   round((( promedio - initialLength ) / 
                                                       initialLength ) * 100 , redondeo )      
                         )
            
            class(ans) <- c("multiBlepd","EvalBranch")                       
            
            if(!is.null(ans))return(ans)
            
            
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
                         branchToEval     =   getTerminalLabels(tree,branchToEval), 
#~                          branchToEval     =   branchToEval, 
                         approach         =   approach,
                         index            =   index, 
                         finalLength      =   promedio,
                         delta            =   round((( promedio - initialLength ) / 
                                                       initialLength ) * 100 , redondeo )                  
                         )
      
            
            class(ans) <- c("multiBlepd","EvalBranch")                       
            
            if(!is.null(ans))return(ans)
            
            ## break("got it")
            
        }else{
			initial <- promedio
			}
                      
    }
    
    } 
    ## end  repeat
            
    }
        
## end best
    

### for utilities

## in utils chexh name 

bestValue <- function(distribution = distribution, initialVal){ 

   best <- row.names(distribution)[which(initialVal == max(initialVal))]
        
   resp <- tmpBest <- gsub("area","",best)
   
   if(length(tmpBest) > 1){
   resp <- paste(tmpBest,collapse="")
   }
   
   return(resp)
}


###NOT in utils




getTerminalLabels <- function( tree, numberOrden ){

library(phytools)
		
if(is.na(numberOrden)){return("XXXXX")}

if(is.null(numberOrden)){return("XXXXX")}
		
	numberNode <- tree$edge[numberOrden,2]
	
	lista <- phytools::getDescendants(tree,numberNode) 

	tree$tip.label[lista[lista <= length(tree$tip.label)]] ## ??
	
	if (numberNode > length(tree$tip.label)){
		 pegar <- paste0("[node number:",numberNode,":",collapse=" ")
	 } else{
		 pegar <- "["
		 }

  return(paste0(pegar,paste0(tree$tip.label[lista[lista <= length(tree$tip.label)]],collapse="/"),"]",collapse=" "))

}
