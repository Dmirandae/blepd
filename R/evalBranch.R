#'
#' @title evalBranch
#'
#' @description The function calculates whether a change in a branch length generates a change in the area selected; and when applies, the branch length value for that change.  
#' 
#' @return Returns a S3 object [class blepd] with all the relevant information: whether there is no-change/change in area as we change the terminal branch length, the maxPD difference for the upper/lower limit, the branch length of the change, the best Initial Area, the actual (initial) branch length, and the area selected.
#'
#' @param tree is a single tree with T terminals, an APER phylo object.
#' 
#' @param distribution is a labeled matrix object, with the distribution of T terminals (columns) in A areas (rows).
#' 
#' @param branchToEval is the number/name of the branch to evaluate, "terminals" or "internals" evaluate only those named, while "all", evaluates all terminals/internals.
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
#'            branchToEval = "internals" ,  approach = "lower" , 
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
                         redondeo      = 3,
                         verbose       = FALSE,
                         compact       = TRUE,
                         printNames    = FALSE){

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
			 		                                branchToEval = branchToUse[conteo]  , 
			 		                                approach = approach ,
			 		                                root = root ,
			 		                                index = index ,
			 		                                maxMultiplier = maxMultiplier )
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
			
			maxVal <- maxMultiplier * round(totalTreeLength,redondeo)
			
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
			                 
                         
                        ans <- list () ## rev
            
                         ans$branchToEval     =   getTerminalLabels(tree,
                                                                    branchToEval,
                                                                    printNames)
                         
                         ans$bestInitialArea  =   bestInitialArea
                         
                         ans$bestModifiedArea =   bestModifiedArea
                         
                         ans$approach         =   approach 
                         
                         ans$delta            =   round((( promedio - initialLength ) / 
                                                       initialLength ) * 100 , redondeo ) 
                         
                         
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
                                                       initialLength ) * 100 , redondeo ) 
                         
                         
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
                                                       initialLength ) * 100 , redondeo ) 
                         
                         
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
    

### for utilities

## in utils check name 

bestValue <- function(distribution = distribution, initialVal){ 

   best <- row.names(distribution)[which(initialVal == max(initialVal))]
        
   resp <- tmpBest <- gsub("area","",best)
   
   if(length(tmpBest) > 1){
   resp <- paste(tmpBest,collapse="")
   }
   
   return(as.data.frame(resp))
}


###NOT in utils




getTerminalLabels <- function(tree, numberOrden, printNames=TRUE){

library(phytools)
		
if(is.na(numberOrden)){return("XXXXX")}

if(is.null(numberOrden)){return("XXXXX")}
		
	numberNode <- tree$edge[numberOrden,2]
	
	if (printNames){
	lista <- phytools::getDescendants(tree,numberNode) 

	tree$tip.label[lista[lista <= length(tree$tip.label)]] ## ??
	
	if (numberNode > length(tree$tip.label)){
		 pegar <- paste0("[node number:",numberNode,":",collapse=" ")
	 }else{
		 pegar <- "["
		 }

        return(paste0(pegar,paste0(tree$tip.label[lista[lista <= length(tree$tip.label)]],collapse="/"),"]|",numberNode, collapse=" "))
      }else{
		 
 		 return(numberNode)
		 
 		 }
## Revisar para nombres largos


}
