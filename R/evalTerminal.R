#'
#' @title evalTerminal
#'
#' @description The function calculates whether a change in the terminal branch length generates a change in the area selected; and when applies, the terminal branch length value for that change.  
#' 
#' @return Returns a S3 object [class blepd] with all the relevant information: whether there is no-change/change in area as we change the terminal branch length, the maxPD difference for the upper/lower limit, the branch length of the change, the best Initial Area, the actual (initial) branch length, and the area selected.
#'
#' @param tree is a single tree with T terminals, an APER phylo object.
#' 
#' @param distribution is a labeled matrix object, with the distribution of T terminals (columns) in A areas (rows).
#' 
#' @param tipToEval is the label of the terminal to evaluate. If you use "all", it will evaluate all terminals and will generate a multiBlepd object (S3).
#' 
#' @param approach is the type of limit to evaluate, "upper": from the actual length to maxVal [*maxMultiplier], or "lower": from the actual length to 0. 
#' 
#' @param maxMultiplier is the value to multiply the sum of the branch length values. The upper limit to evaluate will be BL_sum * maxMultiplier. 
#' 
#' @param root is use.root in PD function. 
#' 
#' 
#' @examples
#' library(blepd)
#' data(tree)
#' data(distribution)
#' evalTerminal(tree = tree , distribution = distribution , 
#'              tipToEval = "t1" ,  approach = "lower" , 
#'              root = TRUE)
#'
#'
#'@author Miranda-Esquivel Daniel R.
#'
#'

evalTerminal <- function(tree = tree , 
                         distribution = distribution , 
                         tipToEval = "taxB" , 
                         approach = "lower" , 
                         root = FALSE ,
                         index = "PD",
                         maxMultiplier = 1.01 ){

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

            
            ans <- list (maxPD            =   maxPD , 
                         areas            =   rownames(distribution),
                         terminals        =   colnames(distribution),
                         bestInitialArea  =   bestInitialArea, 
                         bestModifiedArea =   bestModifiedArea,
                         modifiedPD       =   modifiedPD,
                         initialPD        =   initialPD,
                         initialLength    =   initialLength,
                         root             =   root,
                         tipToEval        =   tipToEval  , 
                         approach         =   approach  , 
                         index            =   index,
                         finalLength      =   promedio ,
                         delta            =   round((promedio-initialLength)/initialLength*100,4)
                         )
            
            cat("\nTerminal: ",tipToEval,", has NO effect on branch length == 0 or Max",sep="")
            
            class(ans) <- c("multiBlepd","EvalTerminal") 
                                      
            return(ans)
            
            break("got it")
        }
                        
        ## end of est branch of zero length or max       
    
        
        ## let's continue
        
        ## create a divide and conquer loop 
        
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
        
 
##if(debugDRME & fineDebug){cat((promedio != ValorPrevio),"|",ValorPrevio,"==",promedio,"|  init",bestInitialArea,"modif",bestModifiedArea,"||",initial,"---",final,"| val PD:",paste(reCalculatedPD,sep="__"),"\n")}
     
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
                         tipToEval        =   tipToEval  , 
                         approach         =   approach  , 
                         index            =   index ,
                         finalLength      =   promedio ,
                         delta            =   round((promedio-initialLength) / (initialLength*100) ,4)      
                         )
            
            class(ans) <- c("multiBlepd","EvalTerminal")                       
            
            return(ans)
            
            
            break("got it")
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
                         tipToEval        =   tipToEval  , 
                         approach         =   approach  , 
                         finalLength      =   promedio ,
                         delta            =   round((promedio-initialLength) / (initialLength*100),4)                  
                         )
      
            
            class(ans) <- c("multiBlepd","EvalTerminal")                       
            
            return(ans)
            
            break("got it")
            
        }else{
			initial <- promedio
			}
                      
    }
    
    } 
    ## end  repeat
            
    }
        
## end best
    
