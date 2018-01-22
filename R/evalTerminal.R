#'
#' @title evalTerminal
#'
#' @description The function calculates whether a change in the terminal branch length generates a change in the area selected; and when applies, the terminal branch length value for that change.  
#' 
#' @return Returns four fields, and depending on the results -whether there is no-change/change in area as we change the terminal branch length-, the function returns the maxPD difference for the upper limit, or 0.0 for the lower limit, the best Initial Area, a dummy value of "*" to indicate there is no change in area and the actual (initial) branch length; or, when there is a change in area selected, the function returns the branch length of the change, the best Initial Area, the area selected, and the actual (initial) branch length.
#'
#' @param tree is a single tree with T terminals, an APER phylo object.
#' 
#' @param distribution is a labeled matrix object, with the distribution of T terminals (rows) in A areas (columns).
#' 
#' @param tipToEval is the label of the terminal to evaluate.
#' 
#' @param approach is the type of limit to evaluate, "upper": from the actual length to maxVal, or "lower": from the actual length to 0. 
#' 
#' @param maxMultiplier is the value to multiply the sum of the PD values. This value will be the upper limit to evaluate. 
#' 
#' @param root is use.root in PD function. 
#' 
#' 
#' @examples
#' library(blepd)
#' data(tree)
#' data(distribution)
#' evalTerminal(tree = tree , distribution = distribution , 
#' tipToEval = "t1" ,  approach = "lower" , root = FALSE)
#'
#'
#'@author Miranda-Esquivel Daniel R.
#'
#'

evalTerminal <- function(tree = tree , distribution = distribution , 
                         tipToEval = "taxB" , approach = "lower" , 
                         root = FALSE ,
                         maxMultiplier = 1.5 ){


##if(debugDRME){cat("\n inicio en terminal:",tipToEval,":",approach,"\n")}

## potential errors

        numberTipToEval <- which(tree$tip.label %in% tipToEval) 
        
        if (is.na(numberTipToEval)){stop("Check names in tree / distribution. Mind the closing door")}

        if(!all(colnames(distribution) %in% tree$tip.label)){stop("Check names in tree / distribution. Mind the closing door")}


## in house functions


bestVal <- function(distribution = distribution, initialVal){ 

   best <- row.names(distribution)[which(initialVal == max(initialVal))]
        
   resp <- tmpBest <- gsub("area","",best)
   
   if(length(tmpBest) > 1){
   resp <- paste(tmpBest,collapse="")
   }
   
   return(resp)
}


createTable <- function(tree = tree){
    ## create table
    allDataTable <- tree$edge

    ## bind tree length
    allDataTable <- cbind (allDataTable, tree$edge.length)

return(allDataTable)

}


        
## initial stuff

        initialPD <- myPD(tree = tree, distribution = distribution, root = root)
        
        
        bestInitialArea <- c(bestVal(distribution,initialPD))
        
        initialLength <- round(tree$edge.length[which(createTable(tree)[,2] %in% numberTipToEval)],4)
        
               
## initial test of branch length equal to zero
        
               newTree <- tree
    
        if (tolower(approach) %in% c("lower") ){
			
            newTree$edge.length[which(createTable(tree)[,2] %in% numberTipToEval)] <-  0
            
            maxPD <- 0 ##+ 1
            
			}
                
        if (tolower(approach) %in% c("upper") ){
			
			maxVal <- maxMultiplier * round(sum(tree$edge.length),6)
			
			newTree$edge.length[which(createTable(tree)[,2] %in% numberTipToEval)] <-  maxVal
            
            
            maxPD <- max(initialPD) - min(initialPD)
            ##maxPD <- round(sum(tree$edge.length),6)
			}
        
                
        modifiedPD <- myPD(tree = newTree, distribution = distribution, root = root)
        
    
        bestModifiedArea <-  c(bestVal(distribution,modifiedPD))
        
        
        if(all(bestInitialArea %in% bestModifiedArea) &
           all(bestModifiedArea %in% bestInitialArea)){
            
            ans <-c (maxPD , bestInitialArea, 
                      "*" , initialLength)
            
            return(ans)
            
              break("got it")
        }
                        
        ## end of est brach of zero length or max       
    
        
        ## let's continue
        
        ## create a divide and conquer loop 
        
    if (tolower(approach) == "lower"){
    
        ValorPrevio <- 9999999999
                
        initial <- 0.0
        
        final <- initialLength}


    if (tolower(approach) == "upper"){
    
        ValorPrevio <- 9999999999
                
        initial  <- initialLength+0.00001
        
        final <- maxVal}


    repeat{
            promedio <- mean(c(final,initial))
            
            newTree$edge.length[which(createTable(tree)[,2] %in% numberTipToEval)] <-  promedio
            
            reCalculatedPD  <- myPD(tree = newTree, distribution = distribution, root=root)
            
            bestModifiedArea <-  c(bestVal(distribution,reCalculatedPD))
        
 
##if(debugDRME & fineDebug){cat((promedio != ValorPrevio),"|",ValorPrevio,"==",promedio,"|  init",bestInitialArea,"modif",bestModifiedArea,"||",initial,"---",final,"| val PD:",paste(reCalculatedPD,sep="__"),"\n")}
     
    if(round(promedio,6) != round(ValorPrevio,6)) { 
        
        ValorPrevio <- promedio
        
    if((all(bestInitialArea %in% bestModifiedArea)) &
    (all(bestModifiedArea  %in%  bestInitialArea))){
   
        if (tolower(approach) == "lower"){final <- promedio}
      
        if (tolower(approach) == "upper"){inicial <- promedio
                        
            }
        
        }else{
              
      if (tolower(approach) == "lower"){initial <- promedio}

      if (tolower(approach) == "upper"){final <- promedio}
            
            }
    
        }else{
         
       if (tolower(approach) == "lower"){
            
            promedio <- promedio - 0.0001
            
             newTree$edge.length[which(createTable(tree)[,2] %in% numberTipToEval)] <-  promedio
            
            reCalculatedPD  <- myPD(tree = newTree, distribution = distribution, root = root)
            
            bestModifiedArea <-  c(bestVal(distribution,reCalculatedPD))
                        
			return(c(round(promedio,4), bestInitialArea, bestModifiedArea, initialLength))
            
            break("got it")
        }
        
        if ((tolower(approach) == "upper")  &
        !all(bestInitialArea %in% bestModifiedArea)){
            
            promedio <- promedio + 0.0001
            
            return(c(round(promedio,4), bestInitialArea, bestModifiedArea, initialLength))
            
            break("got it")
            
        }else{initial <- promedio}
                      
    }
    
    } 
    ## end  repeat
            
    }
        
## end best
    
