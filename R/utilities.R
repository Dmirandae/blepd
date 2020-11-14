
## In-house functions

#### printing


print.blepd <- function(obj,tabular=FALSE) {
	           if(!tabular) cat("\nBestInitial:")
               cat(obj$bestInitialArea)
               if(!tabular){ cat("\n")}else{cat("\t")}
                ##if(!tabular) cat("Selected:\t")
                if(tabular){
					cat(obj$bestModifiedArea,"\n")
					}else{
					    obj$bestModifiedArea$Percent <- obj$bestModifiedArea$Freq / obj$nTimes * 100
						print(obj$bestModifiedArea)
						}
}



write.blepd <- function(obj,tabular=FALSE, file="output.csv") {
	           if(!tabular) cat("\nBestInitial:",file=file)
               cat(obj$bestInitialArea,
                   file = file,
                   append =TRUE)
               if(!tabular){ 
				   cat("\n",
				   file = file,
                   append =TRUE)
                   }else{
					   cat("\t",
					   file = file,
					   append =TRUE)
					   }
                ##if(!tabular) cat("Selected:\t")
                if(tabular){
					cat(unlist(obj$bestModifiedArea),"\n",
					    file = file,
                        append =TRUE)
					}else{
					    obj$bestModifiedArea$Percent <- obj$bestModifiedArea$Freq / obj$nTimes * 100
							write.table(obj$bestModifiedArea,
							file = file,
                            append =TRUE)
						}
}




print.multiBlepd <- function(obj,tabular=TRUE) {
               if(tabular) cat("\nDelta (%)\tTerminal\tInitial\tSelected\tEvaluating",obj[[1]]$approach,"\n")	
	           
	           for( contador in 1:length(obj) ){
				   
				   cat(obj[[contador]]$delta,"\t",obj[[contador]]$tipToEval ,"\t")
					   
                   print.blepd(obj[[contador]],tabular=tabular)
                   
                }
}




`%nin%` = Negate(`%in%`)


.bestVal <- function(distribution = distribution, initialVal){ 

   best <- row.names(distribution)[which(initialVal == max(initialVal))]
        
   resp <- tmpBest <- gsub("area","",best)
   
   if(length(tmpBest) > 1){
   resp <- paste(tmpBest,collapse="")
   }
   
   return(resp)
}



.createTable <- function(tree = tree){
    ## create table
    allDataTable <- tree$edge

    ## bind tree length
    allDataTable <- cbind (allDataTable, tree$edge.length)

 return(allDataTable)
 }


## utils check tree / distribution

.checkInput <- function(distribution = distribution, tree = tree){
                        if ( (class(tree)!="phylo") || 
                             (class(distribution)!="matrix") ||
                             (!all(colnames(distribution) == tree$tip.label))
                            ){
												
							return(2)
							
							stop("Check class. tree and/or distribution. Mind the closing door")
							
							}else{
								return(1)
								}									
				}





PDindex <- function (tree = tree, 
                     distribution = distribution, 
                     root = FALSE, 
                     index= "PD", 
                     percentual = FALSE){
						 
	tree <- reorder(tree, order = "cladewise")
					 
        
    if(index == "PD" ){

         indexVal <- pd(samp =distribution,tree = tree,include.root = root)$PD
            
        }

   if(index == "PE" ){
       
           PDtotal      <-  sum(tree$edge.length)
           
           PDcomplement <-  pd(samp =!distribution,tree = tree,include.root = root)$PD
           
           indexVal     <-  c(PDtotal - PDcomplement)
                  
   }


	if(percentual){
		
		indexVal <- round((indexVal/sum(indexVal)*100),2)
		
		}else{
			
         return(indexVal)
	 
	 }

  #c <- cophenetic(tree)
  #AvTD <- taxondive(comm = distribution ,
  #                  dis = c,
  #                  match.force = T)            
  #res <- AvTD$Dplus
  #res[is.nan(res)] <- 0
                    
}



getTerminals <- function (tree = tree){

      numberTerminals <- length(tree$tip.label)   

       terminals <- tree$edge[,2] < (numberTerminals + 1)

  return(terminals)

}    

    


lengthTerminals  <- function ( tree = tree ){

                  terminals          <-  getTerminals ( tree )
       
                  BLterminals        <-  NULL
       
                  BLterminals        <-  tree$edge.length[c(terminals)]
       
                  names(BLterminals) <- tree$tip.label

  return(BLterminals)

}    
    



#~ graficar <- function(x){

#~   qplot(x$AreaSelected,x$Freq, ylim=c(0,sum(x$Freq)), xlab="", ylab="Frecuency")

#~ }


##
## from https://stackoverflow.com/questions/32470937/exchange-two-elements-of-a-vector-in-one-call
##


.swtch <- function(x,i,j) {x[c(i,j)] <- x[c(j,i)]; x} 



#'
#' @title matrix2XY
#'
#' @description Converts a distribution object into a XY data.frame, suitable to plot.
#' 
#' @param distribution is a labeled matrix object, with the distribution of T terminals (columns) in A areas (rows).
#' 

#' 
#' @examples
#' library(blepd)
#' data(distribution)
#' matrix2XY(distribution)
#'
#'
#' @author Miranda-Esquivel Daniel R.
#'
#'

matrix2XY <- function(distribution = distribution){

	numberAreas <-  length(row.names(distribution))

	for( area in 1:numberAreas ){
	  a1 <- names(which(distribution[area,] == 1))
	  
	  dfTemporal <- data.frame(Terminal=a1,Area=(replicate(length(a1),row.names(distribution)[area])))
	  
	if( area ==1 ){
	dfFinal <- dfTemporal}else{
	dfFinal <- rbind(dfFinal,dfTemporal)}
	}	

return(dfFinal)

}
