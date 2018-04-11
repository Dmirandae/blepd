
## in house functions

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

## utils check tree

.checkInput <- function(distribution = distribution, tree = tree){
                        if ((class(tree)!="phylo") || (class(distribution)!="matrix")){
							stop("Check  tree and/or distribution. Mind the closing door")
							}
				}

