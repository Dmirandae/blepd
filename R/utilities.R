
## In-house functions



`%nin%` = Negate(`%in%`)


.bestVal <- function(distribution = distribution, initialVal){ 

   best <- row.names(distribution)[which(initialVal == max(initialVal))]
        
   resp <- tmpBest <- gsub("area","",best)
   
   if(length(tmpBest) > 1){
   resp <- paste(tmpBest,collapse="")
   }
   
   return(resp)
}



bestValue <- function(distribution = distribution, initialVal){ 

   best <- row.names(distribution)[which(initialVal == max(initialVal))]
        
   resp <- tmpBest <- gsub("area","",best)
   
   if(length(tmpBest) > 1){
   resp <- paste(tmpBest,collapse="")
   }
   
   return(as.data.frame(resp))
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
                        if ( (class(tree)!= "phylo")           || 
                             (class(distribution) != "matrix") ||
                             (!all(colnames(distribution) == tree$tip.label))
                            ){
												
							return(2)
							
							stop("Check class. tree and/or distribution. Mind the closing door")
							
							}else{
								return(1)
								}									
				}


###
##
# PD / PE
##
###

#'@title Calculate Phylogenetic Diversity (PD) or Phylogenetic Endemism (PE) for a given distribution.
#'
#' @description 
#' This function calculates the Phylogenetic Diversity (PD) or Phylogenetic Endemism (PE) for a given distribution of species across a phylogeny.
#'
#' @param tree A phylogenetic tree object in the 'phylo' format.
#' @param distribution A matrix or data frame representing the presence/absence of species in different areas. Rows represent areas and columns represent species.
#' @param root Logical. If TRUE, the root of the tree is included in the calculations. Defaults to FALSE.
#' @param index Character string specifying the index to calculate. Valid options are "PD" for Phylogenetic Diversity and "PE" for Phylogenetic Endemism. Defaults to "PD".
#' @param percentual Logical. If TRUE, the results are returned as percentages of the total PD or PE. Defaults to FALSE.
#'
#' @return A numeric vector containing the calculated PD or PE values for each area in the distribution matrix.
#'
#' @examples
#' 
#' # Calculate PD for each set
#' PD_values <- PDindex(tree = tree, distribution = distribution) 
#' 
#' @export

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
		
		}
			
         return(indexVal)
	 
	 

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
    



##
## from https://stackoverflow.com/questions/32470937/exchange-two-elements-of-a-vector-in-one-call
##


.swtch <- function(x,i,j) {x[c(i,j)] <- x[c(j,i)]; x} 


###
##
###
##
###

getCommand <- function(x){tolower(substr(x, start = 1, stop = 2))}

###
##
###
##
###

#' @title Find terminal branches with lengths exceeding a specified threshold.
#'
#' @description 
#' This function identifies the terminal branches in a phylogenetic tree that have lengths greater than a given threshold.
#'
#' @param arbol A phylogenetic tree object in the 'phylo' format.
#' @param valor A numeric value representing the length threshold. Branches with lengths greater than this value will be identified. Defaults to 0.0.
#'
#' @return A character vector containing the labels of the terminal branches that meet the length threshold.
#'
#' @examples
#' 
#' # Find terminal branches with lengths greater than 0.5
#' long_branches <- findTerminalgivenLength(arbol = tree, valor = 0.5) 
#' 
#' @export

findTerminalgivenLength <- function(arbol,valor=0.0){

target <- arbol$edge.length > valor

long <- length(arbol$tip.label)

listado <- arbol$edge[target,2]

listado <- listado[listado <= long]

return(arbol$tip.label[listado])

}


#' @title Convert a species distribution matrix into a data frame of terminal taxa and their associated distribution.
#' 
#' @description 
#' This function takes a matrix or data frame representing the presence/absence of species in different sets and converts it into a data frame where each row represents a terminal taxon and its corresponding area.
#' 
#' @param distribution A matrix or data frame representing the presence/absence of species in different sets. Rows represent sets and columns represent species.
#' 
#' @return A data frame with two columns: "Terminal" (the species name) and "Area" (the area where the species is present).
#'
#' @examples
#' 
#' # Convert distribution matrix to data frame
#' xy_df <- matrix2XY(distribution = distribution)
#' 
#' @export


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



findTerminalBranch <- function(arbol,terminal){

	numberTerminal  <-  which(arbol$tip.label == terminal)

	numberBranch    <-  which(arbol$edge[,2] == numberTerminal)


  return(numberBranch)
}



changeBLTerminal <- function(arbol,terminal,value){

	numBranch <- findTerminalBranch(arbol,terminal)

	arbol$edge.length[numBranch] <- value


  return(arbol)
}


## utils check name 


getTerminalLabels <- function(tree, numberOrden, printNames=TRUE){

	if(is.na(numberOrden)){return("XXXXX")}

	if(is.null(numberOrden)){return("XXXXX")}
	
		
	numberNode <- tree$edge[numberOrden,2]
	
	if (printNames){
	lista <- phytools::getDescendants(tree,numberNode) 

	#tree$tip.label[lista[lista <= length(tree$tip.label)]] ## ??
	
#~ 	if (numberNode > length(tree$tip.label)){
#~ 		 pegar <- "["
#~ 		 pegar <- "[InternalNode:"
#~ 	 }else{
#~ 		 pegar <- "["
#~ 		 }

        return(paste0("[",paste0(tree$tip.label[lista[lista <= length(tree$tip.label)]],collapse="/"),"]",
        collapse=" "))
#~                       numberNode, collapse=" "))
      }else{
		 
 		 return(numberNode)
		 
 		 }

## Revisar para nombres largos

}

