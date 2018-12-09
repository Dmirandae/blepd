
## in house functions


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

## utils check tree

.checkInput <- function(distribution = distribution, tree = tree){
                        if ((class(tree)!="phylo") || (class(distribution)!="matrix")){
							stop("Check  tree and/or distribution. Mind the closing door")
							}
				}





PDindex <- function (tree = tree, distribution = distribution, root = FALSE, index= "PD", percentual = FALSE){
        
    if(index == "PD" ){

    PDcalc <- pd(samp =distribution,tree = tree,include.root = root)$PD
    
         indexVal <- PDcalc
        
        }

   if(index == "PE" ){
       
           PDtotal <- sum(tree$edge.length)
           
           PDcomplement <- pd(samp =!distribution,tree = tree,include.root = root)$PD
           
           endemism <-  c(PDtotal - PDcomplement)
           
           indexVal <- endemism
       
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

    


lengthTerminals <- function (tree = tree){

       terminals <- getTerminals ( tree )
       
       BLterminals <- NULL
       
       BLterminals <- tree$edge.length[c(terminals)]
       
       names(BLterminals) <- tree$tip.label

  return(BLterminals)

}    
    



graficar <- function(x){

  qplot(x$AreaSelected,x$Freq, ylim=c(0,sum(x$Freq)), xlab="", ylab="Frec")

}


##
## from https://stackoverflow.com/questions/32470937/exchange-two-elements-of-a-vector-in-one-call
##
.swtch <- function(x,i,j) {x[c(i,j)] <- x[c(j,i)]; x} 
