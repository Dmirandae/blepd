#'
#' @title matrix2XY
#'
#' @description Converts a distribution object in a XY data.frame.
#' 
#' @param distribution is a labeled matrix object, with the distribution of T terminals (rows) in A areas (columns).
#' 

#' 
#' @examples
#' library(blepd)
#' data(distribution)
#' matrix2XY(distribution)
#'
#'
#'@author Miranda-Esquivel Daniel R.
#'
#'

#~ matrix2XY1 <- function(distribution = distribution){

#~ numberAreas <-  length(rownames(distribution))

#~ ## numberAreas

#~ tmpX <- function(numberArea){

#~     areaName <- rownames(distribution)[numberArea]

#~     tmpArea <- paste(areaName,colnames( distribution[,
#~         which(distribution[numberArea,] == 1)]))
        
#~     tmpArea <- unlist(strsplit(tmpArea," "))
    
#~     return(tmpArea)    
        
#~     }

#~ cuatro <- unlist(lapply((1:numberAreas),tmpX))

#~ newDist <- matrix(cuatro, ncol=2, byrow=TRUE)

#~ colnames(newDist) <- c("Area","Terminal")

#~ newDist <-  as.data.frame(newDist)

#~ return(newDist)

#~ }


matrix2XY <- function(distribution = distribution){

species  <- colnames(distribution)

numSpecies <- length(species)

for(contador in 1:numSpecies){

    t1 <- paste(names(which(distribution[,contador] == 1)),species[contador],sep = " ")

    t1 <- strsplit(t1," ")

    t1<- unlist(t1)
    
    a1 <- matrix(t1, ncol=2, byrow=TRUE)
    
    if(contador == 1){salida <- a1} else{
		
		salida <- rbind(salida,a1)
		}

}

colnames(salida) <- c("Area","Terminal")

salida <-  as.data.frame(salida)

return(salida)

}

