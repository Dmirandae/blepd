

matrix2XY <- function(distribution = distribution){

numberAreas <-  length(rownames(distribution))

numberAreas

tmpX <- function(numberArea){

    areaName <- rownames(distribution)[numberArea]

    tmpArea <- paste(areaName,colnames( distribution[,
        which(distribution[numberArea,] == 1)]))
        
    tmpArea <- unlist(strsplit(tmpArea," "))
    
    return(tmpArea)    
        
    }

cuatro <- unlist(lapply((1:numberAreas),tmpX))

newDist <- matrix(cuatro, ncol=2, byrow=TRUE)

colnames(newDist) <- c("Area","Terminal")

newDist <-  as.data.frame(newDist)

return(newDist)

}
