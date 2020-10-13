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

matrix2XY <- function(distribution = distribution){

numberAreas <-  length(row.names(distribution))


  for(area in 1:numberAreas){
	  a1 <- names(which(distribution[area,] == 1))
	  
	  dfTemporal <- data.frame(Terminal=a1,Area=(replicate(length(a1),row.names(distribution)[area])))
	  
  if(area ==1){
	dfFinal <- dfTemporal}else{
	dfFinal <- rbind(dfFinal,dfTemporal)}
}	


return(dfFinal)

}

