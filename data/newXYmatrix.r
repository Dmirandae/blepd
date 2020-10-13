csvFile <- dir(pattern=".csv")
distribution <-          as.matrix(read.table(csvFile,
                                  stringsAsFactors=FALSE,
                                  header=TRUE,
                                  row.names=1,
                                  sep=",")
                          )

distribution

numberAreas <-  length(row.names(distribution))


for(area in 1:numberAreas){

a1 <- names(which(distribution[area,] == 1))

dfTemporal <- data.frame(Terminals=a1,Areas=(replicate(length(a1),row.names(distribution)[area])))

if(area ==1){
	dfFinal <- dfTemporal}else{
	dfFinal <- rbind(dfFinal,dfTemporal)}
}	


dfFinal	
