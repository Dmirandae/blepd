
for(repetir in 1:3){

            val <- swapBL(RhinoclemmysData$tree,
                          RhinoclemmysData$distribution,
                          model = "allswap",
                         branch = "terminals",
                         nTimes = 10**repetir
                         )
           
                         
salida <- paste("table2_nTimes",val$nTimes,".csv",sep="")

write.blepd(val,file=salida)

}
