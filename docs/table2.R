
for(repetir in 1:3){

            val <- swapBL(RhinoclemmysData$tree,
                          RhinoclemmysData$distribution,
                          model = "allswap",
                         branch = "terminals",
                         nTimes = 10**repetir
                         )

print.blepd(val)

}
