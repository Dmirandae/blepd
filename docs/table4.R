
#~  ## calculate values

terminals  <- evalTerminalvsInternal(RhinoclemmysData$tree,
                                RhinoclemmysData$distribution,
                                nTimes=100,
                                branch = "terminals")

internals  <- evalTerminalvsInternal(RhinoclemmysData$tree,
                                RhinoclemmysData$distribution,
                                nTimes=100,
                                branch = "internals")


for( i in 1:3 ){

#    cat("name ",i,"\n")


    cat("Evaluated:",terminals[[i]]$evaluated,"\tBranch swapped:",terminals[[i]]$branch,"\n")

    print.blepd(terminals[[i]])

    cat("Evaluated:",internals[[i]]$evaluated,"\tBranch swapped:",internals[[i]]$branch,"\n")

    print.blepd(internals[[i]])

}




