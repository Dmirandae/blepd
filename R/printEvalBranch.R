
print.evalBranchAll <- function(object0){


if(!any(class(object0) %in% c("EvalBranch","EvalBranch0","EvalBranch1"))){return(print("Wrong class, object must be EvalBranch"))}



if(any(class(object0) == "EvalBranch1")){
	
	if(any(class(object0[[1]]) == "EvalBranch")){
 

prepareOutput <- function(x,object){
cat(unlist(object[[x]]$branchToEval),
unlist(object[[x]]$bestInitialArea),
unlist(object[[x]]$bestModifiedArea),
unlist(object[[x]]$delta),"\n",sep="\t")
}

	
	Entries <- 1:length(object0)


cat("\nApproach:",unlist(object0[[1]]$approach),"\nTerminalsInBranch","initArea","Modified","Delta%\n",sep="\t")
cat(unlist(sapply(Entries,prepareOutput,object=object0)))
cat("\n\n")


}
return()
}

prepareOutput <- function(x,object){
cat(unlist(object$upper[[x]]$branchToEval),
unlist(object$upper[[x]]$bestInitialArea),
unlist(object$upper[[x]]$bestModifiedArea),
unlist(object$upper[[x]]$delta),
unlist(object$lower[[x]]$bestModifiedArea),
unlist(object$lower[[x]]$delta),"\n",sep="\t")
}
	
	Entries <- 1:length(object0$lower)

cat("TerminalsInBranch","initArea","Mod-upper","Delta%","Mod-lower","Delta%\n",sep="\t")
cat(unlist(sapply(Entries,prepareOutput,object=object0)))
cat("\n\n")

return()
}




