
print.evalBranchAll <- function(object0, compact=TRUE){


if(!any(class(object0) %in% c("EvalBranch","EvalBranch0","EvalBranch1"))){return(print("Wrong class, object must be EvalBranch"))}



if(any(class(object0) == "EvalBranch1")){
	
	if(any(class(object0[[1]]) == "EvalBranch")){
 

prepareOutput <- function(x,object){
if(compact){ branch <- strsplit(object[[x]]$branchToEval,split=":")[[1]][2]
	}else{
	branch <- unlist(object[[x]]$branchToEval)}
	
	if(is.na(branch)){branch <- unlist(object[[x]]$branchToEval)}
	
cat(branch,
unlist(object[[x]]$bestInitialArea),
unlist(object[[x]]$bestModifiedArea),
unlist(object[[x]]$delta),"\n",sep="\t")
}

	
	Entries <- 1:length(object0)

if(compact){inT <- "\nBranchNumber"}else{inT <- "\nTerminalsInBranch"}
	
cat("\nApproach:",unlist(object0[[1]]$approach),inT,"initArea","Modified","Delta%\n",sep="\t")
cat(unlist(sapply(Entries,prepareOutput,object=object0)))
cat("\n\n")


}
#~ return(" ")
}

prepareOutput <- function(x,object){
if(compact){ branch <- strsplit(object$upper[[x]]$branchToEval,split=":")[[1]][2]
	}else{
	branch <- unlist(object$upper[[x]]$branchToEval)}
	
	if(is.na(branch)){branch <- unlist(object$upper[[x]]$branchToEval)}
	
cat(branch,
unlist(object$upper[[x]]$bestInitialArea),
unlist(object$upper[[x]]$bestModifiedArea),
unlist(object$upper[[x]]$delta),
unlist(object$lower[[x]]$bestModifiedArea),
unlist(object$lower[[x]]$delta),"\n",sep="\t")
}
	
	Entries <- 1:length(object0$lower)

if(compact){inT <- "\nBranchNumber"}else{inT <- "\nTerminalsInBranch"}


cat(inT,"initArea","Mod-upper","Delta%","Mod-lower","Delta%\n",sep="\t")
cat(unlist(sapply(Entries,prepareOutput,object=object0)))
cat("\n\n")

#~ return(" ")
}




