
#### printing


print.blepd <- function(obj,tabular=FALSE, compact=FALSE) {
				if(compact){
					    numAreas <- dim(obj$distribution)[1]

						tabTemp0 <- data.frame(matrix(NA,1,(numAreas+2)))
		
						tabTemp0[1, c(1:numAreas)] <- obj$initialPD

						tabTemp0[1, numAreas+1] <- paste0(as.character(unlist(obj$bestModifiedArea[,1])),collapse=" / ")

						tabTemp0[1,numAreas+2] <- paste0(as.character(unlist(obj$bestModifiedArea[,2])),collapse=" / ")


						colnames(tabTemp0) <- c(row.names(obj$distribution),"Swap-areas","Swap-values")

						row.names(tabTemp0) <- ""

						return(tabTemp0)
					}
	
	           if(!tabular) cat("\nBestInitial:",obj$bestInitialArea,"\n\tPD",
	           c(row.names(obj$distribution)),"\n\t   ",obj$initialPD)
               if(!tabular){ cat("\n") }else{ cat("\t") }
                ##if(!tabular) cat("Selected:\t")
                  if(tabular){
			      cat(obj$bestModifiedArea,"\n")
			      }else{obj$bestModifiedArea$Percent <- round(obj$bestModifiedArea$Freq / obj$nTimes * 100,2)
			    print(obj$bestModifiedArea)
						}
}


##! 2022 03 11
##! file left to "" to text whether is plausible to use write as a
##! generic function to stdout and file

write.blepd <- function(obj,tabular=FALSE, file="") {
	           if(!tabular) cat("\nBestInitial:",file=file)
               cat(obj$bestInitialArea,
                   file = file,
                   append =TRUE)
               if(!tabular){ 
			cat("\n",
			file = file,
                        append =TRUE)
                   }else{
		        cat("\t",
			file = file,
			append =TRUE)
                   }
                   
                ##if(!tabular) cat("Selected:\t")
                if(tabular){
			    cat(unlist(obj$bestModifiedArea),"\n",
			        file = file,
                                append =TRUE)
			   }else{
			    obj$bestModifiedArea$Percent <- obj$bestModifiedArea$Freq / obj$nTimes * 100
			    write.table(obj$bestModifiedArea,
					file = file,
                                        append =TRUE)
				}
}


