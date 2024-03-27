

printMultiBlepd <- function(obj,tabular=TRUE) {
               if(tabular) cat("\nDelta (%)\tTerminal\tInitial\tSelected\tEvaluating",obj[[1]]$approach,"\n")	
	           
	           for( contador in 1:length(obj) ){
				   
				   cat(obj[[contador]]$delta,"\t",obj[[contador]]$tipToEval ,"\t")
					   
                   printBlepd(obj[[contador]],tabular=tabular)
                   
                }
}

