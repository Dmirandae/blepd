#'
#' @title evalTree
#'
#' @description The function is a wrap for evalTerminal and calculates whether changes in all the terminal branches length generate changes in the area selected; and when applies, the terminal branch length value for that change(s).  
#' 
#' @return A data.frame object with 14 fields:  labelTerminal, lowerBranchLength, InitialArea, lowerFinalArea, initialLength, upperBranchLength, upperFinalArea, changeLower, changeUpper, deltaUpper, deltaLower.   deltaPD,  areaDelta, and  abDelta.
#'
#' @param tree is a single tree with T terminals, an APER phylo object.
#' 
#' @param distribution is a labeled matrix object, with the distribution of T terminals (rows) in A areas (columns).
#' 
#' @param root is use.root in PD function. 
#' 
#' 
#' @examples
#' library(blepd)
#' data(tree)
#' data(distribution)
#' evalTree(tree = tree , distribution = distribution , root = FALSE)
#'
#'
#'@author Miranda-Esquivel Daniel R.
#'
#'

evalTree <- function(tree = tree, distribution = distribution,  root=FALSE, index = "PD"){
     
	## potential errors
	
	.checkInput(tree = tree , distribution = distribution)

##cat(approach)

#maxLength <- 2*max(tree$edge.length)

results <-  data.frame(
                labelTerminal         =   character() , #1
                lowerBranchLength     =   double(),
                InitialArea           =   character() , #3
                lowerFinalArea        =   character() , 
                initialLength         =   double(),     #5
                upperBranchLength     =   double(),
                upperFinalArea        =   character() , #7
                stringsAsFactors      =   FALSE) 

counter <- 1

for(terminal in tree$tip.label){
  
  results[counter,1] <- terminal  
  
  evalTemp <- evalTerminal( tree = tree, 
                            distribution = distribution, 
                            tipToEval = terminal, 
                            approach = "lower" ,
                            root = root)
  
  results[counter,c(2:5)] <- evalTemp
    
  evalTemp <- evalTerminal( tree = tree, 
                            distribution = distribution, 
                            tipToEval = terminal, 
                            approach = "upper" ,
                            root = root)
  
  results[counter,c(6,7)] <- evalTemp[c(1,3)]
        
  counter <- counter + 1

}


#results$changeLower <- paste(results$InitialArea,results$lowerFinalArea,sep = "->")

#results$changeUpper <- paste(results$InitialArea,results$upperFinalArea,sep = "->")

results$changeLower <- results$lowerFinalArea

results$changeUpper <- results$upperFinalArea

results$deltaUpper <- abs(as.numeric(results$upperBranchLength) - as.numeric(results$initialLength))

results$deltaLower <- abs(as.numeric(results$initialLength) - as.numeric(results$lowerBranchLength))

initialPD <- PDindex(tree = tree, distribution = distribution, root=root, index = index )
## remenber include root

results$deltaPD <- max(initialPD) - min(initialPD)

DeltaU <- results$deltaUpper - results$deltaPD


DeltaL <- results$deltaLower - results$deltaPD


nDelta <- (DeltaL / DeltaU+0.00000001)+1


Delta <- rep("LU",length(nDelta))
abDelta <- rep(0,length(nDelta))


tmp1 <- results$upperFinalArea == "*"

#Delta[tmp1] <- paste("Lb",results$changeLower[tmp1],sep="_")
#
Delta[tmp1] <- results$changeLower[tmp1]

abDelta[tmp1] <- results$deltaLower[tmp1]



tmp2 <- results$lowerFinalArea == "*"

#Delta[tmp2] <- paste("Ub",results$changeUpper[tmp2],sep="_")
#
Delta[tmp2] <- results$changeUpper[tmp2]

abDelta[tmp2] <- results$deltaUpper[tmp2]


tmp3 <- ((results$lowerFinalArea == "*") & (results$upperFinalArea == "*"))

Delta[tmp3] <- "notAny"

abDelta[tmp3] <- results$deltaLower[tmp3] + results$deltaUpper[tmp3]


results$areaDelta <- Delta

tmp4 <- results$areaDelta  == "LU"

results$areaDelta[tmp4] <- paste("L:",results$changeLower[tmp4],"/U:",results$changeUpper[tmp4],sep="_")

results$abDelta <- abDelta

#
results$lowerBranchLength     <-  as.numeric(results$lowerBranchLength)

#
results$initialLength         <-  as.numeric(results$initialLength)

#
results$upperBranchLength     <-  as.numeric(results$upperBranchLength)




results <- results[c("labelTerminal",
                   "InitialArea",
                   "initialLength",
                   "lowerFinalArea",
                   "lowerBranchLength",
                   "changeLower",
                   "deltaLower",
                   "upperFinalArea",
                   "upperBranchLength",
                   "changeUpper",
                   "deltaUpper",
                   "deltaPD",
                   "areaDelta",
                   "abDelta")]



return(results)

}
