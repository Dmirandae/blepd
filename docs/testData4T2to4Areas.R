
## DRME
## blepd testing
## data and ggtree

## 2018 - 01 - 16

## cleaning
rm(list = ls())

## libraries

#library(ape)
#library(vegan)
#library(picante)

library(blepd)

packageVersion("blepd")


library(ggplot2)

library("ggtree")

library(gridExtra)


## directory for test data

rutaactual <- getwd()

setwd("../blepd/testData/")

## data

## trees
initialTree <- read.tree("tree00")

## distributions
dist4taxa <- as.matrix(read.table("dist4T00",
 stringsAsFactors=FALSE,
 header=TRUE,
 row.names=1)
 )


## distributions
dist4T4A <- as.matrix(read.table("dist4T4A",
 stringsAsFactors=FALSE,
 header=TRUE,
 row.names=1)
 )



setwd(rutaactual)


## distribution to XY

distXY <- matrix2XY(dist4taxa)



## recalculate length brnach terminal A

## change the brnach value in A
## case 2 loger than the remaining branches

tipNumber <- which(initialTree$tip.label == "t1")

if(is.null(tipNumber)){quit()} 

tipPosition1 <- which(initialTree$edge[,2] == tipNumber)

tipNumber <- which(initialTree$tip.label == "t3")

if(is.null(tipNumber)){quit()} 


tipPosition2 <- which(initialTree$edge[,2] == tipNumber)

valoresModificar1 <- c(1,0.5,0.75,1.5,2)
valoresModificar2 <- c(1,0.5,1.5)

for (valor1 in valoresModificar1){

for (valor2 in valoresModificar2){
    
modifiedTree <- initialTree

modifiedTree$edge.length[tipPosition1] <- valor1

modifiedTree$edge.length[tipPosition2] <- valor2

cat("\n\ntip Nodes",tipPosition1,tipPosition2,"\nChange:",valor1,valor2,"\n")

eval <- evalTree(tree = modifiedTree, distrib = dist4taxa)


#str(eval)

cat("\nPD",myPD(tree=modifiedTree, distribution = dist4taxa),"\n\n")

print(eval)

PDvalue <- myPD(tree = initialTree, distribution = dist4taxa)

p0 <-    ggtree(modifiedTree, layout="slanted", ladderize=TRUE,
                       color="black", size=1 
                       ) + 
              #geom_tiplab(size=6, aes(color=c(as.character(eval$areaDelta)))) +
              #geom_tiplab(aes(color=c(as.character(eval$areaDelta)))) +
               #theme_tree2() +  
               theme(legend.position="right") +
                labs(title = paste("t1:",valor1,"t3:",valor2,"Four terminals, non-equal branch length; PD",PDvalue[1],PDvalue[2]))

#p0

p <- p0 %<+% eval + geom_tiplab(aes(color=paste(areaDelta,abDelta,sep="_")), size =6) 

print(p)

}}


print("root TRUE")



dist4T4A

print("PD")

myPD(tree = initialTree, distribution = dist4T4A, root=TRUE)


results <- evalTree(tree = initialTree, distrib = dist4T4A, root=TRUE)

results

class(results)
