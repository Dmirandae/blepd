
options(warn=-1)
suppressMessages(library(ggtree))
#~ library(ape)
suppressMessages(library(blepd))
library(ggplot2)
#~ library(ggtree)


## Version

cat("Analyses made  with blepd version:",unlist(packageVersion("blepd")))



## Create an object to place the distribution and the tree


RhinoclemmysData <- list()


## Read data

## distribution is a csv file areas x terminals, labeled


setwd("../data/")

csvFile <- dir(pattern=".csv")

RhinoclemmysData$distribution <-  as.matrix(read.table(csvFile,
                                  stringsAsFactors=FALSE,
                                  header=TRUE,
                                  row.names=1,
                                  sep=",")
                          )


##print(t(RhinoclemmysData$distribution))



## tree(s) in nexus or newick format

treeFiles <- dir(pattern=".tre")

##treeFiles


RhinoclemmysData$tree  <-    read.tree(treeFiles)


## name of tree(s)

treeFiles <- gsub(".tre","",treeFiles)





## Plotting


#par(mfrow=c(2,1))


## The tree

## using ggtree

plotTree <-  ggtree(RhinoclemmysData$tree, ladderize=FALSE,
                    color="black", size=0.51, linetype="solid") +
             geom_tiplab(size=4, color="black") +
             xlim(0,0.035) +
             theme_tree2() +
             ggtitle(treeFiles[1])



##print(plotTree)


#~ Alternatively, we can plot the trees using  APE
##
## plot.phylo(RhinoclemmysData$tree)
##
####nodelabels()
####tiplabels()




## the distribution


distXY <- matrix2XY(RhinoclemmysData$distribution)


terminals <- colnames(RhinoclemmysData$distribution)


realOrder <- match(terminals,RhinoclemmysData$tree$tip.label)


equivalencias <- data.frame(terminals,realOrder)


dXY2 <-     distXY

for(cambiar in terminals){

    dXY2$Terminal[distXY$Terminal == cambiar]     <-
    equivalencias$realOrder[equivalencias$terminals==cambiar]

}


distGraficar <- dXY2

plotDistrib <- ggplot(distGraficar,
                      aes(x= Area, y=Terminal), size =35) +
               geom_point(shape=19, fill="white", color="darkred", size=7) +
               labs(title = "Distributions",
                    y = "",
                    x = "Area") +
               theme(axis.line=element_blank(),
					 axis.text.y=element_blank(), 
					 axis.ticks=element_blank(), 
					 axis.title.y=element_blank(), 
					 legend.position="none", 
					 panel.background=element_blank(), 
					 panel.border=element_blank(), 
					 panel.grid.major=element_blank(), 
					 panel.grid.minor=element_blank(), 
					 plot.background=element_blank()
					 ) 


##print(plotDistrib)

##
cowplot::plot_grid(plotTree, plotDistrib, ncol=2)

