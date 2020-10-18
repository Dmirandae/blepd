

#par(mfrow=c(2,1))

a  <- qplot(RhinoclemmysData$tree$edge.length,
            bins=12,
            main=treeFiles,
            xlab="Branch length: internals and terminals") +
            xlim(c(0.001,0.025)) +
            ylim(c(0,5))


terminals <- RhinoclemmysData$tree$edge[,2] < 1+length(RhinoclemmysData$tree$tip.label)


b  <- qplot(RhinoclemmysData$tree$edge.length[terminals],
            bins=12,
            #main=treeFiles,
            xlab="Branch length: terminals")  +
            xlim(c(0.001,0.025)) +
            ylim(c(0,5))


c  <- qplot(RhinoclemmysData$tree$edge.length[!terminals],
            bins=12,
            #main=treeFiles,
            xlab="Branch length: internals")  +
            xlim(c(0.001,0.025)) +
            ylim(c(0,5))


#~ 

cowplot::plot_grid(a, b, c, nrow=3)
