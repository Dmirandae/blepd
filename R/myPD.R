myPD <- function (tree = tree , distribution = distribution, root =FALSE){
    

    
    PDcalc <- pd(samp =distribution,tree = tree,include.root = root)$PD
    
        return(PDcalc)    


  #c <- cophenetic(tree)
  #AvTD <- taxondive(comm = distribution ,
  #                  dis = c,
  #                  match.force = T)            
  #res <- AvTD$Dplus
  #res[is.nan(res)] <- 0
                    
}
