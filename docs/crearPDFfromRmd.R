library(devtools)
library(rmarkdown)

archivos <- dir(pattern=".Rmd")

for(nombre in archivos){
#render(input=nombre, "all")
render(input=nombre, "all")

}
#render(input="brachLengthEvalInTaxRich.Rmd", "all", run_pandoc=FALSE, clean=FALSE)
q()
