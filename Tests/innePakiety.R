install.packages("depth")
install.packages("depthTools")
install.packages("localdepth")


require(localdepth)
require(depthTools)

set.seed(0)
x <- matrix(rnorm(100),10,10)
depthTools::scalecurve(x)

?scalecurve


data(prostate)
prost.x<-prostate[,1:100]
prost.y<-prostate[,101]
centralPlot(prost.x[prost.y==0,], p=0.5) ## 50 % most central normal samples




### Opis funckji:
extract_help <- function(pkg, fn = NULL, to = c("txt", "html", "latex", "ex"))
{
  to <- match.arg(to)
  rdbfile <- file.path(find.package(pkg), "help", pkg)
  rdb <- tools:::fetchRdDB(rdbfile, key = fn)
  convertor <- switch(to, 
                      txt   = tools::Rd2txt, 
                      html  = tools::Rd2HTML, 
                      latex = tools::Rd2latex, 
                      ex    = tools::Rd2ex
  )
  f <- function(x) capture.output(convertor(x))
  if(is.null(fn)) lapply(rdb, f) else f(rdb)
}


getFuncDesc = function(pkg)
{
  func = extract_help(pkg, to ="html")
  sapply(seq_along(func),
         function(i)
         {
           x = func[[i]]
           x[grep(pattern = "Description", x)]
           pos = grep(pattern = "<h3>",x)[1:2]
            if(is.na(pos[2])) return(NULL)
           
           xcl = x[pos[1]:(pos[2]-1)]
           
           cleanFun <- function(htmlString) {
             return(gsub("<.*?>", "", htmlString))
           }
           
           desc = paste(cleanFun(xcl)[-1], collapse = "")
           paste("[",names(func)[i],"]", desc, sep = " ")
         })
}



getFuncDescLatex = function(pkg)
{
  desc = getFuncDesc(pkg)
  desc = paste("\\item",desc, sep = "")
  desc = c("\\begin{itemize}",desc,"\\end{itemize}")
  desc = paste(desc, collapse = "\n")
  cat(desc)
}


getFuncDescLatex("localdepth")

getFuncDescLatex("depth")

getFuncDescLatex("depthproc")



