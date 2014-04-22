

findPattern = function(pattern)
{
x = dir("../depthproc/pkg/R/",full.names=TRUE)
res = NULL
for(i in 1:length(x))
{
  tmp = readLines(x[i])
  match = which(grepl(pattern,tmp))
  
  if(length(match)>0)
  {
    a = paste(tail(strsplit(x[i],"/")[[1]],1) , paste(match,collapse=","))
    res = c(res,a)
  }

}
return(res)
}

findPattern("require")
