path = "plain_doc.txt"

convertDolarToEqn = function(path, lines)
{
  text = paste(readLines(path)[lines], collapse = " ")
  
  pos = gregexpr("\\$",text)[[1]]  
  pos = pos[seq_along(pos)%%2==1]

  text = strsplit(text,"\\$")[[1]]

  text
  for(i in head(seq_along(text),-1))
  {
   if(i%%2 == 1) text[1] = paste(text[1], "\\eqn{",text[2])
   if(i%%2 == 0) text[1] = paste(text[1], "}",text[2])
   text = text[-2]
  }
  
  remove_list = c("\\\\mathbb","\\\\textbf")
  for(rem in remove_list) text = gsub(rem,"",text)
  cat(text)
  
}

# Asymmetry curve
convertDolarToEqn(path, 51:58)
#binningDepth2d
convertDolarToEqn(path, 110:114)
#CovLP
convertDolarToEqn(path, 154:165)
