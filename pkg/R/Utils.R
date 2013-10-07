### Theme for ggplot2

.depTheme= function()
{
  return(theme(axis.title.x = element_text(face = "bold", vjust = 0, size = 16),
               axis.title.y = element_text(face = "bold", angle = 90, vjust = 0.2, size = 16),
               axis.text.x = element_text(size = 14),
               axis.text.y = element_text(size = 14),
               title = element_text(face = "bold", vjust = 1, size = 18)
  ))
}