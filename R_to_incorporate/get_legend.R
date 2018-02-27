# Legend function adapted from
# http://stackoverflow.com/questions/16963137/ggplot2-multiple-plots-with-different-variables-in-a-single-row-single-groupin
get_legend <- function(p){
  # Legend function adapted from
  # http://stackoverflow.com/questions/16963137/ggplot2-multiple-plots-with-different-variables-in-a-single-row-single-groupin

  tmp = ggplot_gtable(ggplot_build(p))
  leg = which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend = tmp$grobs[[leg]]
  return(legend)
}