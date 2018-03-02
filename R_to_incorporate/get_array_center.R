get_array_center <- function(a) {
  dim_a <- dim(a)

  result <- rep(as.double(NA), length(dim_a))

  for(i in 1:length(dim_a)) {
    result[[i]] <- (dim_a[[i]] - 1) / 2
  }

  result
}
