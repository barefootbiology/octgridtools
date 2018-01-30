#' Repeat the first row at the end of the data.frame.
#'
#' @param df A data.frame
#' @return A data.frame with the first row repeated at the end.
.repeat_first <- function(df) {
    return(rbind(df, df[1, ]))
}