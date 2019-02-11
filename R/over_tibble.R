#' A wrapper for sp::over that allows one to bind data to the resulting tibble
#'
#' A wrapper for sp::over that allows one to bind data to the resulting tibble
#'
#' @export
#' @importFrom dplyr bind_cols
#' @importFrom tibble as_tibble
#' @importFrom sp over
#' @importFrom magrittr %>%
over_tibble <- function(x, y, data = NULL) {
  result <-
    over(x = x, y = y) %>%
    as_tibble()

  if (!is.null(data)) {
    result <- result %>%
      bind_cols(data) %>%
      as_tibble()
  }

  result
}
