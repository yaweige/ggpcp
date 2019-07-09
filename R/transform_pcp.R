#' Transforming values in a parallel coordinate plot
#'
#'
#' @param data data frame as returned by gather_pcp
#' @param method string specifying the method that should be used for scaling the values
#' in a parallel coordinate plot. One of "minmax' (default),
#' @export
transform_pcp <- function(data, method = "minmax") {
  name <- NULL
  level <- NULL

  if (method == "minmax") {
    data <- group_by(data, name)
    data <- mutate(
      data,
      value = (level-min(level, na.rm=TRUE))/(max(level, na.rm=TRUE)-min(level, na.rm=TRUE))
    )
  }

  data
}
