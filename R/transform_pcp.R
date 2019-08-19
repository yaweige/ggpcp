#' Transforming values in a parallel coordinate plot
#'
#' Set of transformations to use in parallel coordinate plots. All transformation aim for y values in the interval [0,1].
#'
#' \code{method} is a character string that denotes how to scale the variables
#' in the parallel coordinate plot. Options are named in the same way as the options in `ggparcoord` (GGally):
#' \itemize{
#'   \item{\code{raw}}{: raw data used, no scaling will be done.}
#'   \item{\code{std}}{: univariately, subtract mean and divide by standard deviation. To get values into a [0,1] interval we use a linear transformation of f(y) = y/4+0.5. }
#'   \item{\code{robust}}{: univariately, subtract median and divide by median absolute deviation. To get values into a [0,1] interval we use a linear transformation of f(y) = y/4+0.5. }
#'   \item{\code{uniminmax}}{: univariately, scale so the minimum of the variable is zero, and the maximum is one.}
#'   \item{\code{globalminmax}}{: gobal scaling; the global maximum is mapped to 1,
#'     global minimum across the variables is mapped to 0. }
#' }
#' @param data data frame as returned by gather_pcp
#' @param method string specifying the method that should be used for scaling the values
#' in a parallel coordinate plot (see Details).
#' @importFrom stats median mad
#' @importFrom assertthat assert_that has_name
#' @noRd
transform_pcp <- function(data, method = "uniminmax") {

  level <-  value <- name <- NULL # hack to make R CMD CHECK happy
  assert_that(has_name(data, "name"))
  assert_that(has_name(data, "level"))
  assert_that(has_name(data, "value"))

  assert_that(!is.null(method))
  assert_that(method %in% c("raw", "uniminmax", "robust", "std", "globalminmax"))

  data$value_text <- data$value
  # any of the transformations work only in case level is a numeric value.
  # in case it is not (character variables) this probably goes wrong. Should check on it.
  if (method == "raw") {
    data <- group_by(data, name)
    data <- mutate(
      data,
      value = level
    )
  }
  if (method == "std") {
    data <- group_by(data, name)
    data <- mutate(
      data,
      value = scale(level, center = TRUE, scale=TRUE),
      value = value/4.0+0.5
    )
  }
  if (method == "robust") {
    data <- group_by(data, name)
    data <- mutate(
      data,
      value = (level - median(level, na.rm = TRUE)) / mad(level, na.rm = TRUE),
      value = value/4.0+0.5
    )
  }

  if (method == "uniminmax") {
    data <- group_by(data, name)
    data <- mutate(
      data,
      value = (level-min(level, na.rm=TRUE))/(max(level, na.rm=TRUE)-min(level, na.rm=TRUE))
    )
  }
  if (method == "globalminmax") {
    # no grouping
    data <- mutate(
      data,
      value = (level-min(level, na.rm=TRUE))/(max(level, na.rm=TRUE)-min(level, na.rm=TRUE))
    )
  }

  data
}
