#' gather for parallel coordinate plot
#'
#' It should be used before using other drawing functions
#'
#' @param  data The data set used
#' @param ... choose the columns to be used
#' @return dataframe with gathered data and some extra columns
#' @export
#' @importFrom dplyr left_join %>% select
#' @importFrom rlang enquos !!!
#' @importFrom stringr str_detect

gather_pcp2 <- function(data, ...) {

  subdata <- data[,eval(...)]

  data$id <- 1:nrow(data)
  data[, eval(...)] <- lapply(subdata, FUN = as.numeric)
  gather_data <- gather(data, name, value, eval(...))
  gather_data$level <- unlist(lapply(subdata, FUN = as.numeric))
  gather_data$class <- rep(unlist(lapply(subdata, FUN = class)), each = nrow(data))


  add_names <- c("id", setdiff(names(data), names(gather_data)))

  gather_data_wide <- left_join(gather_data, data[, add_names], by="id")
  gather_data_wide
}
