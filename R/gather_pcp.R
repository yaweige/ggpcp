#' gather for parallel coordinate plot
#'
#' It should be used before using other drawing functions
#'
#' @param  data The data set used
#' @param ... choose the columns to be used
#' @export gather_pcp
#' @importFrom dplyr left_join %>% select
#' @importFrom rlang enquos !!!

gather_pcp <- function(data, ...) {
  columns<- enquos(...)

  originaldata <- data # HH: fix for below

  data <- data %>% select(!!!columns)
  #data <- data[,columns] # HH: that deletes EVERYTHING else that's not shown in the parallel coordinate plot
  # ggplot will delete columns that are not needed after the plot specification is done.

  data_id <- rep(1:nrow(data), ncol(data))
  data_name <- rep(colnames(data), each = nrow(data))
  data_value <- unlist(lapply(data, FUN = as.character))
  data_level <- unlist(lapply(data, FUN = as.numeric))
  data_class <- rep(unlist(lapply(data, FUN = class)), each = nrow(data))

  gather_data <- data.frame("id" = data_id,
                            "name" = data_name,
                            "value" = data_value,
                            "level" = data_level,
                            "class" = data_class,
                            stringsAsFactors = FALSE)
  originaldata$id <- 1:nrow(originaldata)
  add_names <- c("id", setdiff(names(originaldata), names(gather_data)))

  gather_data_wide <- left_join(gather_data, originaldata[, add_names], by="id")
  gather_data_wide
}
