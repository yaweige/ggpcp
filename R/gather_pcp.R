#' gather for parallel coordinate plot
#'
#' It should be used before using other drawing functions
#'
#' @param  data The data set used
#' @param columns choose the columns to be used
#' @export gather_pcp

gather_pcp <- function(data, columns) {
  data <- data[,columns]

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
}
