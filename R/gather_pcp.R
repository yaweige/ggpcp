#' gather for parallel coordinate plot
#'
#' It should be used before using other drawing functions
#'
#' @param  data The data set used
#' @param ... choose the columns to be used
#' @return dataframe with gathered data and some extra columns
#' @importFrom dplyr left_join
#' @importFrom tidyr gather
#' @noRd
gather_pcp <- function(data, ...) {
  name <- value <- NULL # make R CMD CHECK happy
  # coerce character variable
  data <- data.frame(lapply(data, FUN = function(x){
    if(is.character(x)) {
      output <- factor(x)
    } else {
      output <- x
    }
    output
  }))

  subdata <- data[,eval(...), drop=FALSE]
  data$id__ <- 1:nrow(data)
  data[, eval(...)] <- lapply(subdata, FUN = as.character)
  gather_data <- gather(data, name, value, eval(...))
  gather_data$level <- unlist(lapply(subdata, FUN = as.numeric))
  gather_data$class <- rep(unlist(lapply(subdata, FUN = function(x)
    paste(class(x), collapse=" "))), each = nrow(data))
  gather_data$class <- gsub("[oO]rdered ", "", gather_data$class) # just ignore the ordered factors


  add_names <- c("id__", setdiff(names(data), names(gather_data)))

  gather_data_wide <- left_join(gather_data, data[, add_names], by="id__")
  gather_data_wide
}

