#' gather for parallel coordinate plot
#'
#' It should be used before using other drawing functions
#'
#' @param  data The data set used
#' @param ... choose the columns to be used
#' @export gather_pcp
#' @importFrom dplyr left_join %>% select
#' @importFrom rlang enquos !!!
#' @importFrom stringr str_detect

gather_pcp <- function(data, ...) {
  #columns<- enquos(...)

  originaldata <- data # HH: fix for below
  # kind of stupid, but works for some cases now
  columns <- eval(substitute(alist(...)))
  columns <- unlist(lapply(columns, deparse))

  columns_name <- columns %in% colnames(data)

  columns_num <-stringr::str_detect(columns, pattern = "^\\d+$")
  columns[columns_num] <- colnames(data)[as.numeric(columns[columns_num])]

  # to deal with name:name, name:position pattern
  columns_colon <-  stringr::str_detect(columns, pattern = "^.+:.+$")

  for (i in seq_along(colnames(data))) {
    columns[columns_colon] <- stringr::str_replace_all(columns[columns_colon], pattern = colnames(data)[i], replacement = as.character(i))
  }


  # the following assume the results are numbers & positions, then matched to names
  evaluated_col<- lapply(as.list(columns[!columns_name & !columns_num]), function(x) {
    colnames(data)[eval(parse(text = x))]
  })
  # deal with the order of evaluated parts and the name, number parts
  insert_position <- which(!columns_name & !columns_num)

  newcol <- as.list(columns)
  for (i in seq_along(insert_position)) {
    newcol[insert_position[i]] <- evaluated_col[i]
  }

  newcol <- unlist(newcol)
  data <- data[, newcol]
  #data <- data %>% select(!!!columns)
  #data <- data[,columns] # HH: that deletes EVERYTHING else that's not shown in the parallel coordinate plot
  # ggplot will delete columns that are not needed after the plot specification is done.

  # to do the class coercion for character variables

  character_position <- which(unlist(lapply(data, is.character)))

  if (length(character_position) != 0) {
    data[, character_position] <- data.frame(lapply(data[, character_position, drop = FALSE], as.factor))
  }


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
