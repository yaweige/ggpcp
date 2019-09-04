# helper function
defaults <- function(x, y) c(x, y[setdiff(names(y), names(x))])

#' Custom function for pcp layer
#'
#' straight copy from layer-.r in ggplot2, except for the first block
#' to remove the vars mapping from the plot
#' @importFrom utils getFromNamespace packageVersion
#' @importFrom rlang as_label
#' @noRd
compute_aesthetics_pcp <- function(self, data, plot) {
  # move the vars aesthetics out of the mapping
  if (!is.null(plot$mapping$vars))
    plot$mapping <- plot$mapping[-grep("vars", names(plot$mapping))]

  # For annotation geoms, it is useful to be able to ignore the default aes
  if (self$inherit.aes) {
    aesthetics <- defaults(self$mapping, plot$mapping)
  } else {
    aesthetics <- self$mapping
  }

  # Drop aesthetics that are set or calculated
  set <- names(aesthetics) %in% names(self$aes_params)
  calculated <- getFromNamespace("is_calculated_aes", asNamespace("ggplot2"))(aesthetics)
  aesthetics <- aesthetics[!set & !calculated]

  # Override grouping if set in layer
  if (!is.null(self$geom_params$group)) {
    aesthetics[["group"]] <- self$aes_params$group
  }

  getFromNamespace("scales_add_defaults", asNamespace("ggplot2"))(plot$scales, data, aesthetics, plot$plot_env)

  # Evaluate aesthetics
  evaled <- lapply(aesthetics, rlang::eval_tidy, data = data)

#  evaled <- compact(evaled) ## HH: not sure that we need that

  # Check for discouraged usage in mapping
  if (packageVersion("ggplot2") == '3.2.1.9000')
  getFromNamespace("warn_for_aes_extract_usage", asNamespace("ggplot2"))(aesthetics, data[setdiff(names(data), "PANEL")])

  # Check aesthetic values
  nondata_cols <- getFromNamespace("check_nondata_cols", asNamespace("ggplot2"))(evaled)
  if (length(nondata_cols) > 0) {
    msg <- paste0(
      "Aesthetics must be valid data columns. Problematic aesthetic(s): ",
      paste0(vapply(nondata_cols, function(x) {paste0(x, " = ", as_label(aesthetics[[x]]))}, character(1)), collapse = ", "),
      ". \nDid you mistype the name of a data column or forget to add stat()?"
    )
    stop(msg, call. = FALSE)
  }

  n <- nrow(data)
  if (n == 0) {
    # No data, so look at longest evaluated aesthetic
    if (length(evaled) == 0) {
      n <- 0
    } else {
      n <- max(vapply(evaled, length, integer(1)))
    }
  }
  getFromNamespace("check_aesthetics", asNamespace("ggplot2"))(evaled, n)

  empty <- getFromNamespace("empty", asNamespace("ggplot2"))
  # Set special group and panel vars
  if (empty(data) && n > 0) {
    evaled$PANEL <- 1
  } else {
    evaled$PANEL <- data$PANEL
  }
  evaled <- lapply(evaled, unname)

  evaled <- getFromNamespace("as_gg_data_frame", asNamespace("ggplot2"))(evaled)
  evaled <- getFromNamespace("add_group", asNamespace("ggplot2"))(evaled)
  evaled
}

#' function to setup the vars mapping in pcps
#' @noRd
#' @importFrom tidyselect vars_select
#' @importFrom dplyr tbl_vars
#' @importFrom rlang eval_tidy
setup_layer_pcp <- function(self, data, plot) {
  #browser()
  # the vars have to be defined in either self$mapping or in plot$mapping
  aes_vars <- plot$mapping$vars
  if (is.null(aes_vars))
    aes_vars <- self$mapping$vars

  if (!is.null(aes_vars)) {
    if (!is.null(self$mapping$vars))
      aes_vars <- rlang::eval_tidy(self$mapping$vars)
    if (!is.null(plot$mapping$vars))
      aes_vars <- rlang::eval_tidy(plot$mapping$vars)
    idx <- getFromNamespace("vars_select_eval","tidyselect")(names(data), aes_vars)
    aes_vars <- names(data)[unlist(idx)]

    var_x <- paste0("x__", 1:length(aes_vars), "__", as.character(aes_vars))
  }

  # move the new aesthetics into self, that way they will be preserved
  # HH: what happens if we are dealing with different data sets?
  if (is.null(self$mapping)) self$mapping <- aes()
  for (i in seq_along(var_x)) {
    self$mapping[[var_x[i]]] <- as.symbol(eval(aes_vars[[i]]))
  }
  if (!is.null(self$mapping$vars))
    self$mapping <- self$mapping[-grep("vars", names(self$mapping))]

  data
}
