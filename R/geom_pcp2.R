#' Wrapper for a list
#'
#' @param ... Unquoted variables going into the product plot.
#' @export
product <- function(...) {
  rlang::exprs(...)
}


#' Parallel coordinate plot for both numeric and categorical data
#'
#' The parallel coordinate plot displays multiple y-axes, and shows the observations across
#' several dimensions as ploi-lines. This function work well with both numeric and categorical
#' variables at the same time after proper scaling.
#'
#' @param mapping Set of aesthetic mappings created by [aes()] or
#'   [aes_()]. If specified and `inherit.aes = TRUE` (the
#'   default), it is combined with the default mapping at the top level of the
#'   plot. You must supply `mapping` if there is no plot mapping.
#' @param data The data to be displayed in this layer. There are three
#'    options:
#'
#'    If `NULL`, the default, the data is inherited from the plot
#'    data as specified in the call to [ggplot()].
#'
#'    A `data.frame`, or other object, will override the plot
#'    data. All objects will be fortified to produce a data frame. See
#'    [fortify()] for which variables will be created.
#'
#'    A `function` will be called with a single argument,
#'    the plot data. The return value must be a `data.frame`, and
#'    will be used as the layer data.
#' @param stat The statistical transformation to use on the data for this
#'    layer, as a string.
#' @param position Position adjustment, either as a string, or the result of
#'    a call to a position adjustment function.
#' @param na.rm If `FALSE`, the default, missing values are removed with
#'    a warning. If `TRUE`, missing values are silently removed.
#' @param show.legend logical. Should this layer be included in the legends?
#'   `NA`, the default, includes if any aesthetics are mapped.
#'   `FALSE` never includes, and `TRUE` always includes.
#'   It can also be a named logical vector to finely select the aesthetics to
#'   display.
#' @param inherit.aes If `FALSE`, overrides the default aesthetics,
#'   rather than combining with them. This is most useful for helper functions
#'   that define both data and aesthetics and shouldn't inherit behaviour from
#'   the default plot specification, e.g. [borders()].
#' @param ... Other arguments passed on to [layer()]. These are
#'    often aesthetics, used to set an aesthetic to a fixed value, like
#'    `colour = "red"` or `size = 3`. They may also be parameters
#'    to the paired geom/stat.
#'    #'
#' @param method which method should be used to transform the values of each variable into acommon y axis? See `transform_pcp` for details.
#' @param freespace The total gap space among levels within each factor variable
#' @param boxwidth The width of the box for each factor variable
#' @param rugwidth The width of the rugs for numeric variable
#' @param interwidth The width for the lines between every neighboring variables, either
#'  a scalar or a vector.
#' @param breakpoint To break three or more factors into peices
#' @param arrow specification for arrow heads, as created by arrow()
#' @param arrow.fill fill colour to use for the arrow head (if closed). NULL means use colour aesthetic
#' @param lineend Line end style (round, butt, square)
#' @param linejoin Line join style (round, mitre, bevel)
#' @import ggplot2
#' @export geom_pcp2
#' @examples
#' library(ggplot2)
#' library(dplyr)
#' mtcars %>%
#' mutate(cyl = factor(cyl),
#'       vs = factor(vs),
#'       am = factor(am),
#'       gear=factor(gear),
#'       carb = factor(carb)) %>%
#'  gather_pcp(1:ncol(mtcars)) %>%
#'  transform_pcp(method = "uniminmax") %>%
#'  ggplot(aes(id = id, name = name, value = value, level = level, class = class)) +
#'  geom_pcp_box(boxwidth=0.1, fill=NA, colour="grey70") +
#'  geom_pcp(aes(colour = mpg), boxwidth=0.1, breakpoint=9:10, size=1, alpha =0.9) +
#'  scale_colour_gradient2("mpg", mid="grey50", midpoint = 20) +
#'  theme_bw()

geom_pcp2 <- function(
  mapping = NULL, data = NULL,
  stat = "pcp2", position = "identity",
  ...,
  method = "uniminmax",
  freespace = 0.1,
  boxwidth = 0,
  rugwidth = 0,
  interwidth = 1,
  breakpoint = NULL,
  arrow = NULL,
  arrow.fill = NULL,
  lineend = "butt",
  linejoin = "round",
  na.rm = FALSE,
  show.legend = NA,
  inherit.aes = TRUE) {

  browser()

  ll <- layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomPcp2,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      method = method,
      freespace = freespace,
      boxwidth = boxwidth,
      rugwidth = rugwidth,
      interwidth = interwidth,
      breakpoint = breakpoint,
      arrow = arrow,
      arrow.fill = arrow.fill,
      lineend = lineend,
      linejoin = linejoin,
      na.rm = na.rm,
      ...
    )
  )

  defaults <- function(x, y) c(x, y[setdiff(names(y), names(x))])

  ll$compute_aesthetics = function(self, data, plot) {
    browser()
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
    calculated <- ggplot2:::is_calculated_aes(aesthetics)
    aesthetics <- aesthetics[!set & !calculated]

    # Override grouping if set in layer
    if (!is.null(self$geom_params$group)) {
      aesthetics[["group"]] <- self$aes_params$group
    }

    ggplot2:::scales_add_defaults(plot$scales, data, aesthetics, plot$plot_env)

    # Evaluate aesthetics
    evaled <- lapply(aesthetics, rlang::eval_tidy, data = data)
    evaled <- compact(evaled)

    # Check for discouraged usage in mapping
    ggplot2:::warn_for_aes_extract_usage(aesthetics, data[setdiff(names(data), "PANEL")])

    # Check aesthetic values
    nondata_cols <- ggplot2:::check_nondata_cols(evaled)
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
    ggplot2:::check_aesthetics(evaled, n)

    # Set special group and panel vars
    if (ggplot2:::empty(data) && n > 0) {
      evaled$PANEL <- 1
    } else {
      evaled$PANEL <- data$PANEL
    }
    evaled <- lapply(evaled, unname)
    evaled <- ggplot2:::as_gg_data_frame(evaled)
    evaled <- ggplot2:::add_group(evaled)
    evaled
  }

  ll$setup_layer <- function(self, data, plot) {
    browser()
    # the vars have to be defined in either self$mapping or in plot$mapping
    aes_vars <- plot$mapping$vars
    if (is.null(aes_vars))
      aes_vars <- self$mapping$vars

    if (!is.null(aes_vars)) {
      if (!is.null(self$mapping$vars))
        aes_vars <- rlang::eval_tidy(self$mapping$vars, data = data)
      if (!is.null(plot$mapping$vars))
        aes_vars <- rlang::eval_tidy(plot$mapping$vars, data = data)
      var_x <- paste0("x__", as.character(aes_vars))
    }

    # move the new aesthetics into self, that way they will be preserved
    if (is.null(self$mapping)) self$mapping <- aes()
    for (i in seq_along(var_x)) {
      self$mapping[[var_x[i]]] <- aes_vars[[i]]
    }
    if (!is.null(self$mapping$vars))
      self$mapping <- self$mapping[-grep("vars", names(self$mapping))]



    data
  }

  ll
}




GeomPcp2 <- ggproto(
  "GeomPcp2", Geom,
  setup_data = function(data, params) {
    #   we adjust the box width here?
    browser()
    cat("setup data in geom\n params$method: ")
    cat(params$method)
    data
  },

  # it seems that we can't have id = id, name = name, value = value,
  # level = level, class = class, in default_aes
  # neither does required_aes

  # required_aes = c("id", "name", "value", "level", "class"),

  default_aes = aes(
    colour = "grey30", size = 0.5, linetype = "solid", alpha = 1,
    linewidth=.1, stroke = 2, method = "uniminmax"
  ),

  draw_panel = function(data, panel_params, coord,
                        arrow = NULL,
                        arrow.fill = NULL,
                        lineend = "butt",
                        linejoin = "round",
                        na.rm = na.rm) {

    GeomSegment$draw_panel(data, panel_params, coord,
                           arrow = arrow,
                           arrow.fill = arrow.fill,
                           lineend = lineend,
                           linejoin = linejoin,
                           na.rm = na.rm)
  }
)
