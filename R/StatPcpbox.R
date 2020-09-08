#' Boxes for the levels in each factor variable in parallel coordinate plot
#'
#' To add boxes showing the levels in each factor variable
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
#' @param geom The geometric object to use display the data
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
#' @param freespace A number in 0 to 1 (excluded). The total gap space among levels within each factor variable
#' @param boxwidth A number or a numeric vector (length equal to the number of factor variables) for the widths of the boxes for each factor variable
#' @param rugwidth A number or a numeric vector (length equal to the number of numeric variables) for the widths of the rugs for numeric variable
#' @param interwidth A number or a numeric vector (length equal to the number of variables minus 1) for the width for the lines between every neighboring variables, either
#'  a scalar or a vector.
#' @param reverse reverse the plot, useful especially when you want to reverse the structure in factor blocks,
#' i.e. to become more ordered from right to left
#' @import ggplot2
#' @importFrom dplyr %>% group_by ungroup arrange
#' @importFrom tidyr spread
#' @noRd
stat_pcp_box <- function(mapping = NULL, data = NULL,
                         geom = "polygon", position = "identity",
                         ...,
                         freespace = 0.1,
                         boxwidth = 0,
                         rugwidth = 0,
                         interwidth = 1,
                         reverse = FALSE,
                         na.rm = FALSE,
                         show.legend = NA,
                         inherit.aes = TRUE) {
  layer(
    data = data,
    mapping = mapping,
    stat = StatPcpbox,
    geom = geom,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      na.rm = na.rm,
      freespace = freespace,
      boxwidth = boxwidth,
      rugwidth = rugwidth,
      interwidth = interwidth,
      reverse = reverse,
      ...
    )
  )
}

StatPcpbox <- ggproto(
  "StatPcpbox", Stat,
  default_aes = ggplot2::aes(
    #id__ = id__, name = name, value = value, level = level, class = class,
    vars = NULL,
    width = 0.75, linetype = "solid", fontsize=5,
    shape = 19, #colour = "grey30", # no mapping to colour,colour can only be set
    size = .1, fill = NA, alpha = .8, stroke = 0.1,
    linewidth=.1, weight = 1),

  setup_data = function (data, params) {
    idx <- grep("x__", names(data))
    names(data) <- gsub("x__[0-9]+__", "", names(data))
    data <- data.frame(data, stringsAsFactors = TRUE)
    data <- gather_pcp(data, idx)
    data <- transform_pcp(data, method = "uniminmax")

    data
  },

  compute_layer = function(self, data, params, layout) {
    # adjust function to avoid deleting all data
    ggplot2:::check_required_aesthetics(
      self$required_aes,
      c(names(data), names(params)),
      ggplot2:::snake_class(self)
    )

    # Trim off extra parameters
    params <- params[intersect(names(params), self$parameters())]

    scales <- layout$get_scales(data$PANEL[1])
    layout$panel_scales_x <- list(xscale_pcp(data, params, layout)) # only one scale overall - might need one for each panel

    args <- c(list(data = quote(data), scales = quote(scales)), params)
    gg <- ggplot2:::dapply(data, "PANEL", function(data) {
      tryCatch(do.call(self$compute_panel, args), error = function(e) {
        warning("Computation failed in `", ggplot2:::snake_class(self), "()`:\n",
                e$message, call. = FALSE)
        ggplot2:::new_data_frame()
      })
    })
    gg
  },

  compute_panel = function(data, scales,
                           freespace = 0.1,
                           boxwidth = 0.1,
                           rugwidth = 0.05,
                           interwidth = 1,
                           reverse = FALSE,
                           method = "uniminmax"
  ) {
    # the following code are some of the internal part of StatPcp
    #browser()
    # Data preparation: to convert the input data to the form we can directly use
    # number of observations
    nobs <- length(unique(data$id__))
    # a vector to tell the class of variables
    classpcp <- data$class[data$id__==min(data$id__)]
    namepcp <- data$name[data$id__==min(data$id__)]
    data$name <- factor(data$name, levels = namepcp)
    data_spread <- prepare_data(data, classpcp, nobs)
    text_spread <- spread(data[, c("id__", "name", "value_text")], key=name, value = value_text)
    level_spread <- spread(data[, c("id__", "name", "level")], key=name, value = level)

    # boxwidth
    # interval length, boxwidth, rugwidth
    # interval length, boxwidth, rugwidth ajustment preparation
    width_adjusted <- prepare_width_ajustment(classpcp, boxwidth, rugwidth, interwidth, reverse = reverse)

    # box
    # make sure all of the levels in text_spread are in the right order
    lapply(2:ncol(text_spread), FUN = function(i) {
      if (classpcp[i-1] == "factor") {
        levels <- unique(text_spread[,i])[order(unique(level_spread[,i]))]
        text_spread[,i] <<- factor(text_spread[,i], levels=levels)
      }
    })
    # fac coming from the classpcp == "factor"
    nlevels_list <- lapply(text_spread[, c(FALSE, classpcp == "factor"), drop = FALSE],
                           FUN = function(x) {
                             list(nlevels = length(unique(x)),
                                  table = table(x))})
    eachobs <- (1 - freespace)/nobs
    level_range <- lapply(nlevels_list,
                          FUN = function(x) {
                            freespace_offset <- rep(freespace/(x$nlevels - 1), times = x$nlevels - 1)
                            freespace_offset_cum <- cumsum(freespace_offset)
                            c(0, rep(cumsum(eachobs*x$table), each = 2)[-2*x$nlevels]) +
                              c(0, 0, rep(freespace_offset_cum, each = 2))
                          })
    data_box_y <- rep(unlist(level_range), each = 2)

    data_box_x <- Map(f = function(x, y, z) {
      rep(c(y, z, z, y), times = x$nlevels)
    },
    nlevels_list,
    width_adjusted$boxwidth_xstart[classpcp == "factor"],
    width_adjusted$boxwidth_xend[classpcp == "factor"])

    data_box_x <- unlist(data_box_x)
    data_box_group <- rep(1:(length(data_box_y)/4), each = 4)

    # keep those labels
    data_labels <- unlist(lapply(nlevels_list, function(x) {
      names(x$table)
    }))
    data_box <- data.frame(x = data_box_x,
                           y = data_box_y,
                           label = rep(data_labels, each = 4),
                           group = data_box_group,
                           PANEL = data$PANEL[1] # are all the same in compute_panel
    )
    data_box
  }
)

