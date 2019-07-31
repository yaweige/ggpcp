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
#' @param freespace The total gap space among levels within each factor variable
#' @param boxwidth The width of the box for each factor variable
#' @param rugwidth The width of the rugs for numeric variable
#' @param interwidth The width for the lines between every neighboring variables, either
#'  a scalar or a vector.
#' @import ggplot2
#' @importFrom dplyr %>% group_by ungroup arrange
#' @importFrom tidyr spread
#' @export stat_pcp_box
stat_pcp_box <- function(mapping = NULL, data = NULL,
                         geom = "polygon", position = "identity",
                         ...,
                         freespace = 0.1,
                         boxwidth = 0,
                         rugwidth = 0,
                         interwidth = 1,
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
      ...
    )
  )
}

StatPcpbox <- ggproto(
  "StatPcpbox", Stat,
  default_aes = ggplot2::aes(
    id = id, name = name, value = value, level = level, class = class,
    width = 0.75, linetype = "solid", fontsize=5,
    shape = 19, colour = "grey30",
    size = .1, fill = NA, alpha = .8, stroke = 0.1,
    linewidth=.1, weight = 1),

  compute_panel = function(data, scales,
                           freespace = 0.1,
                           boxwidth = 0.1,
                           rugwidth = 0.05,
                           interwidth = 1
  ) {
    # the following code are some of the internal part of StatPcp

    # Data preparation: to convert the input data to the form we can directly use
    # number of observations
    nobs <- max(data$id)
    # a vector to tell the class of variables
    classpcp <- data$class[1 - nobs + (1:(nrow(data)/nobs))*nobs]
    data_spread <- prepare_data(data, classpcp, nobs)

    # boxwidth
    # interval length, boxwidth, rugwidth
    # adjusted for different lengths
    if (length(interwidth) == 1) {
      interwidth <- rep(interwidth, times = length(classpcp) - 1)
    }
    interwidth <- cumsum(c(1, interwidth))

    if (length(boxwidth) == 1) {
      boxwidth <- rep(boxwidth, times = sum(classpcp == "factor"))
    }
    if (length(rugwidth) == 1) {
      rugwidth <- rep(rugwidth, times = sum(!classpcp == "factor"))
    }
    # calculate cumulated changes
    boxrugwidth <- seq_along(classpcp)
    boxrugwidth[classpcp == "factor"] <- boxwidth
    boxrugwidth[!classpcp == "factor"] <- rugwidth
    cumboxrugwidth <- cumsum(boxrugwidth)
    # calculate the ajusted position
    boxwidth_xend <-  interwidth + cumboxrugwidth
    boxwidth_xstart <- boxwidth_xend - boxrugwidth

    # box
    # fac coming from the classpcp == "factor"
    nlevels_list <- lapply(data_spread[, c(FALSE, classpcp == "factor"), drop = FALSE],
                           FUN = function(x) list(nlevels = nlevels(x),
                                                  table = table(x)))
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
    boxwidth_xstart[classpcp == "factor"],
    boxwidth_xend[classpcp == "factor"])

    data_box_x <- unlist(data_box_x)
    data_box_group <- rep(1:(length(data_box_y)/4), each = 4)

    data_box <- data.frame(x = data_box_x,
                           y = data_box_y,
                           group = data_box_group)
    data_box
  }
)
