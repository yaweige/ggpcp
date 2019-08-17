#' Texts and labels for the categories of factor variables in parallel coordinate plot
#'
#' To add texts and labels in boxes showing the levels in each factor variable
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
#' @param freespace The total gap space among levels within each factor variable
#' @param boxwidth The width of the box for each factor variable
#' @param rugwidth The width of the rugs for numeric variable
#' @param interwidth The width for the lines between every neighboring variables, either
#'   a scalar or a vector.
#' @param parse If `TRUE`, the labels will be parsed into expressions and
#'   displayed as described in `?plotmath`.
#' @param nudge_x,nudge_y Horizontal and vertical adjustment to nudge labels by.
#'   Useful for offsetting text from points, particularly on discrete scales.
#' @param check_overlap If `TRUE`, text that overlaps previous text in the
#'   same layer will not be plotted.
#' @import ggplot2
#' @export
geom_pcp_text <- function(mapping = NULL, data = NULL,
                          stat = "pcpbox", position = "identity",
                          ...,
                          freespace = 0.1,
                          boxwidth = 0,
                          rugwidth = 0,
                          interwidth = 1,
                          parse = FALSE,
                          nudge_x = 0,
                          nudge_y = 0,
                          check_overlap = FALSE,
                          na.rm = FALSE,
                          show.legend = NA,
                          inherit.aes = TRUE) {

  if (!missing(nudge_x) || !missing(nudge_y)) {
    if (!missing(position)) {

      stop("You must specify either `position` or `nudge_x`/`nudge_y`.", call. = FALSE)

    }
    position <- position_nudge(nudge_x, nudge_y)
  }

  ll <- layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomPcptext,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      freespace = freespace,
      boxwidth = boxwidth,
      rugwidth = rugwidth,
      interwidth = interwidth,
      na.rm = na.rm,
      parse = parse,
      check_overlap = check_overlap,
      ...
    )
  )

  ll$compute_aesthetics <- compute_aesthetics_pcp
  ll$setup_layer <- setup_layer_pcp

  ll
}


GeomPcptext <- ggproto(
  "GeomPcptext", Geom,
  setup_data = function(data, params) {
    #    browser()
    data_text <- data[seq(from = 1, to = nrow(data) - 3, by = 4), , drop = FALSE]
    data_text$x <- (data[seq(from = 2, to = nrow(data) -2, by = 4), "x", drop = TRUE] + data_text$x)/2
    data_text$y <- (data[seq(from = 4, to = nrow(data), by = 4), "y", drop = TRUE] + data_text$y)/2
    #data_text$group <- NULL
    data <- data_text

    data
  },

  default_aes = aes(
    colour = "black", size = 3.88, angle = 90, hjust = 0.5,
    vjust = 0.5, alpha = NA, family = "", fontface = 1, lineheight = 1.2
  ),

  draw_panel = function(data, panel_params,
                        coord, parse = FALSE,
                        na.rm = FALSE, check_overlap = FALSE) {

    GeomText$draw_panel(data, panel_params, coord, parse = parse,
                        na.rm = na.rm, check_overlap = check_overlap)
  }
)
