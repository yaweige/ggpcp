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
#' @param arrow specification for arrow heads, as created by arrow()
#' @param arrow.fill fill colour to use for the arrow head (if closed). NULL means use colour aesthetic
#' @param lineend Line end style (round, butt, square)
#' @param linejoin Line join style (round, mitre, bevel)
#' @import ggplot2
#' @export geom_pcp

geom_pcp <- function(mapping = NULL, data = NULL,
                     # where was "boxplot" created, does the following work?
                     stat = "pcp", position = "identity",
                     ...,
                     arrow = NULL,
                     arrow.fill = NULL,
                     lineend = "butt",
                     linejoin = "round",
                     na.rm = FALSE,
                     show.legend = NA,
                     inherit.aes = TRUE) {
  layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomPoint,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      arrow = NULL,
      arrow.fill = NULL,
      lineend = "butt",
      linejoin = "round",
      na.rm = na.rm,
      ...
    )
  )
}


GeomPcp <- ggproto("GeomPcp", Geom,
                   # setup_data = function(data, params) {
                   #   we adjust the box width here?
                   # }
                   required_aes = c("id", "name", "value", "level", "class"),
                   default_aes = ggplot2::aes(
                      id = id, name = name, value = value, level = level, class = class,
                      width = 0.75, linetype = "solid", fontsize=5,
                      shape = 19, colour = "grey30",
                      size = .1, fill = "grey30", alpha = .8, stroke = 0.1,
                      linewidth=.1, weight = 1),


                   draw_panel = function(data, panel_params, coord,
                                         arrow = NULL,
                                         arrow.fill = NULL,
                                         lineend = "butt",
                                         linejoin = "round",
                                         na.rm = na.rm) {
                     pcp_segment <- data.frame(
                       x = data$x,
                       xend = data$xend,
                       y = data$y,
                       yend = data$yend,
                       # And what about other parameters?
                       colour = data$colour,
                       size = data$size,
                       linetype = data$linetype,
                       #fill = alpha(data$fill, data$alpha),
                       alpha = data$alpha,
                       # is there PANEL or group? How those work...
                       PANEL = data$PANEL
                       #group = data$group
                     )

                     GeomSegment$draw_panel(pcp_segment, panel_params, coord,
                                            arrow = arrow,
                                            arrow.fill = arrow.fill,
                                            lineend = lineend,
                                            linejoin = "round",
                                            na.rm = na.rm)
                   }
)
