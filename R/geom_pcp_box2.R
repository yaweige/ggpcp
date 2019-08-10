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
#'  a scalar or a vector.
#' @param rule Either `"evenodd"` or `"winding"`. If polygons with holes are
#'    being drawn (using the `subgroup` aesthetic) this argument defines how the
#'    hole coordinates are interpreted. See the examples in [grid::pathGrob()] for
#'    an explanation.
#' @import ggplot2
#' @export
geom_pcp_box2 <- function(
  mapping = NULL, data = NULL,
  stat = "pcpbox2", position = "identity",
  rule = "evenodd",
  ...,
  method = "uniminmax",
  freespace = 0.1,
  boxwidth = 0,
  rugwidth = 0,
  interwidth = 1,
  na.rm = FALSE,
  show.legend = NA,
  inherit.aes = TRUE) {

  ll <- layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomPcpbox2,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      method = "uniminmax",
      freespace = freespace,
      boxwidth = boxwidth,
      rugwidth = rugwidth,
      interwidth = interwidth,
      na.rm = na.rm,
      rule = rule,
      ...
    )
  )

  ll$compute_aesthetics <- compute_aesthetics_pcp
  ll$setup_layer <- setup_layer_pcp

  ll
}


GeomPcpbox2 <- ggproto(
  "GeomPcpbox2", Geom,
  # setup_data = function(data, params) {
  #   we adjust the box width here?
  # }
  # required_aes = c("id", "name", "value", "level", "class"),
  # default_aes = ggplot2::aes(
  #   id = id, name = name, value = value, level = level, class = class,
  #   width = 0.75, linetype = "solid", fontsize=5,
  #   shape = 19, colour = "grey30",
  #   size = .1, fill = "grey30", alpha = .8, stroke = 0.1,
  #   linewidth=.1, weight = 1),

  default_aes = aes(colour = "grey30", fill = NA, size = 0.5, linetype = 1,
                    alpha = NA, subgroup = NULL),

  draw_panel = function(data, panel_params,
                        coord,
                        rule = "evenodd") {

    GeomPolygon$draw_panel(data, panel_params, coord,
                           rule = rule)
  }
)
