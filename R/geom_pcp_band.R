#' Bands for the parallel coordinate plot
#'
#' To add bands from factor to factor
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
#' @param freespace A number in 0 to 1 (excluded). The total gap space among levels within each factor variable
#' @param boxwidth A number or a numeric vector (length equal to the number of factor variables) for the widths of the boxes for each factor variable
#' @param rugwidth A number or a numeric vector (length equal to the number of numeric variables) for the widths of the rugs for numeric variable
#' @param interwidth A number or a numeric vector (length equal to the number of variables minus 1) for the width for the lines between every neighboring variables, either
#'  a scalar or a vector.
#' @param resort A integer or a integer vector to indicate the positions of vertical axes inside (can't be the boundary of) a sequence of factors.
#' To break three or more factors into sub factor blocks,
#' and conduct resort at the axes. Makes the plot clearer for adjacent factor variables.
#' @param reverse reverse the plot, useful especially when you want to reverse the structure in factor blocks,
#' i.e. to become more ordered from right to left
#' @param merge To merge the bands or not
#' @import ggplot2
#' @export geom_pcp_band
#' @examples
#' library(ggplot2)
#' library(dplyr)
#' data(mtcars)
#' mtcars %>%
#' mutate(cyl = factor(cyl),
#'       vs = factor(vs),
#'       am = factor(am),
#'       gear = factor(gear)) %>%
#'  ggplot(aes(vars = vars(cyl, vs:gear))) +
#'  geom_pcp(aes(color = vs), boxwidth = 0.2, resort = 2:3) +
#'  geom_pcp_box(boxwidth = 0.2) +
#'  geom_pcp_band(boxwidth = 0.2, resort = 2:3) +
#'  geom_pcp_text(boxwidth = 0.2)

geom_pcp_band <- function(mapping = NULL, data = NULL,
                          stat = "pcpband", position = "identity",
                          ...,
                          freespace = 0.1,
                          boxwidth = 0,
                          rugwidth = 0,
                          interwidth = 1,
                          resort = NULL,
                          reverse = FALSE,
                          merge = FALSE,
                          na.rm = FALSE,
                          show.legend = NA,
                          inherit.aes = TRUE) {

  ll <- layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomPcpband,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      freespace = freespace,
      boxwidth = boxwidth,
      rugwidth = rugwidth,
      interwidth = interwidth,
      resort = resort,
      reverse = reverse,
      merge = merge,
      na.rm = na.rm,
      ...
    )
  )

  ll$comp_aes <- ll$compute_aesthetics
  ll$compute_aesthetics <- compute_aesthetics_pcp
  ll$setup_layer <- setup_layer_pcp

  ll
}


GeomPcpband <- ggproto("GeomPcpband", Geom,

                       default_aes = aes(colour = "grey30", size = 0.5, linetype = 1, alpha = NA, fill = NA),

                       draw_group = function(data, panel_params, coord,
                                             na.rm = na.rm) {

                         GeomRibbon$draw_group(data, panel_params, coord,
                                               na.rm = na.rm)
                       }
)
