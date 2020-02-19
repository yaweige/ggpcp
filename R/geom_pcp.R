#' Parallel coordinate plot for both numeric and categorical data
#'
#' The parallel coordinate plot displays multiple y-axes, and shows the observations across
#' several dimensions as lines. This function work well with both numeric and categorical
#' variables at the same time after proper scaling.
#'
#' \code{method} is a character string that denotes how to scale the variables
#' in the parallel coordinate plot. Options are named in the same way as the options in `ggparcoord` (GGally):
#' \itemize{
#'   \item{\code{raw}}{: raw data used, no scaling will be done.}
#'   \item{\code{std}}{: univariately, subtract mean and divide by standard deviation. To get values into a [0,1] interval we use a linear transformation of f(y) = y/4+0.5. }
#'   \item{\code{robust}}{: univariately, subtract median and divide by median absolute deviation. To get values into a [0,1] interval we use a linear transformation of f(y) = y/4+0.5. }
#'   \item{\code{uniminmax}}{: univariately, scale so the minimum of the variable is zero, and the maximum is one.}
#'   \item{\code{globalminmax}}{: gobal scaling; the global maximum is mapped to 1,
#'     global minimum across the variables is mapped to 0. }
#' }
#'
#' \code{overplot} is a character string that denotes how to conduct overplotting
#' in the parallel coordinate plot. The lines from \code{geom_pcp()}  are drawn according to the order they shown in your data set in default.
#' Note that this argument provides a framework, the order in the original data still has a role in overplotting,
#' especially for lines outside factor blocks(for \code{hierarchical} only), plots with \code{breakpoint} turned on(for methods except \code{hierarchical}):
#' \itemize{
#'   \item{\code{original}}{: use the original order, first shown first drawn.}
#'   \item{\code{hierarchical}}{: hierarchically drawn according to the combinations of levels of factor variables,
#'   which will change according to different level structures of factor variables you provided.
#'   This was done separately for each factor block. The right most factor variables have the largest weight across a sequence of factor variables,
#'   the last level of a factor variable has the largest weight within a factor variable.
#'   Groups of lines with larger weight will be drawn on top. Lines outside of factor blocks still use the original order, which is different from other methods.}
#'   \item{\code{smallfirst}}{: smaller groups of lines are drawn first, placing large groups of lines on top.}
#'   \item{\code{largefirst}}{: larger groups of lines are drawn first, placing small groups of lines on top.}
#' }
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
#'
#' @param method string specifying the method that should be used for scaling the values
#' in a parallel coordinate plot (see Details).
#' @param freespace The total gap space among levels within each factor variable.
#' @param boxwidth The width of the boxes for factor variables, either a scalar or a vector with length equal to the number of factor variables.
#' @param rugwidth The width of the rugs for numeric variables,either a scalar or a vector with length equal to the number of numeric variables.
#' @param interwidth The width for the lines between every neighboring variables, either
#'  a scalar or a vector with length equal to the total number of variables subtracting one.
#' @param breakpoint Positions indicating where to break, can be a vector.
#' To break three or more factor variables to better show the relations between adjacent factor variables.
#' Can't be used when there is only one or two factor vairbles.
#' @param overplot methods used to conduct overplotting when overplotting becomes an issue.
#' @param mirror mirror the plot, useful especially when you want to reverse the structure in factor block
#' @param arrow specification for arrow heads, as created by arrow()
#' @param arrow.fill fill colour to use for the arrow head (if closed). NULL means use colour aesthetic
#' @param lineend Line end style (round, butt, square)
#' @param linejoin Line join style (round, mitre, bevel)
#' @import ggplot2
#' @export geom_pcp
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
#'  geom_pcp(aes(color = vs), boxwidth = 0.2, breakpoint = 2:3) +
#'  geom_pcp_box(boxwidth = 0.2) +
#'  geom_pcp_band(boxwidth = 0.2, breakpoint = 2:3) +
#'  geom_pcp_text(boxwidth = 0.2)

geom_pcp <- function(
  mapping = NULL, data = NULL,
  stat = "pcp", position = "identity",
  ...,
  method = "uniminmax",
  freespace = 0.1,
  boxwidth = 0,
  rugwidth = 0,
  interwidth = 1,
  breakpoint = NULL,
  overplot = "hierarchical",
  mirror = FALSE,
  arrow = NULL,
  arrow.fill = NULL,
  lineend = "butt",
  linejoin = "round",
  na.rm = FALSE,
  show.legend = NA,
  inherit.aes = TRUE) {

  ll <- layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomPcp,
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
      overplot = overplot,
      mirror = mirror,
      arrow = arrow,
      arrow.fill = arrow.fill,
      lineend = lineend,
      linejoin = linejoin,
      na.rm = na.rm,
      ...
    )
  )
  ll$comp_aes <- ll$compute_aesthetics
  ll$compute_aesthetics <- compute_aesthetics_pcp
  ll$setup_layer <- setup_layer_pcp

  ll
}




GeomPcp <- ggproto(
  "GeomPcp", Geom,

  default_aes = aes(
    colour = "grey30", size = 0.5, linetype = "solid", alpha = 1,
    linewidth=.1, stroke = 2, method = "uniminmax", vars = NULL
  ),

  draw_panel = function(data, panel_params, coord,
                        arrow = NULL,
                        arrow.fill = NULL,
                        lineend = "butt",
                        linejoin = "round",
                        na.rm = na.rm) {
    # if (arrange)
    #   data <- data %>% group_by(group) %>% mutate(n = n()) %>% arrange(desc(n))

    GeomSegment$draw_panel(data, panel_params, coord,
                           arrow = arrow,
                           arrow.fill = arrow.fill,
                           lineend = lineend,
                           linejoin = linejoin,
                           na.rm = na.rm)
  }
)
