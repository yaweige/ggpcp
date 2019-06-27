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
                           rugwidth = 0.05
  ) {
    # the following code are some of the internal part of StatPcp
    # common data process
    data$name <- factor(data$name, levels = unique(data$name))
    data_spread <- spread(data[, c("id", "name", "value")], key = name, value = value)
    nobs <- max(data$id)
    ncol <- nrow(data)/nobs
    nvar <- length(levels(data$name))
    classpcp <- data$class[1 - nobs + (1:ncol)*nobs]
    num <- classpcp %in% c("numeric", "integer")
    fac <- classpcp == "factor"
    data_spread[, c(FALSE, num)] <-  lapply(data_spread[, c(FALSE, num), drop = FALSE],
                                            FUN = function(x) as.numeric(as.character(x)))
    if (sum(fac) != 0) {
      original_levels <- unique(data[which(data$class == "factor"),c("name", "value", "level")])
      original_levels$name <- droplevels(original_levels$name)
      original_levels <- original_levels %>%
        group_by(name) %>%
        arrange(level, .by_group = TRUE) %>%
        ungroup()

      original_levels <- split(original_levels, f = original_levels$name)

      data_spread[, c(FALSE, fac)] <- Map(f = function(x, y){
        factor(x, levels = y$value)
      },
      data_spread[, c(FALSE, fac), drop = FALSE],
      original_levels)
    }
    # boxwidth
    # for xstart, for xend
    fac_count <- cumsum(fac)
    num_count <- cumsum(!fac)
    boxwidth_xend <-  seq_along(classpcp) + fac_count*2*boxwidth + num_count*2*rugwidth
    boxwidth_xstart <- c(1, (boxwidth_xend + 1)[-length(classpcp)])

    # box
    eachobs <- 1/nobs
    # fac coming from the classpcp == "factor"
    nlevels_list <- lapply(data_spread[, c(FALSE, fac), drop = FALSE],
                           FUN = function(x) list(nlevels = nlevels(x),
                                                  table = table(x)))
    level_range <- lapply(nlevels_list,
                          FUN = function(x) c(0, rep(cumsum(eachobs*x$table)[-x$nlevels], each = 2) +
                                                freespace/(x$nlevels-1)/2*rep(c(-1, 1), times = x$nlevels-1), 1))
    data_box_y <- rep(unlist(level_range), each = 2)

    data_box_x <- Map(f = function(x, y, z) {
      rep(c(y, z, z, y), times = x$nlevels)
    },
    nlevels_list,
    boxwidth_xstart[fac],
    boxwidth_xend[fac])

    data_box_x <- unlist(data_box_x)
    data_box_group <- rep(1:(length(data_box_y)/4), each = 4)

    data_box <- data.frame(x = data_box_x,
                           y = data_box_y,
                           group = data_box_group)
    data_box
  }
)