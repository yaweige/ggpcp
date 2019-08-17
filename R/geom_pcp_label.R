#' @rdname geom_pcp_text
#' @param label.padding Amount of padding around label. Defaults to 0.25 lines.
#' @param label.r Radius of rounded corners. Defaults to 0.15 lines.
#' @param label.size Size of label border, in mm.
#' @import ggplot2
#' @export geom_pcp_label
geom_pcp_label <- function(mapping = NULL, data = NULL,
                          stat = "pcpbox", position = "identity",
                          ...,
                          freespace = 0.1,
                          boxwidth = 0,
                          rugwidth = 0,
                          interwidth = 1,
                          parse = FALSE,
                          nudge_x = 0,
                          nudge_y = 0,
                          label.padding = unit(0.25, "lines"),
                          label.r = unit(0.15, "lines"),
                          label.size = 0.25,
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
    geom = GeomPcplabel,
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
      label.padding = label.padding,
      label.r = label.r,
      label.size = label.size,
      ...
    )
  )

  ll$compute_aesthetics <- compute_aesthetics_pcp
  ll$setup_layer <- setup_layer_pcp

  ll
}


GeomPcplabel <- ggproto("GeomPcplabel", Geom,
                       setup_data = function(data, params) {
                         data_text <- data[seq(from = 1, to = nrow(data) - 3, by = 4), , drop = FALSE]
                         data_text$x <- (data[seq(from = 2, to = nrow(data) -2, by = 4), "x", drop = TRUE] + data_text$x)/2
                         data_text$y <- (data[seq(from = 4, to = nrow(data), by = 4), "y", drop = TRUE] + data_text$y)/2
                         #data_text$group <- NULL
                         data <- data_text

                         data
                       },

                       # default_aes = aes(colour = "grey30", fill = NA, size = 0.5, linetype = 1,
                       #                   alpha = NA, subgroup = NULL),
                       default_aes = aes(
                         colour = "black", fill = "white", size = 3.88, angle = 0,
                         hjust = 0.5, vjust = 0.5, alpha = NA, family = "", fontface = 1,
                         lineheight = 1.2),

                       draw_panel = function(data, panel_params, coord, parse = FALSE,
                                             na.rm = FALSE,
                                             label.padding = unit(0.25, "lines"),
                                             label.r = unit(0.15, "lines"),
                                             label.size = 0.25) {

                         GeomLabel$draw_panel(data, panel_params, coord, parse = parse,
                                              na.rm = na.rm,
                                              label.padding = label.padding,
                                              label.r = label.r,
                                              label.size = label.size)
                       }
)
