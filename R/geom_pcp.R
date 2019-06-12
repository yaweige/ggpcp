# Geom function of geom_pcp
geom_pcp <- function(mapping = NULL, data = NuLL,
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
                   # default_aes = aes()
                   # required_aes = c()
                   draw_panel = function(data, panel_params, coord,
                                         arrow = NULL,
                                         arrow.fill = NULL,
                                         lineend = "butt",
                                         linejoin = "round",
                                         na.rm = na.rm) {
                     pcp_segment <- data.frame(
                       x = data$data_final_xstart,
                       xend = data$data_final_xend,
                       y = data$data_final_ystart,
                       yend = data$data_final_yend,
                       # And what about other parameters?
                       colour = data$colour,
                       size = data$size,
                       linetype = data$linetype,
                       fill = alpha(data$fill, data$alpha),
                       # is there PANEL or group? How those work...
                       PANEL = data$PANEL,
                       group = data$group
                     )

                     # what is the data goes to GeomSegment$draw_panel looks like, what about other parameters
                     GeomSegment$draw_panel(pcp_segment, panel_params, coord,
                                            arrow = arrow,
                                            arrow.fill = arrow.fill,
                                            lineend = lineend,
                                            linejoin = "round",
                                            na.rm = na.rm)
                   }
)

# study the above questions: data looks like, where should the parameters goes
# by browser() into geom_segment

# how to create the function for data process, finish the basic process
