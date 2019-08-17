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
#' @param breakpoint To break three or more factors into peices
#' @param merge To merge the bands or not
#'
#' @import ggplot2
#' @importFrom dplyr %>% group_by ungroup arrange select mutate summarise
#' @importFrom tidyr spread
#' @export stat_pcp_band


stat_pcp_band <- function(mapping = NULL, data = NULL,
                          geom = "ribbon", position = "identity",
                          ...,
                          freespace = 0.1,
                          boxwidth = 0,
                          rugwidth = 0,
                          interwidth = 1,
                          breakpoint = NULL,
                          merge = FALSE,
                          na.rm = FALSE,
                          show.legend = NA,
                          inherit.aes = TRUE) {
  layer(
    data = data,
    mapping = mapping,
    stat = StatPcpband,
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
      breakpoint = breakpoint,
      merge = merge,
      ...
    )
  )
}


StatPcpband <- ggproto(
  "StatPcpband",
  Stat,
  default_aes = ggplot2::aes(
    id = id, name = name, value = value, level = level, class = class,
    width = 0.75, linetype = "solid", fontsize=5,
    shape = 19, colour = "grey30",
    size = .1, fill = NA, alpha = .8, stroke = 0.1,
    linewidth=.1, weight = 1),

  setup_data = function (data, params) {
    #   browser()
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
  # continue coding following StatPcp

  # find the start and end position for every combination inside each factor
  # and label properly for the latter use: group
  # We'd better to have a new way to assign bands without considering observations, bandid acrossing the factor block

  compute_panel = function(data, scales, freespace = 0.1, boxwidth = 0,
                           rugwidth = 0 , interwidth = 1,
                           breakpoint = NULL, merge = FALSE) {


    # Data preparation: to convert the input data to the form we can directly use

    # number of observations
    obs_ids <- unique(data$id)
    nobs <- length(unique(data$id))
    # a vector to tell the class of variables
    classpcp <- data$class[1 - nobs + (1:(nrow(data)/nobs))*nobs]
    data_spread <- prepare_data(data, classpcp, nobs)


    # at this time, data_spread is like the original data set, with columns properly defined
    # assume numeric variables are properly scaled into 0-1
    if (is.character(breakpoint)) {
      breakpoint <- which(names(data_spread) %in% breakpoint) - 1
    }

    classification <- classify(classpcp, breakpoint = breakpoint)

    # for factor to factor, set up

    if (!length(classification$fac2fac) == 0) {
      # some values needed
      # to find the factor block(more than one factor together)
      # produce continuous_fac for each factor_block
      # 0622new: accomodate to new classification method, continuous_fac_all_raw
      continuous_fac_all_raw <- as.vector(rbind(classification$fac2fac, classification$fac2fac + 1))
      continuous_fac_all <- continuous_fac_all_raw[c(which(diff(continuous_fac_all_raw) != 0 & diff(continuous_fac_all_raw) != -1 ),
                                                     length(continuous_fac_all_raw))]
      break_position <- c(0, which(diff(continuous_fac_all) != 1), length(continuous_fac_all))
      continuous_fac_all_list <- lapply(1:(length(break_position) - 1), FUN = function(x) {
        continuous_fac_all[(break_position[x] + 1):break_position[x + 1]]
      })
      # detect if there is a numeric variable prior to the factor block, after the factor block
      start_fac2fac <- continuous_fac_all[break_position[-length(break_position)] + 1]
      end_fac2fac <- continuous_fac_all[break_position[-1]]
      # to identify which columns should be used to sort factor blocks
      bywhich <- prepare_bywhich(start_fac2fac, classpcp)

      if (is.na(bywhich[1])) {
        start_position <- as.data.frame(matrix(rep(1:nobs, length(bywhich)), ncol = length(bywhich)))
      } else {
        start_position <- data_spread[,bywhich + 1,drop = FALSE]
      }

      # use Map to apply the function to every factor_block
      arranged_fac_block <- Map(f = function(x, y) {
        process_fac2fac(data_spread = data_spread,
                        continuous_fac = x,
                        start_position = y,
                        freespace = freespace,
                        nobs = nobs)},
        continuous_fac_all_list,
        as.data.frame(start_position))

      # organize the output correctly into one
      data_final_xstart_fac2fac <- unlist(lapply(arranged_fac_block,
                                                 FUN = function(x) x[[1]]$data_final_xstart_fac2fac))
      data_final_ystart_fac2fac <- unlist(lapply(arranged_fac_block,
                                                 FUN = function(x) x[[1]]$data_final_ystart_fac2fac))
      data_final_xend_fac2fac <- unlist(lapply(arranged_fac_block,
                                               FUN = function(x) x[[1]]$data_final_xend_fac2fac))
      data_final_yend_fac2fac <- unlist(lapply(arranged_fac_block,
                                               FUN = function(x) x[[1]]$data_final_yend_fac2fac))

      # also extract the bandid information
      data_final_ystart_fac2fac_bandid <- unlist(lapply(arranged_fac_block,
                                                        FUN = function(x) x[[2]]$data_final_ystart_fac2fac_bandid))
      data_final_yend_fac2fac_bandid <- unlist(lapply(arranged_fac_block,
                                                      FUN = function(x) x[[2]]$data_final_yend_fac2fac_bandid))


      # This part is new for bands

      # a better way of calculating bands may be applying the calculation during the calculation of lines for breakpoint,
      # but that will still need some ideas from here. In generally, we could think breaking at any possible position,
      # and decide the bandid to pass to next sub-factor block according to if we really want to break

      # We need a way to detect if the bands should be merged
      # We also need a way to index group in the final output
      data_band_raw <- data.frame(data_final_xstart_fac2fac = data_final_xstart_fac2fac,
                                  data_final_ystart_fac2fac = data_final_ystart_fac2fac,
                                  data_final_xend_fac2fac = data_final_xend_fac2fac,
                                  data_final_yend_fac2fac = data_final_yend_fac2fac,
                                  data_final_ystart_fac2fac_bandid = data_final_ystart_fac2fac_bandid,
                                  data_final_yend_fac2fac_bandid = data_final_yend_fac2fac_bandid,
                                  id = rep(obs_ids, times = length(data_final_xstart_fac2fac)/nobs))

      data_band_raw_split <- split(data_band_raw, f = rep(1:(nrow(data_band_raw)/nobs), each = nobs))

      # there may be problem when there is only one or two observations fot each level or each factor variable

      # a value to be compared with: eachobs
      eachobs <- (1 - freespace)/nobs
      data_band_list <- lapply(data_band_raw_split, FUN = function(x) {
        # unmerged data for bands
        data_band_unmerged <- x %>%
          group_by(data_final_ystart_fac2fac_bandid) %>%
          summarise(band_ystart_min = min(data_final_ystart_fac2fac),
                    band_ystart_max = max(data_final_ystart_fac2fac),
                    band_yend_min = min(data_final_yend_fac2fac),
                    band_yend_max = max(data_final_yend_fac2fac),
                    band_xstart = data_final_xstart_fac2fac[1],
                    band_xend = data_final_xend_fac2fac[1],
                    bandid = data_final_ystart_fac2fac_bandid[1],
                    id = min(id)) %>%
          ungroup() %>%
          select(-data_final_ystart_fac2fac_bandid)
        # merge the bands when necessary, we will need a function to do this

        if (merge) {
          data_band_merged <- band_merge(data_band_unmerged, eachobs)
        } else {
          data_band_merged <- as.data.frame(data_band_unmerged)
        }
        data_band_merged

      })

      data_band_final_list <- lapply(c(1:6, 8), FUN = function(x) {
        temp <- vector()
        for(i in 1:length(data_band_list)) {
          temp <- c(temp, data_band_list[[i]][, x])
        }
        temp
      })

      names(data_band_final_list) <- c("band_ystart_min",
                                       "band_ystart_max",
                                       "band_yend_min",
                                       "band_yend_max",
                                       "band_xstart",
                                       "band_xend",
                                       "id")

      data_band_final_wide <- as.data.frame(data_band_final_list)

    } else {
      stop("No factor variable, no band can be drawn")
    }


    # interval length, boxwidth, rugwidth ajustment preparation
    width_adjusted <- prepare_width_ajustment(classpcp, boxwidth, rugwidth, interwidth)

    # to modify and convert to long form
    data_band_final_wide[ ,5] <- width_adjusted$boxwidth_xend[data_band_final_wide[ ,5]]
    data_band_final_wide[ ,6] <- width_adjusted$boxwidth_xstart[data_band_final_wide[ ,6]]
    data_band_final_long <- data.frame(x = c(data_band_final_wide[ ,5], data_band_final_wide[ ,6]),
                                       ymin = c(data_band_final_wide[ ,1], data_band_final_wide[ ,3]) - 0.5*eachobs,
                                       ymax = c(data_band_final_wide[ ,2], data_band_final_wide[ ,4]) + 0.5*eachobs,
                                       id = c(data_band_final_wide[ ,7], data_band_final_wide[ ,7]))
    data_band_final_long$group <- rep(1:nrow(data_band_final_wide), times = 2)

    # To include the original data information for potential color or other mapping
    datanames <- setdiff(names(data),c("name", "value", "level", "class", "group", "x", "y", "ymin", "ymax"))
    # is there any problem when the original data has column x, y ymin, ymax, group, id?
    # don't include the pcp specific variables - those are dealt with
    output_data <- left_join(data_band_final_long, unique(data[,datanames]), by = "id")
    if (merge) {
      if (any(names(output_data) %in% c("color", "fill"))) {
        message("When merge = TRUE, the additional aesthesics (color, fill...) is likely not one to one corresponding")
      }
    }
    output_data

  }


)

# A function used to merge smaller bands into a larger one if they are actually merged in the plot
# we still need to deal with a extreme case when freespace = 0, for certain data
band_merge <- function(data_band_unmerged, eachobs) {
  # for R CMD check
  id <- NULL
  if(nrow(data_band_unmerged) >= 2) {
    data_band_unmerged <- data_band_unmerged %>%
      arrange(.data$band_ystart_min) %>%
      mutate(ystart_distance = .data$band_ystart_min - c(0, .data$band_ystart_max[-length(.data$band_ystart_max)]),
             yend_distance = .data$band_yend_min - c(2, .data$band_yend_max[-length(.data$band_yend_max)])) %>%
      mutate(to_merge = (.data$ystart_distance <= 1.001 * eachobs)&(.data$yend_distance <= 1.001 * eachobs & .data$yend_distance > 0))
    # there might be rounding error, so not strictly equal
    # in which case, freespace = 0, might cause problem? Will two bands from two levels are merged
    merge_which <- which(data_band_unmerged$to_merge)
    merge_break <- c(0, which(diff(merge_which) != 1), length(merge_which))
    merge_list <- lapply(1:(length(merge_break) - 1), FUN = function(x) {
      temp <- merge_which[(merge_break[x] + 1):merge_break[x + 1]]
      # to get the value before the sequence
      output <- unique(as.vector(rbind(temp-1, temp)))
    })
    merged_band_only <- lapply(merge_list, FUN = function(x) {
      selected_data <- data_band_unmerged[x, ]
      data.frame(band_ystart_min = selected_data$band_ystart_min[1],
                 band_ystart_max = selected_data$band_ystart_max[length(x)],
                 band_yend_min = selected_data$band_yend_min[1],
                 band_yend_max = selected_data$band_yend_max[length(x)],
                 band_xstart = selected_data$band_xstart[1],
                 band_xend = selected_data$band_xend[1],
                 bandid = min(selected_data$bandid),
                 id = min(selected_data$id))
    })
    data_merged_band_only <- lapply(1:8, FUN = function(x) {
      temp <- vector()
      for (i in 1:length(merged_band_only)) {
        temp <- c(temp, merged_band_only[[i]][, x])
      }
      temp
    })
    data_band_unmerged_only <- as.data.frame(data_band_unmerged[setdiff(1:nrow(data_band_unmerged), unlist(merge_list)), ])
    data_band_merged <- data.frame(band_ystart_min = c(data_merged_band_only[[1]], data_band_unmerged_only[, 1]),
                                   band_ystart_max = c(data_merged_band_only[[2]], data_band_unmerged_only[, 2]),
                                   band_yend_min = c(data_merged_band_only[[3]], data_band_unmerged_only[, 3]),
                                   band_yend_max = c(data_merged_band_only[[4]], data_band_unmerged_only[, 4]),
                                   band_xstart = c(data_merged_band_only[[5]], data_band_unmerged_only[, 5]),
                                   band_xend = c(data_merged_band_only[[6]], data_band_unmerged_only[, 6]),
                                   bandid = c(data_merged_band_only[[7]], data_band_unmerged_only[, 7]),
                                   id = c(data_merged_band_only[[8]], data_band_unmerged_only[, 8]))

    data_band_merged <- data_band_merged[!is.na(data_band_merged[, 1]), ]
  } else {
    data_band_merged <- as.data.frame(data_band_unmerged)
  }
  data_band_merged
}

