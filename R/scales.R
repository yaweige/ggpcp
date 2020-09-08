
#' Helper function to create scales for the parallel coordinate plot
#' @noRd
xscale_pcp <- function(data, params, layout, ...) {
  # p <- sum(data$id__==1)
  # # adjust breaks of x axis
  # type <- data$class[data$id__==1]
  # breaks <- 1:p +
  #   cumsum(params$boxwidth*(type=="factor")) -
  #   params$boxwidth/2*(type=="factor")

  # if (is.null(params$reverse)) params$reverse <- FALSE
  boxwidth <- params$boxwidth
  rugwidth <- params$rugwidth
  interwidth <- params$interwidth
  nobs <- length(unique(data$id__))
  classpcp <- data$class[1 - nobs + (1:(nrow(data)/nobs))*nobs]
  # fac <- classpcp %in% c("factor", "ordered factor")
  #
  # if (length(interwidth) == 1) {
  #   interwidth <- rep(interwidth, times = length(classpcp) - 1)
  # }
  # interwidth <- cumsum(c(1, interwidth))
  #
  # if (length(boxwidth) == 1) {
  #   boxwidth <- rep(boxwidth, times = sum(fac))
  # }
  # if (length(rugwidth) == 1) {
  #   rugwidth <- rep(rugwidth, times = sum(!fac))
  # }
  #
  # boxrugwidth <- seq_along(classpcp)
  # boxrugwidth[fac] <- boxwidth
  # boxrugwidth[!fac] <- rugwidth
  #
  # cumboxrugwidth <- cumsum(boxrugwidth)
  #
  #
  # boxwidth_xend <-  interwidth + cumboxrugwidth
  # boxwidth_xstart <- boxwidth_xend - boxrugwidth
  #
  # breaks <- boxwidth_xend - boxrugwidth/2

  width_adjusted <- prepare_width_ajustment(classpcp, boxwidth, rugwidth, interwidth, reverse = params$reverse)
  boxwidth_xend <-  width_adjusted$boxwidth_xend
  boxwidth_xstart <- width_adjusted$boxwidth_xstart
  breaks <- width_adjusted$breaks
  # scales$x <- scale_x_continuous(limits = c(1,p + params$boxwidth*sum(type=="factor")), breaks = breaks, labels = data$name[data$id__==1])
  if (params$reverse == TRUE) {
    scale <- scale_x_continuous(limits = c(min(boxwidth_xend), max(boxwidth_xstart)), breaks = breaks, labels = unique(data$name), ...)
  } else {
    scale <- scale_x_continuous(limits = c(min(boxwidth_xstart), max(boxwidth_xend)), breaks = breaks, labels = unique(data$name), ...)
  }
  scale$get_breaks <- function(limits) breaks
  scale
}
