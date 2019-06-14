# StatPcp or StatPcp_box?

# Expected output: x, y, group for geom_polygon
# Expected output: xmin, ymin, xmax, ymax for geom_crossbar, then follow the same way in GeomCrossbar
StatPcp_box <- ggproto("StatPcp_box", Stat,
                       compute_panel = function() {

                         ### continue calculation from StatPcp
                         #data_box <- data_stat

                         ### The following lines from StatPcp will be helpful for finding boxes places

                         # the calculation before this, can be found in StatPcp, or draft3forbox second example
                         eachobs <- 1/nobs
                         boxwidth <- 0.1
                         #fac # coming from the classpcp == "factor"
                         nlevels_list <- lapply(data_spread[, c(FALSE, fac), drop = FALSE],
                                                FUN = function(x) list(nlevels = nlevels(x),
                                                                       table = table(x)))
                         level_range <- lapply(nlevels_list,
                                               FUN = function(x) c(0, rep(cumsum(eachobs*x$table)[-x$nlevels], each = 2) +
                                                                     freespace/(x$nlevels-1)/2*rep(c(-1, 1), times = x$nlevels-1), 1))



                         # The following calculation is for boxwidth method one

                         data_box_y <- rep(unlist(level_range), each = 2)

                         data_box_x <- Map(f = function(x, y) {
                           rep(c(y - boxwidth, y + boxwidth,
                                 y + boxwidth, y - boxwidth), times = x$nlevels)
                         },
                         nlevels_list,
                         which(fac))

                         data_box_x <- unlist(data_box_x)
                         data_box_group <- rep(1:(length(data_box_y)/4), each = 4)

                         data_box <- data.frame(data_box_x = data_box_x,
                                                data_box_y = data_box_y,
                                                data_box_group = data_box_group)


                       }
)







# more adjustment on the boxwidth================================================================
# Accept a parameter to adjust the width of box
# accept a parameter to adjust the width of rugs

data_stat <- final_data #from test 1, simpledata3

# how to do the adjustment

# method 1. only adjust the facotr variable itself without adjusting other parts at the same time
# method 2. after adjusting the boxwidth, the distance between the variables are still the same(variable to box edge)

boxwidth <- 0.1

# method 1. (be careful about the first and last variable)

# detect the factor
classpcp <- c("factor", "factor", "factor", "numeric", "factor",
              "factor", "numeric", "factor", "numeric")

# fac, as defined previously
fac <- classpcp == "factor"
fac

data_boxwidth <- data_stat
data_boxwidth$data_final_xstart <- ifelse(data_boxwidth$data_final_xstart %in% which(fac),
                                          data_boxwidth$data_final_xstart + boxwidth,
                                          data_boxwidth$data_final_xstart)
data_boxwidth$data_final_xend <- ifelse(data_boxwidth$data_final_xend %in% which(fac),
                                        data_boxwidth$data_final_xend - boxwidth,
                                        data_boxwidth$data_final_xend)

ggplot(data_boxwidth) +
  geom_segment(aes(x = data_final_xstart, y = data_final_ystart,
                   xend = data_final_xend, yend = data_final_yend))

# method 1 finished=========================





# method 2===============================
# way1 先全部压扁到起点， 然后再累积的调整
# way2 先统一按照fac调整，再累积的调整
# way3 直接在data_final_xstart_fac2fac的层面调整
# way4 先将中心直接调整到最后的位置，再start和end
# way5 本质上是在调整一个向量1:7，所以可以先调整这个向量




# add the parallel segment for factors============

# for method 1
data_stat

which_fac_mid <- which(fac)
# the following can deal with the first variable
if (which_fac_mid[length(which_fac_mid)] == length(classpcp)) {
  which_fac_mid <- which_fac_mid[-length(which_fac_mid)]
}


data_segment <- data_stat[which(data_stat$data_final_xstart %in% which_fac_mid),]

data_segment_xstart <- data_segment$data_final_xstart - boxwidth
data_segment_xend <- data_segment$data_final_xstart + boxwidth
data_segment_ystart <- data_segment$data_final_ystart
data_segment_yend <- data_segment_ystart

# for the last variable
which_fac_mid <- which(fac)
if (which_fac_mid[length(which_fac_mid)] == length(classpcp)) {
  data_segment <- data_stat[which(data_stat$data_final_xend == length(classpcp)),]
  data_segment_xstart <- c(data_segment_xstart, data_segment$data_segment_xend - boxwidth)
  data_segment_xend <- c(data_segment_xend, data_segment$data_segment_xend + boxwidth)
  data_segment_ystart <- c(data_segment_ystart, data_segment$data_segment_yend)
  data_segment_yend <- c(data_segment_yend, data_segment_ystart)

}

data_segment <- data.frame(data_segment_xend = data_segment_xend,
                           data_segment_yend = data_segment_yend,
                           data_segment_xstart = data_segment_xstart,
                           data_segment_ystart = data_segment_ystart)

ggplot(data_boxwidth) +
  geom_segment(aes(x = data_final_xstart, y = data_final_ystart,
                   xend = data_final_xend, yend = data_final_yend)) +
  geom_segment(data = data_segment, aes(x = data_segment_xstart, y = data_segment_ystart,
                                        xend = data_segment_xend, yend = data_segment_yend))


# for data_segment_xend, the last variable need to be dealt with seperately
# for data_segment_xstart, the first variable need to be dealt with seperately

# finished segment inside factors================================
