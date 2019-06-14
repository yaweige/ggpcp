# boxwidth adjustment and box: method one================
# for method 1: boxwidth=================

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


# for method 1: segment=================
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


# for method 1: box===============================
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













# for method 2: boxwidth=================
data_stat <- final_data #from test 1, simpledata3
data_boxwidth <- data_stat
# how to do the adjustment
boxwidth <- 0.1
rugwidth <- 0.05
# detect the factor
classpcp <- c("factor", "factor", "factor", "numeric", "factor",
              "factor", "numeric", "factor", "numeric")
# fac, as defined previously
fac <- classpcp == "factor"

# for xstart, for xend
fac_count <- cumsum(fac)
num_count <- cumsum(!fac)

boxwidth_xend <-  seq_along(classpcp) + fac_count*2*boxwidth + num_count*2*rugwidth

boxwidth_xstart <- c(1, (boxwidth_xend + 1)[-length(classpcp)])

data_boxwidth$data_final_xstart <- boxwidth_xend[data_boxwidth$data_final_xstart]
data_boxwidth$data_final_xend <- boxwidth_xstart[data_boxwidth$data_final_xend]

ggplot(data_boxwidth) +
  geom_segment(aes(x = data_final_xstart, y = data_final_ystart,
                   xend = data_final_xend, yend = data_final_yend))



# for method 2: segment=================
data_segment <- data_stat

data_segment_xstart <- boxwidth_xstart[data_segment$data_final_xstart]
data_segment_xend <- boxwidth_xend[data_segment$data_final_xstart]
data_segment_ystart <- data_segment$data_final_ystart
data_segment_yend <- data_segment_ystart

# for the last variable seperately
data_segment_xstart <- c(data_segment_xstart, boxwidth_xstart[rep(length(classpcp), nobs)])
data_segment_xend <- c(data_segment_xend, boxwidth_xend[rep(length(classpcp), nobs)])
data_segment_ystart <- c(data_segment_ystart,
                         data_segment[which(data_segment$data_final_xend == length(classpcp)), "data_final_yend"])
data_segment_yend <- data_segment_ystart

# combine those into one data set
data_segment <- data.frame(data_segment_xend = data_segment_xend,
                           data_segment_yend = data_segment_yend,
                           data_segment_xstart = data_segment_xstart,
                           data_segment_ystart = data_segment_ystart)

ggplot(data_boxwidth) +
  geom_segment(aes(x = data_final_xstart, y = data_final_ystart,
                   xend = data_final_xend, yend = data_final_yend)) +
  geom_segment(data = data_segment, aes(x = data_segment_xstart, y = data_segment_ystart,
                                        xend = data_segment_xend, yend = data_segment_yend))


# for method 2: box=================
# some values like boxwidth_xstart, boxwidth_xend coming from boxwidth calculation
# some values like fac = classpcp == "factor"
data_box_y <- rep(unlist(level_range), each = 2)

data_box_x <- Map(f = function(x, y, z) {
  rep(c(y, z, z, y), times = x$nlevels)
},
nlevels_list,
boxwidth_xstart[fac],
boxwidth_xend[fac])

data_box_x <- unlist(data_box_x)
data_box_group <- rep(1:(length(data_box_y)/4), each = 4)

data_box <- data.frame(data_box_x = data_box_x,
                       data_box_y = data_box_y,
                       data_box_group = data_box_group)
