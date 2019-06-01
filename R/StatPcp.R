# StatPcp function

# Expected input:

# x: variable names
# y: corresponding values

# Expected output:

# for geom_segment, we need x, y, xend, yend
# for geom_ribbon, we need x, ymin, ymax

# question list:
# can I use dplyr inside functions? How?
# is is.numeric() safe?
# do I need to use like spread_ instead of spread?
# how to get the proper data that we can know if a variable is factor or numeric from

StatPcp <- ggproto("StatPcp", Stat,
                   required_aes = c("x", "y"),

                   # want to figure out the number of observations
                   # want to figure out the number of different classer of the variables
                   setup_params = function(data, params) {
                     # params$naxes <- unique(data$x)
                     # data$id <- rep(1:(nrow(data)/params$naxes), params$naxes)

                     # spread_data <- spread(data, key = x, value = y)
                     # classes <- vapply(spread_data, FUN = class, FUN.VALUE = character(1))

                     # assume we can keep the attribute and use it here. assume the classes are as follows
                     params$num <- attr(data, "classpcp") == "numeric"
                     params$fac <- attr(data, "classpcp") == "factor"
                     params$nnum <- sum(attr(data, "classpcp") == "numeric")
                     params$nfac <- sum(attr(data, "classpcp") == "factor")
                     params$nobs <- nrow(data)/length(attr(data, "classpcp"))
                     params$classpcp <- attr(data, "classpcp")
                     params
                   }


                   # want to calculate the parameters directly can be used for geom_segment and geom_ribbon
                   # and how to arrange them properly in the same time

                   # or we can put the attribute in the function prarameters?
                   compute_panel = function(data, scales, num, fac, nnum, nfac, nobs, classpcp) {

                     # several possible combinations: num to num, num to factor, factor to num, factor to factor
                     # need an algrothm to do this classification, write this function in a different place
                     # we use the function: classify here
                     classification <- classify(classpcp)

                     # for convenience of dealing with the data
                     data$id <- rep(1:nobs, length(classpcp))
                     # make sure the correct order of variables in columns after spread()
                     data$x <- factor(data$x, levels = unique(data$x))
                     data_spread <- spread(data, key = x, value = y)
                     data_spread[, c(FALSE, num)] <-  lapply(data_spread[, c(FALSE, num)],
                                                             FUN = function(x) as.numeric(as.character(x)))
                     # need to check weather the factor orders are kept correctly
                     # this need to work together with the preparation together to guarantee
                     data_spread[, c(FALSE, fac)] <-  lapply(data_spread[, c(FALSE, fac)],
                                                             FUN = droplevels)
                     # at this time, data_spread is like the original data set, with columns properly defined
                     # assume numeric variables are properly scaled into 0-1

                     # for all variables, we need only calculate one x-axies
                     data_final_x <- 1:length(classpcp)

                     # for num to num, set up
                     # set up ystart, yend.(sometimes we plus one to adjust for ID column)
                     # for ystart of lines

                     data_final_ystart_num2num <- unlist(as.list(spread_simpledata[, classification$num2num + 1]))
                     # for yend of lines
                     data_final_yend_num2num <- unlist(as.list(spread_simpledata[, classification$num2num + 2]))
                     # for xstart of lines
                     data_final_xstart_num2num <- rep(classification$num2num, each = nobs)
                     # for xend of lines
                     data_final_xend_num2num <- rep(classification$num2num + 1, each = nobs)


                     # for num to factor, set up
                     # Here I want to treat the factor(categorical) variable as bands according to its levels,
                     # so I'm not going to treat it as several points. the end points uniformly distributed within each band
                     # I also want to order those end points landing on the bands somehow
                     # I will add bands to indicate the different levels of a categorical variables later(like a big error bar?)
                     # for ystart of lines(same as num2num)
                     data_final_ystart_num2fac <- unlist(as.list(spread_simpledata[, classification$num2fac + 1]))
                     # for yend of lines
                     # first calculete the number of levels and number of observations landing in each levels



                   }
)

# used to identify the type of neighboring classes, return the position of the first one in a pair(like num-num, num-fac)
# for num to num, we use lines
# for num to factor, we use lines
# for factor to num, we use lines
# for factor to factor, we use ribbons
classify <- function(classpcp) {
  classpcp <- as.numeric(classpcp == "numeric")
  classpcp_diff <- diff(classpcp)
  num2fac <- (1:(length(classpcp)-1))[classpcp_diff == -1]
  fac2num <- (1:(length(classpcp)-1))[classpcp_diff == 1]

  num2num <- (1:(length(classpcp)-1))[classpcp_diff == 0 & classpcp[-length(classpcp)] == TRUE]
  fac2fac <- (1:(length(classpcp)-1))[classpcp_diff == 0 & classpcp[-length(classpcp)] == FALSE]

  classification <-  list(num2num = num2num,
                          num2fac = num2fac,
                          fac2num = fac2num,
                          fac2fac = fac2fac)
}
