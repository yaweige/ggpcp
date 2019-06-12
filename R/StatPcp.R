# StatPcp function

# Expected input:

# x: variable names
# y: corresponding values

# Expected output:

# for geom_segment, we need x(xstart), y(ystart), xend, yend
# for geom_ribbon, we need x, ymin, ymax

# question list:
# can I use dplyr inside functions? How?
# is is.numeric() safe?
# do I need to use like spread_ instead of spread?
# how to get the proper data that we can know if a variable is factor or numeric from

# for stat_pcp?

# for statPcp
StatPcp <- ggproto("StatPcp", Stat,
                   required_aes = c("x", "y"),

                   # want to figure out the number of observations
                   # want to figure out the number of different classer of the variables
                   ### setup_params accept params from stat_pcp or geom_pcp?
                   # setup_params = function(data, params) {
                   #   # assume we can keep the attribute and use it here. assume the classes are as follows
                   #   params$num <- attr(data, "classpcp") == "numeric"
                   #   params$fac <- attr(data, "classpcp") == "factor"
                   #   params$nobs <- nrow(data)/length(attr(data, "classpcp"))
                   #   params$classpcp <- attr(data, "classpcp")
                   #   params
                   # }
                   setup_params = function(data, params) {
                     params$freespace <- ifelse(is.null(freespace), 0.1, freespace)

                   }


                   # want to calculate the parameters directly can be used for geom_segment and geom_ribbon
                   # and how to arrange them properly in the same time

                   # or we can put the attribute in the function prarameters?
                   compute_panel = function(data, scales, freespace = 0.1) {

                     # make adjustment to accept proper data set
                     # make sure the output data_spread has the same correct expected column order
                     data$name <- factor(data$name, levels = unique(data$name))
                     data_spread <- spread(data[, c("id", "name", "value")], key = name, value = value)

                     nobs <- max(data$id)
                     ncol <- nrow(data)/nobs
                     nvar <- length(levels(data$name))
                     # ncol should be the same as nvar

                     # this may not work with tibble
                     classpcp <- data$class[1 - nobs + (1:ncol)*nobs]
                     num <- classpcp == "numeric"
                     fac <- classpcp == "factor"

                     data_spread[, c(FALSE, num)] <-  lapply(data_spread[, c(FALSE, num)],
                                                             FUN = function(x) as.numeric(as.character(x)))

                     # to deal with factors, assign proper levels
                     # same name, value may break down this,
                     # if the user choose to put the same variable into the data twive
                     original_levels <- unique(gather_data[which(gather_data$class == "factor"),c("name", "value", "level")])
                     original_levels$name <- droplevels(original_levels$name)
                     original_levels <- original_levels %>%
                       group_by(name) %>%
                       arrange(level, .by_group = TRUE) %>%
                       ungroup()

                     original_levels <- split(original_levels, f = original_levels$name)

                     data_spread[, c(FALSE, fac)] <- Map(f = function(x, y){
                       factor(x, levels = y$value)
                     },
                     data_spread[, c(FALSE, fac)],
                     original_levels)

                     # at this time, data_spread is like the original data set, with columns properly defined
                     # assume numeric variables are properly scaled into 0-1

                     # several possible combinations: num to num, num to factor, factor to num, factor to factor
                     # need an algrothm to do this classification, write this function in a different place
                     # we use the function: classify here
                     classification <- classify(classpcp)

                     # for num to num, set up
                     if (!length(classification$num2num) == 0) {
                       # set up ystart, yend.(sometimes we plus one to adjust for ID column)
                       # for ystart of lines (seems we can use unlist to data.frame directly)
                       data_final_ystart_num2num <- unlist(data_spread[, classification$num2num + 1])
                       # for yend of lines
                       data_final_yend_num2num <- unlist(data_spread[, classification$num2num + 2])
                       # for xstart of lines
                       data_final_xstart_num2num <- rep(classification$num2num, each = nobs)
                       # for xend of lines
                       data_final_xend_num2num <- rep(classification$num2num + 1, each = nobs)
                     } else {
                       data_final_ystart_num2num <- NULL
                       data_final_yend_num2num <- NULL
                       data_final_xstart_num2num <- NULL
                       data_final_xend_num2num <- NULL
                     }



                     # for num to factor, set up
                     if (!length(classification$num2fac) == 0) {
                       # Here I want to treat the factor(categorical) variable as bands according to its levels,
                       # so I'm not going to treat it as several points. the end points uniformly distributed within each band
                       # I also want to order those end points landing on the bands somehow
                       # I will add bands to indicate the different levels of a categorical variables later(like a big error bar?)

                       # for ystart of lines(same as num2num, use unlist withour as.list first)
                       data_final_ystart_num2fac <- unlist(data_spread[, classification$num2fac + 1])

                       # for yend of lines
                       # first calculete the number of levels and number of observations landing in each level
                       nlevels_list <- lapply(data_spread[, classification$num2fac + 2],
                                              FUN = function(x) list(nlevels = nlevels(x),
                                                                     table = table(x)))
                       # uniformly assign space for each level and observations within each level
                       # inserted some space between every two levels here, called freespace for the space in total
                       # obs_position is the postion assigned for factors
                       obs_position <- assign_fac(nlevels_list, nobs, freespace = 0.1)

                       # for yend of lines, continued
                       # write another function to arrange the positions of the end
                       # points according to the ystart, and the order of data
                       # is it right to directly unlist? Yes it seems
                       data_final_yend_num2fac <- unlist(arrange_fac_by_ystart(data_spread,
                                                                               start_position = classification$num2fac + 1,
                                                                               end_position = classification$num2fac + 2,
                                                                               obs_position = obs_position))

                       # for xstart of lines
                       data_final_xstart_num2fac <- rep(classification$num2fac, each = nobs)
                       # for xend of lines
                       data_final_xend_num2fac <- rep(classification$num2fac + 1, each = nobs)
                     } else {
                       data_final_ystart_num2fac <- NULL
                       data_final_yend_num2fac <- NULL
                       data_final_xstart_num2fac <- NULL
                       data_final_xend_num2fac <- NULL
                     }



                     # for factor to num, set up (this should be similar to num2fac)
                     if (!length(classification$fac2num) == 0) {
                       # need to do some adjustments to make the functions above more general and can be used here

                       # for xstart of lines
                       data_final_xstart_fac2num <- rep(classification$fac2num, each = nobs)
                       # for xend of lines
                       data_final_xend_fac2num <- rep(classification$fac2num + 1, each = nobs)
                       # for yend of lines
                       data_final_yend_fac2num <- unlist(data_spread[, classification$fac2num + 2])
                       # for ystart of lines (mimic the calculation to num2fac, be careful about the difference)
                       nlevels_list_2 <- lapply(data_spread[, classification$fac2num + 1],
                                                FUN = function(x) list(nlevels = nlevels(x),
                                                                       table = table(x)))
                       obs_position_2 <- assign_fac(nlevels_list_2, nobs, freespace = 0.1)
                       # here arrange_fac_by_ystart, actually arranges fac (ystart) by yend
                       data_final_ystart_fac2num <- unlist(arrange_fac_by_ystart(data_spread,
                                                                                 start_position = classification$fac2num + 2,
                                                                                 end_position = classification$fac2num + 1,
                                                                                 obs_position = obs_position_2))

                     } else {
                       data_final_xstart_fac2num <- NULL
                       data_final_xend_fac2num <- NULL
                       data_final_yend_fac2num <- NULL
                       data_final_ystart_fac2num <- NULL
                     }

                     # we have to make sure those postions are consistent, which are on the same vertical axis, but shared by different pairs
                     # even if it is consistent(same), which I think is very likely ensured by our consitent method of dealing with variables
                     # we can still make some improvement above, to save some calculation to avoid twice calculation of same objecets
                     # the only variables, we need to care are factor variables.

                     # for factor to factor, set up
                     # this repeated the efforts of ggparallel in a sense

                     # make use of the function to calculate level_range inside assign_fac(),
                     # and nlevel_lists as before when dealing with factors

                     # write a function for this part to assign and match the factors,
                     # we may first calculate a table of the possible combinations between every two factors, and then assign position
                     # with a constant freespace = 0.1, we make sure the lenghts of area taken are the same among factors

                     # for factor to factor block, segment(not line!)
                     # here is a little different from previous ones, we draw arrange same group together, not by variables

                     ### This may work for only one factor block, need more preparation for more than one block

                     if (!length(classification$fac2fac) == 0) {
                       # some values needed
                       # to find the factor block(more than one factor together)
                       # produce continuous_fac for each factor_block
                       continuous_fac_all <- unique(as.vector(rbind(classification$fac2fac, classification$fac2fac + 1)))
                       break_position <- c(0, which(diff(continuous_fac_all) != 1), length(continuous_fac_all))
                       continuous_fac_all_list <- lapply(1:(length(break_position) - 1), FUN = function(x) {
                         continuous_fac_all[(break_position[x] + 1):break_position[x + 1]]
                       })
                       # detect if there is a numeric variable prior to the factor block, after the factor block
                       start_fac2fac <- continuous_fac_all[break_position[-length(break_position)] + 1]
                       end_fac2fac <- continuous_fac_all[break_position[-1]]
                       bywhich <- start_fac2fac - 1

                       if (start_fac2fac[1] == 1) {
                         bywhich[1] <- end_fac2fac[1] + 1
                       }

                       prior_num <- classpcp[start_fac2fac[-1] - 1] == "numeric"

                       # detect if there is only one factor block or multiple
                       if(length(prior_num) != 0){
                         for (i in 1:(length(bywhich) - 1)) {
                           if (!prior_num[i]) {
                             bywhich[i+1] <- end_fac2fac[i+1]  + 1
                           }
                         }
                       }

                       # detect if all the variables are factors
                       if((end_fac2fac[1] + 1) > length(classpcp)) bywhich <- NULL

                       if (is.null(bywhich)) {
                         start_position <- 1:nobs
                       } else {
                         start_position <- data_spread[bywhich + 1]
                       }

                       # use Map to apply the function to every factor_block
                       arranged_fac_block <- Map(f = function(x, y) {
                         process_fac2fac(data_spread = data_spread,
                                         continuous_fac = x,
                                         start_position = y,
                                         freespace = freespace,
                                         nobs = nobs)},
                         continuous_fac_all_list,
                         start_position)

                       # organize the output correctly into one
                       data_final_xstart_fac2fac <- unlist(lapply(arranged_fac_block,
                                                                  FUN = function(x) x$data_final_xstart_fac2fac))
                       data_final_ystart_fac2fac <- unlist(lapply(arranged_fac_block,
                                                                  FUN = function(x) x$data_final_ystart_fac2fac))
                       data_final_xend_fac2fac <- unlist(lapply(arranged_fac_block,
                                                                FUN = function(x) x$data_final_xend_fac2fac))
                       data_final_yend_fac2fac <- unlist(lapply(arranged_fac_block,
                                                                FUN = function(x) x$data_final_yend_fac2fac))
                     } else {
                       data_final_xstart_fac2fac <- NULL
                       data_final_ystart_fac2fac <- NULL
                       data_final_xend_fac2fac <- NULL
                       data_final_yend_fac2fac <- NULL
                     }


                     # to do list in the following:
                     # 1. small modification, move data_spread$bandid to the beginning part, after all done


                     # some modification for num2fac_blcok (num2fac, fac is a factor block, more than one factor)

                     # detect those variables
                     # consider zero or multiple factor blocks
                     num2fac_block_relative <- which((classification$num2fac + 1) %in% classification$fac2fac)
                     num2fac_block_fac_relative <- which((classification$fac2fac -1) %in% classification$num2fac)
                     num2fac_change <- unlist(lapply(num2fac_block_relative, FUN = function(x) {
                       ((x - 1)*nobs + 1):(x*nobs)
                     }))
                     num2fac_fac_input <- unlist(lapply(num2fac_block_fac_relative, FUN = function(x) {
                       ((x - 1)*nobs + 1):(x*nobs)
                     }))

                     # make modification
                     data_final_yend_num2fac[num2fac_change] <- data_final_ystart_fac2fac[num2fac_fac_input]



                     # some modification for fac2num_blcok (fac2num, fac is a factor block, more than one factor)

                     # for multiple factor blocks, and multiple places to modify
                     # detect those variables
                     # consider zero or multiple factor blocks
                     fac2num_block_relative <- which(classification$fac2num %in% (classification$fac2fac + 1))
                     fac2num_change <- unlist(lapply(fac2num_block_relative, FUN = function(x) {
                       ((x - 1)*nobs + 1):(x*nobs)
                     }))
                     fac2num_block_fac_relative <- which((classification$fac2fac + 1) %in% classification$fac2num)
                     fac2num_fac_input <- unlist(lapply(fac2num_block_fac_relative, FUN = function(x) {
                       ((x - 1)*nobs + 1):(x*nobs)
                     }))

                     # make modification
                     data_final_ystart_fac2num[fac2num_change] <- data_final_yend_fac2fac[fac2num_fac_input]


                     # some other modifaction for fac2num (for num-fac-num case)
                     # we don't need to adjust fac2num for num-fac-num case, since it is adjusted by num2fac, keep consistent
                     # so we need to modify it back
                     # detect those variables
                     fac2num_numfacnum <- classification$fac2num[classification$fac2num %in% (classification$num2fac + 1)]
                     fac2num_numfacnum_relative <- which(classification$fac2num == fac2num_numfacnum)
                     num2fac_numfacnum_relative <- which(classification$num2fac == (fac2num_numfacnum - 1))
                     # for ystart of lines, keep it same as the num2fac result; this is the only thing that should be correct
                     data_final_ystart_fac2num[((fac2num_numfacnum_relative - 1)*nobs + 1):(fac2num_numfacnum_relative*nobs)] <-
                       data_final_yend_num2fac[((num2fac_numfacnum_relative - 1)*nobs + 1):(num2fac_numfacnum_relative*nobs)]



                     # put everything together

                     data_final_xstart <- c(data_final_xstart_num2num,
                                            data_final_xstart_num2fac,
                                            data_final_xstart_fac2num,
                                            data_final_xstart_fac2fac)
                     data_final_xend <- c(data_final_xend_num2num,
                                          data_final_xend_num2fac,
                                          data_final_xend_fac2num,
                                          data_final_xend_fac2fac)
                     data_final_ystart <- c(data_final_ystart_num2num,
                                            data_final_ystart_num2fac,
                                            data_final_ystart_fac2num,
                                            data_final_ystart_fac2fac)
                     data_final_yend <- c(data_final_yend_num2num,
                                          data_final_yend_num2fac,
                                          data_final_yend_fac2num,
                                          data_final_yend_fac2fac)
                     data_final <- data.frame(data_final_xstart = data_final_xstart,
                                              data_final_xend = data_final_xend,
                                              data_final_ystart = data_final_ystart,
                                              data_final_yend = data_final_yend)

                     # This has different length from the original data coming to compute_panel

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

# used to assign postion for the factors
# here is a long calculation formula
# defined another function inside this function, make sure they are correctly nested
assign_fac <- function(nlevels_list, nobs, freespace = 0.1) {
  eachobs <- 1/nobs
  # assign each level
  level_range <- lapply(nlevels_list,
                        FUN = function(x) c(0, rep(cumsum(eachobs*x$table)[-x$nlevels], each = 2) +
                                              freespace/(x$nlevels-1)/2*rep(c(-1, 1), times = x$nlevels-1), 1))
  # assign each obs
  obs_position <- Map(f = function(x, y){
    obs_position_each <- vector("list", length = length(x)/2)
    for (i in 1:(length(x)/2)) {
      obs_position_each[[i]] <- seq(from = x[2*i-1] + 0.5*eachobs,
                                    to = x[2*i] - 0.5*eachobs,
                                    length.out = y$table[i])
    }
    names(obs_position_each) <- names(y$table)
    obs_position_each},
    level_range, nlevels_list)

  return(obs_position)
}


# used to arrange the factor positions properly to match the ystart, the same observation.
# start_position = classification$num2fac + 1, for num2fac. Just to make it a little more general.
# end_position = start_position + 1, for num2fac. For fac2num, it's different, be careful.
# start_position = classification$num2fac + 2, end_position = start_position - 1, for fac2num.
# start_position is the one we need to refer to, end_position is the one we actually adjust.
# make sure the levels of factors are used correctly here(match the data, match the order). How was it decided before?
arrange_fac_by_ystart <- function(data_spread, start_position, end_position, obs_position) {
  # Map is usd here to deal with three lists in parallel
  arranged_postion <- Map(f = function(y, x, z) {
    # lapply uses like x[[i]] to extract sublist, assume Map is the same. And it works
    # is it safe to use name here? it should work in my example, and it does work in my example
    for (i in 1:length(z)) {
      x <- replace(x,
                   list = which(y == names(z)[i]),
                   z[[i]][rank(x[y == names(z)[i]])])
    }
    x
  },
  data_spread[ ,end_position, drop = FALSE],
  data_spread[ ,start_position, drop = FALSE],
  obs_position)

  arranged_postion
}


# a function to assign box in each level in each factor
# fac_table is a summary of the exsiting combination of factors
# level_range_2 is calculated as level_range inside assign_fac to get postions of levels
# names_to_group is the names of those factors, for convenience
assign_box <- function(fac_table, level_range_2, nlevels_list_con_fac, names_to_group) {

  box_position <- Map(f = function(x, y, z){
    box_proportion <- fac_table %>%
      group_by_(z) %>%
      mutate(proportion = Freq/sum(Freq)) %>%
      ungroup()
    box_position <- list()
    for(i in 1:(y$nlevels)){
      box_proportion_each <- box_proportion[box_proportion[z] == names(y$table)[i],]
      eachlevel <- x[c(2*i-1, 2*i)]
      eachbox <- eachlevel[1] + (eachlevel[2] - eachlevel[1])*cumsum(box_proportion_each$proportion)
      names(eachlevel) <- NULL
      box_position[[i]] <- c(eachlevel[1], eachbox)
      names(box_position)[i] <- names(y$table)[i]
    }
    box_position
  },
  level_range_2, nlevels_list_con_fac, names_to_group)

  box_position
}

# calculate the bandid (all possible combinations of factors) for the data
# assign observations to different band
# continuous_fac is the position of factor block
bandid <- function(data_spread, continuous_fac, nobs, nlevels_list_con_fac) {
  aa <- as.data.frame(lapply(data_spread[,continuous_fac + 1],
                             FUN =  function(x) as.numeric(x) - 1))
  dd <- vector()
  for (i in 1:length(continuous_fac)) {
    dd[i] <- nlevels_list_con_fac[[i]][[1]]
  }
  dd

  as.matrix(aa)%*%c(1, cumprod(dd)[-length(continuous_fac)]) + 1
}


# used to arrange observation positions within each bandid by ystart
# the previous arrange_fac_by_ystart arrange observation positions with in each level
# to properly adjust the lines to avoid overlap, to match the positions in a factor to the numeric value

# within each level, arrange the positions according to bandid from small to big, which is consistent with box_position

# notice that, to accomadate no numeric variable case, I changed the stat_position to a data vector
# since this function is only used to one start column and one end column(by lapply and Map to generalize)
# start_position indicates the numeric variable in data_spread, to be adjusted by
# end_position indicates the first factor variable in a factor block, or the last one when adjust backward
# obs_position is the return value of assign_fac(nlevels_list_con_fac, nobs)
# names_to_group for convience
arrange_fac_by_ystart_bandid <- function(data_spread, start_position, end_position,
                                         obs_position, fac_table, names_to_group) {

  # actually, we may not need Map, unless when we deal with more than one factor block
  # but we do deal with it
  arranged_position <- Map(f = function(y, x, z, l) {

    for (i in 1:length(z)) {
      aa <- fac_table[fac_table[l]==names(z[i]), ]
      aa$position_in_level <- cumsum(aa$Freq)
      bb <- aa$position_in_level

      for (j in 1:length(bb)) {
        position_in_box <- z[[i]][(bb[j]-aa$Freq[j] + 1):bb[j]]
        x <- replace(x,
                     list = which(data_spread$bandid == aa$bandid[j]),
                     values = position_in_box[rank(x[data_spread$bandid == aa$bandid[j]])]
        )
      }
    }
    x
  },
  # notice that, to accomadate no numeric variable case, I changed the stat_position to a data vector
  data_spread[, end_position, drop = FALSE],
  as.data.frame(start_position),
  obs_position,
  names_to_group)

  arranged_position
}


# summarize the steps for one factor block into a function
# input: continuous_fac, data_spread and bywhich, freespace, nobs
# bywhich: positions, indicate which numeric variable to use for adjustment, prior or after
# output: arranged_position_inband, or: data.frame for the final xstart, xend, ystart, yend

process_fac2fac <- function(data_spread, continuous_fac, start_position, freespace, nobs) {
  # nlevels_list for those "continued" factors, for further use
  nlevels_list_con_fac <- lapply(data_spread[, continuous_fac+1],
                                 FUN = function(x) list(nlevels = nlevels(x),
                                                        table = table(x)))

  # to calculate the exsiting combinations of levels, for further use
  fac_table <- as.data.frame(table(data_spread[, continuous_fac + 1]))
  fac_table <- fac_table[fac_table$Freq != 0, ]
  fac_table$bandid <- as.numeric(rownames(fac_table))

  # names of the factor variables, for convenience
  names_to_group <- names(data_spread[, continuous_fac + 1])
  # the calculation is used to calculate the position for levels within factors, used inside assign_fac()
  # freespace is 0.1
  level_range_2 <-  lapply(nlevels_list_con_fac,
                           FUN = function(x) c(0, rep(cumsum(0.1*x$table)[-x$nlevels], each = 2) +
                                                 freespace/(x$nlevels-1)/2*rep(c(-1, 1), times = x$nlevels-1), 1))

  # calculate the positions for boxes(within each level within each factor)
  box_position <- assign_box(fac_table, level_range_2, nlevels_list_con_fac, names_to_group)
  # postion for the observations in the factor block
  obs_position_2 <- assign_fac(nlevels_list_con_fac, nobs)

  # bandid for the original data_spread
  data_spread$bandid <- bandid(data_spread, continuous_fac, nobs, nlevels_list_con_fac)

  # match the positions in the factor block to the prior numeric variable, considering band
  ### we need to use similar method to work with the numeric variable after the factor block,
  # when there is no numeric variable prior to the factor block
  # input position adjusted for id column in data_spread
  # it also arranged the positions inside the factor block
  ### we need to come back for multiple factor blocks

  # note, start_position now is a data vector for factors to be adjusted by
  arranged_position_inband <- lapply(seq_along(continuous_fac),
                                     FUN = function(x) arrange_fac_by_ystart_bandid(
                                       data_spread,
                                       start_position = as.data.frame(start_position),
                                       continuous_fac[x] + 1,
                                       obs_position_2[x],
                                       fac_table,
                                       names_to_group = names(data_spread)[continuous_fac[x] + 1]))


  arranged_fac_block <- list(data_final_xstart_fac2fac = rep(continuous_fac[-length(continuous_fac)], each = nobs),
                             data_final_ystart_fac2fac = unlist(arranged_position_inband[-length(arranged_position_inband)]),
                             data_final_xend_fac2fac = rep(continuous_fac[-1], each = nobs),
                             data_final_yend_fac2fac = unlist(arranged_position_inband[-1]))
  arranged_fac_block
}


