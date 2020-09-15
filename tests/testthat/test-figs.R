context("Final GPCP Figures")

# Manually change the following line for check
Sys.setenv(VDIFFR_RUN_TESTS = FALSE)

testthat::test_that("Basic plots", {
  df1 <- data.frame(var1 = 1:10,
                    var2 = 10:1,
                    fac1 = factor(rep(c("a", "b"), times = 5)),
                    fac2 = factor(rep(c("c", "d"), each = 5)),
                    fac3 = factor(rep(c("e", "f", "g", "h", "i"), each = 2)))

  basic_plot1 <- ggplot(df1) + geom_pcp(aes(vars = vars(var1, var2, fac1, fac2, fac3), color = fac3))
  vdiffr::expect_doppelganger("Basic geom_pcp", basic_plot1)

  basic_plot2 <- ggplot(df1) + geom_pcp_box(aes(vars = vars(var1, var2, fac1, fac2, fac3), color = fac3),
                                            boxwidth = 0.2)
  vdiffr::expect_doppelganger("Basic geom_pcp_box", basic_plot2)

  basic_plot3 <- ggplot(df1) + geom_pcp_label(aes(vars = vars(var1, var2, fac1, fac2, fac3), color = fac3),
                                              boxwidth = 0.2)
  vdiffr::expect_doppelganger("Basic geom_pcp_label", basic_plot3)

  basic_plot4 <- ggplot(df1) + geom_pcp_text(aes(vars = vars(var1, var2, fac1, fac2, fac3), color = fac3),
                                             boxwidth = 0.2)
  vdiffr::expect_doppelganger("Basic geom_pcp_text", basic_plot4)

  basic_plot5 <- ggplot(df1) + geom_pcp_band(aes(vars = vars(var1, var2, fac1, fac2, fac3), color = fac3, fill = fac3),
                                             boxwidth = 0.2)
  vdiffr::expect_doppelganger("Basic geom_pcp_band", basic_plot5)

  basic_plot6 <- ggplot(data = df1, mapping = aes(vars = vars(var1, var2, fac1, fac2, fac3), color = fac3)) +
    geom_pcp(boxwidth = 0.2) +
    geom_pcp_box(boxwidth = 0.2) +
    geom_pcp_label(boxwidth = 0.2) +
    geom_pcp_band(mapping = aes(fill = fac3), alpha = 0.2, boxwidth = 0.2)

  vdiffr::expect_doppelganger("Basic geom everything", basic_plot6)

})








