context("Final GPCP Figures")


testthat::test_that("Basic structure for geom_pcp output", {
  df <- data.frame(var1 = 1:10,
                  var2 = 10:1,
                  fac1 = factor(rep(c("a", "b"), times = 5)),
                  fac2 = factor(rep(c("c", "d"), each = 5)))

  basic_plot <- ggplot(df) + geom_pcp(aes(vars = vars(var1, var2, fac1, fac2)))
  vdiffr::expect_doppelganger("numeric and factor variables", basic_plot)
})

