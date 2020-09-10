context("Final GPCP Figures")


testthat::test_that("Basic structure for geom_pcp output", {
  df <- data.frame(var1 = 1:10,
                  var2 = 10:1,
                  fac1 = factor(rep(c("a", "b"), times = 5)),
                  fac2 = factor(rep(c("c", "d"), each = 5)))

  basic_plot <- ggplot(df) + geom_pcp(aes(vars = vars(var1, var2, fac1, fac2)))
  vdiffr::expect_doppelganger("numeric and factor variables", basic_plot)

  example_in_geom_pcp_help_file <- mtcars %>%
    mutate(cyl = factor(cyl),
           vs = factor(vs),
           am = factor(am),
           gear = factor(gear)) %>%
    ggplot(aes(vars = vars(cyl, vs:gear))) +
    geom_pcp(aes(color = vs), boxwidth = 0.2, resort = 2:3) +
    geom_pcp_box(boxwidth = 0.2) +
    geom_pcp_band(boxwidth = 0.2, resort = 2:3) +
    geom_pcp_text(boxwidth = 0.2)

  vdiffr::expect_doppelganger("example 1 in help file", example_in_geom_pcp_help_file)
})

