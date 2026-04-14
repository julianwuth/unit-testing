# Snapshot testing for plots
test_that("summary plot works", {
  vec <- c("a" = 1, "b" = 4, "c" = 6)
  vdiffr::expect_doppelganger("Summary plot", plot_summary(vec))
})
