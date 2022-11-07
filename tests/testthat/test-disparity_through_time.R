test_that("disparity_through_time() works", {
  skip_if_not_installed("dispRity")

  crinoids <- as.data.frame(demo_data$wright$matrix[[1]][, 1:2])
  crinoids$time <- "before extinction"
  crinoids$time[demo_data$wright$subsets$after$elements] <- "after extinction"
  crinoids$time <- factor(crinoids$time)
  attach(crinoids)
  gg <- disparity_through_time(time~V1*V2, groups = time, aspect = c(2,1), xlim = c(-.6,.6), ylim = c(-.5,.5),
                         col.regions = "lightgreen", col.point = c("red","blue"))
  expect_true(is(gg, "trellis"))
  expect_doppelganger_deeptime("disparity_through_time()", gg)
})
