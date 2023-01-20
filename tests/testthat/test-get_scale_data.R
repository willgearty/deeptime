test_that("get_scale_data works", {
  expect_named(get_scale_data("stages"), c("name", "max_age", "min_age", "abbr", "color"))
  expect_equal(get_scale_data("stages"), deeptime::stages)
  expect_equal(get_scale_data("epochs"), deeptime::epochs)
  expect_equal(get_scale_data("periods"), deeptime::periods)
  expect_equal(get_scale_data("eras"), deeptime::eras)
  expect_equal(get_scale_data("eons"), deeptime::eons)
  expect_equal(nrow(get_scale_data("eons")), 3)
  expect_equal(get_scale_data("stages"), get_scale_data("s"))

  expect_error(get_scale_data("e"))
  expect_error(get_scale_data("international house of pancakes"))
  expect_named(get_scale_data("North American Land Mammal Ages"),
               c("name", "max_age", "min_age", "abbr", "color"))
})

test_that("getScaleData is deprecated", {
  lifecycle::expect_deprecated(getScaleData("stages"))
})
