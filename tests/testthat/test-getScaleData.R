test_that("getScaleData works", {
  expect_named(getScaleData("stages"), c("name", "max_age", "min_age", "abbr", "color"))
  expect_equal(getScaleData("stages"), deeptime::stages)
  expect_equal(getScaleData("epochs"), deeptime::epochs)
  expect_equal(getScaleData("periods"), deeptime::periods)
  expect_equal(getScaleData("eras"), deeptime::eras)
  expect_equal(getScaleData("eons"), deeptime::eons)
  expect_equal(nrow(getScaleData("eons")), 3)
  expect_equal(getScaleData("stages"), getScaleData("s"))

  expect_error(getScaleData("e"))
  expect_error(getScaleData("international house of pancakes"))
  expect_named(getScaleData("North American Land Mammal Ages"),
               c("name", "max_age", "min_age", "abbr", "color"))
})
