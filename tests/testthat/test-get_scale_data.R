test_that("get_scale_data works", {
  # check that package data is being loaded correctly
  expect_named(get_scale_data("stages"),
               c("name", "max_age", "min_age", "abbr", "color", "lab_color"))
  expect_identical(get_scale_data("stages"), deeptime::stages)
  expect_identical(get_scale_data("periods"), deeptime::periods)
  expect_identical(get_scale_data("epochs"), deeptime::epochs)
  expect_identical(get_scale_data("eras"), deeptime::eras)
  expect_identical(get_scale_data("eons"), deeptime::eons)

  # check errors
  expect_error(get_scale_data("e"), "multiple")

  skip_if_offline(host = "macrostrat.org")
  # check that package data matches data from Macrostrat
  expect_identical(get_scale_data("stages"),
                   get_scale_data("international ages"))
  expect_identical(get_scale_data("epochs"),
                   get_scale_data("international epochs"))
  expect_identical(get_scale_data("periods"),
                   get_scale_data("international periods"))
  expect_identical(get_scale_data("eras"),
                   get_scale_data("international eras"))
  expect_identical(get_scale_data("eons"),
                   get_scale_data("international eons"))

  # check other Macrostrat timescales
  expect_no_error(get_scale_data("mammal"))
  expect_no_error(get_scale_data("australia"))

  # check Macrostrat errors
  expect_error(get_scale_data("international house of pancakes"),
               "does not match")
  expect_error(get_scale_data("international"), "multiple")
})
