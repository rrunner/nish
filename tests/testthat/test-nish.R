context("nish theme")

test_that("Palettes", {

  # regular expression for hex colours
  re <- "#[a-fA-F0-9]{6}"

  expect_equal(sum(stringr::str_count(nish_colours, re)), 20)
  expect_equal(sum(stringr::str_count(nish_blue_pal_col, re)), 7)
  expect_equal(sum(stringr::str_count(nish_grey_pal_col, re)), 7)
  expect_equal(sum(stringr::str_count(nish_pink_pal_col, re)), 7)
  expect_identical(nish_colours, nish_mixed_pal_col)
  expect_identical(
    nish_pos_zero_neg1,
    c(nish_colours[17], "#1d1d1b", nish_colours[16])
  )
  expect_identical(
    nish_pos_zero_neg2,
    c(nish_colours[1], "#1d1d1b", nish_colours[4])
  )
  expect_identical(
    nish_lt,
    c("solid", "dashed", "solid", "dotted", "dashed", "solid", "dotted")
  )
  expect_equal(length(list_of_pals), 7)
  expect_equal(
    names(list_of_pals),
    c("blue", "grey", "pink", "mixed", "pn1", "pn2", "lt")
  )
  expect_equal(length(manual_palettes), 7)
  expect_true(all(purrr::map_lgl(manual_palettes, is.function)))

  rm(re)
})

test_that("Discrete scales", {

  # only test if exported functions are functions...
  expect_true(is.function(scale_colour_nish_blue))
  expect_true(is.function(scale_color_nish_blue))
  expect_true(is.function(scale_fill_nish_blue))
  expect_true(is.function(scale_colour_nish_grey))
  expect_true(is.function(scale_color_nish_grey))
  expect_true(is.function(scale_fill_nish_grey))
  expect_true(is.function(scale_colour_nish_pink))
  expect_true(is.function(scale_color_nish_pink))
  expect_true(is.function(scale_fill_nish_pink))
  expect_true(is.function(scale_colour_nish_mixed))
  expect_true(is.function(scale_color_nish_mixed))
  expect_true(is.function(scale_fill_nish_mixed))
  expect_true(is.function(scale_colour_nish_pos_neg))
  expect_true(is.function(scale_color_nish_pos_neg))
  expect_true(is.function(scale_fill_nish_pos_neg))
  expect_true(is.function(scale_colour_nish_pos_neg2))
  expect_true(is.function(scale_color_nish_pos_neg2))
  expect_true(is.function(scale_fill_nish_pos_neg2))
  expect_true(is.function(scale_linetype_nish))
})

test_that("Continuous scales", {

  # only test if exported functions are functions...
  expect_true(is.function(scale_colour_gradient_nish_blue))
  expect_true(is.function(scale_color_gradient_nish_blue))
  expect_true(is.function(scale_fill_gradient_nish_blue))
  expect_true(is.function(scale_colour_gradient_nish_grey))
  expect_true(is.function(scale_color_gradient_nish_grey))
  expect_true(is.function(scale_fill_gradient_nish_grey))
  expect_true(is.function(scale_colour_gradient_nish_pink))
  expect_true(is.function(scale_color_gradient_nish_pink))
  expect_true(is.function(scale_fill_gradient_nish_pink))
  expect_true(is.function(scale_colour_gradient2_nish_pos_neg))
  expect_true(is.function(scale_color_gradient2_nish_pos_neg))
  expect_true(is.function(scale_fill_gradient2_nish_pos_neg))
  expect_true(is.function(scale_colour_gradient2_nish_pos_neg2))
  expect_true(is.function(scale_color_gradient2_nish_pos_neg2))
  expect_true(is.function(scale_fill_gradient2_nish_pos_neg2))
  expect_true(is.function(scale_colour_gradientn_nish))
  expect_true(is.function(scale_color_gradientn_nish))
  expect_true(is.function(scale_fill_gradientn_nish))
})

test_that("Themes", {

  # test if themes are... themes
  expect_true(ggplot2::is.theme(theme_nish_base()))
  expect_true(ggplot2::is.theme(theme_nish_blue()))
  expect_true(ggplot2::is.theme(theme_nish_pink()))
  expect_true(ggplot2::is.theme(theme_nish_white()))
})
