library(testthat)

test_that("note_name_to_freq works", {
  expect_equal(note_name_to_freq("A", 0), 440)
  expect_equal(note_name_to_freq("A", -1), 220)
  
})
