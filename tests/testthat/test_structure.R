## Libraries
library(antiplugr)
library(tibble)
library(tm)
library(lsa)
library(quanteda)
library(pdftools)
library(testthat)


test_that("correct format", {
  file1 <- system.file('pdf', 'summary_hansel_and_gretel.pdf', package = 'antiplugr')
  file2 <- system.file('pdf', 'grimm_hanse_and_gretel.pdf', package = 'antiplugr')
  sen1 <- "When four weeks had passed and Hansel was still thin, impatience overcame
  her, and she would wait no longer."
  expect_true(tibble::is.tibble(search_for(x = file1, sen = sen1)))
  expect_true(tibble::is.tibble(compare(file2, file1)))
})

test_that("correct dimension", {
  file1 <- system.file('pdf', 'summary_hansel_and_gretel.pdf', package = 'antiplugr')
  file2 <- system.file('pdf', 'grimm_hanse_and_gretel.pdf', package = 'antiplugr')
  sen1 <- "When four weeks had passed and Hansel was still thin, impatience overcame
                her, and she would wait no longer."
  expect_equal(dim(search_for(x = file1, sen = sen1)), c(1, 3))
  expect_equal(dim(compare(file1, file2)), c(5, 6))
})

test_that("correct entries", {
  file1 <- system.file('pdf', 'summary_hansel_and_gretel.pdf', package = 'antiplugr')
  file2 <- system.file('pdf', 'grimm_hanse_and_gretel.pdf', package = 'antiplugr')
  sen1 <- "When four weeks had passed and Hansel was still thin, impatience overcame
  her, and she would wait no longer."
  expect_equal(as.integer(search_for(x = file1, sen = sen1)[1, 3]), 19)
  expect_equal(as.integer(compare(file1, file2)[1, 5]), 5)
  expect_equal(as.integer(compare(file1, file2)[3, 6]), 16)
})
