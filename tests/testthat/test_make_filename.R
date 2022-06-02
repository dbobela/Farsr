library(testthat)

test_that("make_filename outputs correct filename",
          expect_equal(make_filename(2013), "accident_2013.csv.bz2")
)
