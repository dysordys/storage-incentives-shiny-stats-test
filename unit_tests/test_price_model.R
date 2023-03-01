source("price_model.R")


testthat::test_that("adjustPrice", {

  Map(adjustPrice, 2^(9:12), redundancy = 0) |>
    Map(f = function(x) testthat::expect_equal(x, 1024))

  adjustPrice(currentPrice = 2^10, redundancy = 5) |>
    testthat::expect_equal(1024)

  adjustPrice(currentPrice = 2^11, redundancy = 5) |>
    testthat::expect_equal(1023 * 2^11 / 2^10)

})


testthat::test_that("accumPrice", {

  accumPrice(c(2, 3, 4), 2048) |>
    testthat::expect_equal(c(1027 * 2048 / 2^10, 1025 * 2054 / 2^10,
                             1024 * (1025 * 2054 / 2^10) / 2^10))

})
