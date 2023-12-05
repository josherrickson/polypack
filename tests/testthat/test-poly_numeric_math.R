test_that("poly-numeric tests", {
  p1 <- new("poly",
            1:3,
            powers = 3:1)

  p3 <- make_poly("4" = -2, "1" = 7, "0" = 12)

  expect_s4_class(p1 + 4, "poly")
  expect_s4_class(p1 - 4, "poly")
  expect_s4_class(4 + p3, "poly")
  expect_s4_class(4 - p3, "poly")
  expect_s4_class(p1 * 3, "poly")
  expect_s4_class(3 * p1, "poly")
  expect_s4_class(p1 / 3, "poly")
  expect_error(8 / p1, "not supported")
})
