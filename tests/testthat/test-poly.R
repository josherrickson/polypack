test_that("basic poly creation", {
  p1 <- new("poly",
            1:3,
            powers = 3:1)
  expect_s4_class(p1, "poly")
  expect_length(p1, 3)
  expect_equal(trimws(capture.output(p1)),
               "x^3 + 2x^2 + 3x")

  expect_error(new("poly",
                   1:3,
                   powers = 1:3),
               "descending order")

  expect_error(new("poly",
                   1:4,
                   powers = 4),
               "differing lengths")
})

test_that("make_poly works", {
  p1 <- new("poly",
            1:3,
            powers = 3:1)
  p2 <- make_poly("3" = 1, "2" = 2, "1" = 3)
  expect_true(all.equal(p1, p2))
  expect_true(all.equal(p1@powers, p2@powers))
})
