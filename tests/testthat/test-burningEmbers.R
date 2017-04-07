context("burningEmbers object creation")
x <- burningEmbers()
test_that("burningEmbers object is created correctly with default values",
          {
            expect_is(x,   "burningEmbers")
            expect_equal(x$colRange, colRangeDefault, tolerance = 0.000001)
          })

x <- burningEmbers(
  bot = c(1,     -30, 1.3, -23.5),
  top = c(22.4,  -5,  43,  -22.4),
  colNames = c("Fritz", "Eddie")
)
test_that("burningEmbers object is created correctly",
          {
            expect_equal(class(x),   "burningEmbers")
            expect_equal(x$colRange, colRangeVal)
          })
