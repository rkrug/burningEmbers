library(burningEmbers)
context("plot.burningEmbers")

x <- burningEmbers()
test_that("plot.burningEmbers with default",
          {
            expect_equal(plot(x), readRDS(
              system.file("testdata",
                          "plot.burningEmbers.def.rds",
                          package = "burningEmbers")
            ))
          })
##
x <- burningEmbers(bot = c(1, -2, 3, -3),
                   top = c(2,  5, 4, -2))
test_that("plot.burningEmbers with values",
          {
            expect_equal(plot(x),
                         readRDS(
                           system.file("testdata",
                                       "plot.burningEmbers.val1.rds",
                                       package = "burningEmbers")
                         ))
            expect_equal(plot(x,
                              main = "This is a header",
                              ylim = c(-1, 3)),
                         readRDS(
                           system.file("testdata",
                                       "plot.burningEmbers.val2.rds",
                                       package = "burningEmbers")
                         ))
          })

# Need checks for graphs!!!
# plot(x, main = "This is a header")
