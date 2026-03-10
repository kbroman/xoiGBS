context("trimXO")

test_that("trimXOloc works", {

    xo <- list(BCA117 = structure(logical(0), dim = c(0L, 3L)),
         BCA118 = structure(c(34.3566915,34.3578435, 138.600493, 34.356608, 34.357701, 133.620195, 34.356775,
                              34.357986, 143.580791), dim = c(3L, 3L), dimnames = list(NULL, c("est", "left", "right"))),
         BCA119 = structure(c(87.3194505, 85.821305, 88.817596), dim = c(1L, 3L), dimnames = list(NULL, c("est", "left", "right"))),
         BCA12 = structure(c(22.026335, 145.4004915, 22.021226, 145.302501, 22.031444, 145.498482), dim = 2:3,
                           dimnames = list(NULL, c("est", "left", "right"))))

    result <- xo
    result[["BCA118"]] <- xo[["BCA118"]][-(1:2),,drop=FALSE]

    expect_equal(trimXO(xo), result)
    expect_equal(trimXO(xo[-2]), result[-2])

})
