context("grabXO")

test_that("grabXO works", {

    xo <- list(BCA117 = structure(logical(0), dim = c(0L, 3L)),
         BCA118 = structure(c(34.3566915,34.3578435, 138.600493, 34.356608, 34.357701, 133.620195, 34.356775,
                              34.357986, 143.580791), dim = c(3L, 3L), dimnames = list(NULL, c("est", "left", "right"))),
         BCA119 = structure(c(87.3194505, 85.821305, 88.817596), dim = c(1L, 3L), dimnames = list(NULL, c("est", "left", "right"))),
         BCA12 = structure(c(22.026335, 145.4004915, 22.021226, 145.302501, 22.031444, 145.498482), dim = 2:3,
                           dimnames = list(NULL, c("est", "left", "right"))))

    result <- c(BCA1181=34.3566915,BCA1182=34.3578435, BCA1183=138.600493,
                BCA119.est=87.3194505,
                BCA121=22.026335, BCA122=145.4004915)

    expect_equal(grabXO(xo), result)

})
