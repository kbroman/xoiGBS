context("inferXOloc")

test_that("inferXOloc works", {

    map <- 1:20
    pr <- cbind(rep(0.5, 20), # no data
                rep(0.05, 20), # all homozygous
                rep(0.95, 20), # all heterozygous
                rep(c(0.05, 0.95), c(10, 10)), # crossover at center
                rep(c(0.05, 0.95, 0.05), c(5, 10, 5)), # two crossovers
                rep(c(0.05, 0.95, 0.5, 0.05), c(5, 8, 2, 5))) # two crossovers

    nullres <- matrix(nrow=0, ncol=3)

    result <- list(nullres, nullres, nullres,
                   cbind(est=10.5, left=10, right=11),
                   cbind(est=c(5.5, 15.5),
                         left=c(5,15),
                         right=c(6,16)),
                   cbind(est=c(5.5, 14.5),
                         left=c(5,13),
                         right=c(6,16)))

    expect_equal(inferXOloc(pr, map), result)

    expect_equal(inferXOloc(pr, map, 0.01, 0.99),
                 list(nullres, nullres, nullres, nullres, nullres, nullres))

    expect_equal(inferXOloc(pr, map, 0.1, 0.99),
                 list(nullres, nullres, nullres, nullres, nullres, nullres))

    expect_equal(inferXOloc(pr, map, 0.01, 0.9),
                 list(nullres, nullres, nullres, nullres, nullres, nullres))


})
