context("sort_ids and order_ids")

test_that("sort_ids works", {

    input <- c("BCD1", "BCD10", "BCD11", "BCD12", "BCD13", "BCD14", "BCD15",
               "BCD16", "BCD17", "BCD18", "BCD19", "BCD2", "BCD20", "BCD21",
               "BCD22", "BCD23", "BCD24", "BCD25", "BCD26", "BCD27", "BCD28",
               "BCD29", "BCD3", "BCD30", "BCD31", "BCD4", "BCD5", "BCD5-2",
               "BCD6", "BCD7", "BCD8", "BCD9")

    output <- c(paste0("BCD", 1:5), "BCD5-2",
                paste0("BCD", 6:9), paste0("BCD", 10:31))

    expect_equal(sort_ids(input), output)

    expect_equal(sort_ids(input, decreasing=TRUE), rev(output))

})


test_that("order_ids works", {

    input <- c("BCD1", "BCD10", "BCD11", "BCD12", "BCD13", "BCD14", "BCD15",
               "BCD16", "BCD17", "BCD18", "BCD19", "BCD2", "BCD20", "BCD21",
               "BCD22", "BCD23", "BCD24", "BCD25", "BCD26", "BCD27", "BCD28",
               "BCD29", "BCD3", "BCD30", "BCD31", "BCD4", "BCD5", "BCD5-2",
               "BCD6", "BCD7", "BCD8", "BCD9")

    output <- c(1, 12, 23, 26:32, 2:11, 13:22, 24:25)

    expect_equal(order_ids(input), output)

    expect_equal(order_ids(input, decreasing=TRUE), rev(output))

})


test_that("order_ids works with duplicates", {

    input <- c("BCD1", "BCD10", "BCD11_dup", "BCD11", "BCD13_dup1", "BCD13_dup2", "BCD13",
               "BCD16", "BCD17", "BCD8", "BCD19", "BCD1_dup1")

    output <- c(1, 12, 10, 2, 4, 3, 7, 5, 6, 8, 9, 11)

    expect_equal(order_ids(input), output)

    expect_equal(order_ids(input, decreasing=TRUE), rev(output))

    expect_equal(sort_ids(input), input[output])
    expect_equal(sort_ids(input, decreasing=TRUE), rev(input[output]))

})
