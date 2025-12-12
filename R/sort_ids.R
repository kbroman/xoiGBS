#' Sort individual IDs numerically
#'
#' Sort individual IDs numerically, when they're of the form blah25 or blah25-2
#' Can also have dup info at the end, like blah25_dup1 or blah25_dup2
#'
#' @param ids Vector of individual IDs (as character string)
#'
#' @param decreasing If TRUE, sort in decreasing order (largest to smallest)
#'
#' @return Input IDs, sorted numerically
#'
#' @details We assume the ids have an initial non-numerial label,
#'     followed by a number, followed possibly by _dup and then
#'     another number. The IDs are sorted first by the initial
#'     non-numeric bit, then by the number, then by the duplicate
#'     number (with absense of `_dup` taken to be duplicate "0" and
#'     _dup without a number taken to be duplicate "1").
#'
#' @seealso `order_ids()`
#'
#' @examples
#' ids <- c("BCA70", "BCA1", "PWD1", "PWD2", "BCA2", "BCA75",
#'          "BCA70_dup", "PWD1_dup2", "PWD1_dup1")
#' order_ids(ids)
#' sort_ids(ids)
#' sort_ids(ids, decreasing=TRUE)
#'
#' @export

sort_ids <-
    function(ids, decreasing=FALSE)
{
    ids[order_ids(ids, decreasing=decreasing)]
}


#' Get numeric order of individual IDs
#'
#' Get numeric order of individual IDs, when they're of the form blah25 or blah25-2
#' Can also have dup info at the end, like blah25_dup1 or blah25_dup2
#'
#' @param ids Vector of individual IDs (as character string)
#'
#' @param decreasing If TRUE, get decreasing order (largest to smallest)
#'
#' @return Input IDs, sorted numerically
#'
#' @details We assume the ids have an initial non-numerial label,
#'     followed by a number, followed possibly by _dup and then
#'     another number. The IDs are sorted first by the initial
#'     non-numeric bit, then by the number, then by the duplicate
#'     number (with absense of `_dup` taken to be duplicate "0" and
#'     _dup without a number taken to be duplicate "1").
#'
#'
#' @seealso `sort_ids()`
#'
#' @examples
#' ids <- c("BCA70", "BCA1", "PWD1", "PWD2", "BCA2", "BCA75",
#'          "BCA70_dup", "PWD1_dup2", "PWD1_dup1")
#' order_ids(ids)
#' sort_ids(ids)
#' sort_ids(ids, decreasing=TRUE)
#'
#' @export

order_ids <-
    function(ids, decreasing=FALSE)
{
    initial_bit <- sapply(strsplit(ids, "[0-9]+"), "[", 1)

    dup <- rep(0, length(ids))
    if(any(grepl("_dup", ids))) {
        spl <- strsplit(ids, "_dup")
        spl_len <- sapply(spl, length)

        if(any(spl_len > 1)) {
            dup[spl_len > 1] <- sapply(spl[spl_len > 1], "[", 2)
        }

        dup[spl_len==1 & grepl("_dup", ids)] <- 1

        ids <- sapply(spl, "[", 1)
   }

    num_id <- sub("[A-Za-z]*", "", ids)
    num_id <- sub("\\-", ".", num_id)
    num_id <- as.numeric(num_id)

    order(initial_bit, num_id, dup, decreasing=decreasing)
}
