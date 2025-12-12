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
#' @seealso `order_ids()`
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
#' @seealso `sort_ids()`
#'
#' @export

order_ids <-
    function(ids, decreasing=FALSE)
{
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

    order(num_id, dup, decreasing=decreasing)
}
