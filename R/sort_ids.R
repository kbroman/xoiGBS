#' Sort individual IDs numerically
#'
#' Sort individual IDs numerically, when they're of the form blah25 or blah25-2
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
    num_id <- sub("[A-Za-z]*", "", ids)
    num_id <- sub("\\-", ".", num_id)
    num_id <- as.numeric(num_id)

    order(num_id, decreasing=decreasing)
}
