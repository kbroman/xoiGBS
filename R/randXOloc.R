#' Randomizing estimated crossover locations.
#'
#' Randomizing estimated crossover
#' locations, uniform within their intervals.
#'
#' @param xoloc Either a matrix, or a list of matrices, of crossover locations, as output from [inferXOloc()].
#'
#' @return Object like that input, but with estimated crossover locations randomized.
#'
#' @export

randXOloc <-
    function(xoloc)
{
    if(!is.matrix(xoloc)) {
        if(!is.list(xoloc)) stop("xoloc should be a matrix or a list of matrices")
        return(lapply(xoloc, randXOloc))
    }

    if(nrow(xoloc) < 1) return(xoloc)

    xoloc[,1] <- runif(nrow(xoloc), xoloc[,2], xoloc[,3])

    xoloc
}
