#' Grab all crossover locations
#'
#' Grab all crossover locations, as a vector
#'
#' @param xoloc List of matrices, of crossover locations, as output from [inferXOloc()].
#'
#' @return Vector of crossover locations
#'
#' @export

grabXO <-
    function(xoloc)
{
    stopifnot(is.list(xoloc))
    if(is.list(xoloc[[1]])) return(lapply(xoloc, grabXO))

    result <- unlist(lapply(xoloc, function(a) if(nrow(a) > 0) return(a[,1]) else return(NA)))
    result[!is.na(result)]
}


#' Grab all double-crossover locations
#'
#' Grab all double-crossover locations, as a matrix with two columns, for cases of exactly 2 crossovers
#'
#' @param xoloc List of matrices, of crossover locations, as output from [inferXOloc()].
#'
#' @return Matrix with locations of pair of crossovers, when there are exactly two
#'
#' @export

grab2XO <-
    function(xoloc)
{
    stopifnot(is.list(xoloc))
    if(is.list(xoloc[[1]])) return(lapply(xoloc, grab2XO))

    result <- lapply(xoloc, function(a) if(nrow(a) == 2) return(a[,1]) else return(NULL))
    result <- result[!sapply(result, is.null)]
    if(length(result)==0) return(matrix(ncol=2, nrow=0))
    do.call("rbind", result)
}
