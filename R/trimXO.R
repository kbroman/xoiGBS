#' Trim tight double crossovers from crossover information
#'
#' Trim tight double crossovers from crossover information
#'
#' @param xoloc Either a matrix, or a list of matrices, of crossover locations, as output from [inferXOloc()].
#'
#' @param mind Minimum allowed distance between crossovers
#'
#' @return Object like that input, but with double-crossovers within `mind` of each other removed
#'
#' @export

trimXO <-
    function(xoloc, mind=1)
{
    if(!is.matrix(xoloc)) {
        if(!is.list(xoloc)) stop("xoloc should be a matrix or a list of matrices")
        return(lapply(xoloc, trimXO, mind))
    }

    if(nrow(xoloc) < 2) return(xoloc)

    while(nrow(xoloc) >= 2 & any(diff(xoloc[,1])<mind)) {

        d <- diff(xoloc[,1])
        wh <- which.min(d)
        if(d[wh] < mind) {
            xoloc <- xoloc[-c(wh, wh+1), , drop=FALSE]
        }
    }

    return(xoloc)
}
