#' Infer crossover locations
#'
#' Infer crossover locations from a matrix of genotype probabilities as produced by [calc_genoprob_gbs()]
#'
#' @param genoprob Matrix of genotype probabilities, positions x individuals, as produced by [calc_genoprob_gbs()]
#'
#' @param map Vector of marker positions, same length as `nrow(genoprob)`
#'
#' @param low Lower threshold; if probability below this threshold, infer homozygous
#'
#' @param high Higher threshold; if probaability above this threshold, infer heterozygous
#'
#' @return List of matrices (of length `ncol(genoprob)`, each having
#'     columns with estimated location and left and right interval
#'     endpoints. If no crossovers, it will be a matrix with no rows.
#'
#' @export

inferXOloc <-
    function(genoprob, map, low=0.1, high=0.9)
{
    stopifnot(is.matrix(genoprob))
    stopifnot(nrow(genoprob) == length(map))
    stopifnot(low >= 0, low < high, high <= 1)

    inferXOloc_sub <- function(gpvec, map, low, high) {
        keep <- !is.na(gpvec) & (gpvec <= low | gpvec >= high)
        if(!any(keep)) return(matrix(nrow=0, ncol=3)) # no data
        map <- map[keep]
        gpvec <- gpvec[keep]

        if(all(gpvec <= low) || all(gpvec >= high))
            return(matrix(nrow=0, ncol=3)) # no crossovers

        g <- gpvec
        g[gpvec <= low] <- 1
        g[gpvec >= high] <- 2

        d <- which(diff(g) != 0) # left crossover endpoint
        result <- cbind(est=rep(NA, length(d)),
                        left=map[d],
                        right=map[d+1])
        result[,1] <- (result[,2]+result[,3])/2
        return(result)
    }

    result <- lapply(1:ncol(genoprob), function(i) inferXOloc_sub(genoprob[,i], map, low, high))
    names(result) <- colnames(genoprob)
    result
}
