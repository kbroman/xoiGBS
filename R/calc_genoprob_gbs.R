# calc_genoprob_gbs
#' Calculate genotype probabilities from GBS allele counts
#'
#' Calculate genotype probabilities using an HMM, from high-throughput sequencing data with counts of alleles at SNPs
#'
#' @param counts Three-dimensional array of counts, positions x individuals x two alleles
#'
#' @param map Vector of marker positions, same length as `nrow(counts)`, in cM
#'
#' @param error_prob1 Error probability for sequencing errors
#'
#' @param error_prob2 Error probability for locus errors
#'
#' @param map_function Map function for converting from cM distances to recombination fractions
#'
#' @param cores Number of CPU cores to use, for multi-core calculations
#'
#' @return Matrix of genotype probabilities, positions x individuals
#'
#' @importFrom Rcpp sourceCpp
#' @useDynLib xoiGBS, .registration=TRUE
calc_genoprob_gbs <-
    function(counts, map, error_prob1=0.002, error_prob2=0.002,
             map_function=c("haldane", "kosambi", "c-f", "morgan"),
             cores=1)
{
    map_function <- match.arg(map_function)
    if(map_function=="kosambi") {
        mf <- function(d) 0.5*tanh(d/50)
    }
    else if(map_function=="c-f") {
        imf.cf <- function(r) { r[r >= 0.5] <- 0.5-1e-14; 12.5*(log(1+2*r)-log(1-2*r))+25*atan(2*r) }

        mf <-
            function(d)
            {
                d[d >= 300] <- 300

                icf <- function(r,d)
                    imf.cf(r)-d

                sapply(d,function(a) {
                    if(a==0) return(0)
                    uniroot(icf, c(0,0.5-1e-14),d=a,tol=1e-12)$root })
            }
    }
    else if(map_function=="morgan") {
        mf <- function(d) sapply(d,function(a) min(a/100,0.5))
    }
    else { # haldane
        mf <- function(d) 0.5*(1-exp(-d/50))
    }

    stopifnot(is.array(counts) && length(dim(counts))==3 && dim(counts)[3]==2)
    stopifnot(nrow(counts) == length(map))
    stopifnot(!any(is.na(map)) && all(diff(map) > 0))

    rf <- mf(diff(map))

    pr <- .calc_genoprob_gbs(counts[,,1], counts[,,2], rf, error_prob1, error_prob2)

    # return just probability(AA), and make it pos x ind
    t(pr[1,,])
}
