# calc_genoprob_gbs
#' @importFrom Rcpp sourceCpp
#' @useDynLib xoiGBS, .registration=TRUE

calc_genoprob_gbs <-
    function(counts, map, error_prob1=0.002, error_prob2=0.002,
             map_function, cores=1)
{

#    - make sure stuff is the right length
#
#    - omit markers that aren't in the map
#
#    - fill in 0 counts for pseudomarkers
#
#    - turn map distances into rec fracs
#
#    - using multiple cores

# pr <- .calc_genoprob_gbs(countsA, countsB, rec_frac, error_prob1, error_prob2)
# counts ind x mar

#    - just return prob(het) as a matrix

}
