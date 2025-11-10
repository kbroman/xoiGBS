# calc_genoprob_gbs
#' Calculate genotype probabilities from GBS allele counts
#'
#' Calculate genotype probabilities using an HMM, from high-throughput sequencing data with counts of alleles at SNPs
#'
#' @param counts Three-dimensional array of counts, positions x individuals x two alleles, with alleles A and B
#' where we are expecting genotypes AA and AB
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
#' @return Matrix of genotype probabilities, positions x individuals, for Pr(het)
#'
#' @export
#' @importFrom stats uniroot
#' @importFrom Rcpp sourceCpp
#' @importFrom parallel detectCores makeCluster stopCluster parLapply mclapply splitIndices
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

    cores <- setup_cluster(cores, quiet=TRUE)
    group <- parallel::splitIndices(ncol(counts), n_cores(cores))
    group <- group[sapply(group, length)>0] # drop empty groups
    groupindex <- seq(along=group)

    # function for cluster
    cluster_func <- function(index) {
        ind <- group[[index]]
        if(length(ind)==0) return(NULL) # no individuals in this group

        # as.matrix() makes sure that if just 1 ind, still a matrix
        pr <- .calc_genoprob_gbs(as.matrix(counts[,ind,1]), as.matrix(counts[,ind,2]), rf, error_prob1, error_prob2)

        if(length(ind)==1) {
            pr <- cbind(pr[2,,])
        }
        else {
            pr <- .calc_genoprob_gbs(counts[,ind,1], counts[,ind,2], rf, error_prob1, error_prob2)
            pr <- t(pr[2,,])
        }

        rownames(pr) <- rownames(counts)
        colnames(pr) <- colnames(counts)[ind]

        pr
    }

    if(n_cores(cores)==1) {
        pr <- cluster_func(1)
    } else {
        pr <- cluster_lapply(cores, groupindex, cluster_func)
        pr <- do.call("cbind", pr)
    }

    # return just probability(AA), and make it pos x ind
    pr
}
