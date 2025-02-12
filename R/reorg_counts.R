#' reorganize count data
#'
#' Reorganize count data, from by-individual data frames to a list of 3-dimensional arrays.
#'
#' @param counts A list of data frames, one per individual, with six columns: chromosome, basepairs
#' position, allele1, allele2, readcount1, and readcount 2, as read with `read_counts()`.
#'
#' @param clean_chr If TRUE, remove "chr" from the chromosome names, so chr13 becomes just 13
#'
#' @param quiet If FALSE, print some tracing info
#'
#' @return A list containing `map` and `counts`, with the map being a list of vectors of Mbp positions, and the counts being a list of 3-dimensional arrays, position x individual x allele.
#'
#' @export

reorg_counts <-
    function(counts, clean_chr=TRUE, quiet=TRUE)
{
    # set of unique chromosomes
    chr <- unique(unlist(lapply(counts, function(a) unique(a[,1]))))

    # clean version of those names
    cchr <- ifelse(clean_chr, sub("^chr", "", chr), chr)

    ind <- names(counts)
    alleles <- sub("Count_", "", colnames(counts[[1]])[5:6])

    map <- output_counts <- vector("list", length(chr))
    names(map) <- names(output_counts) <- chr

    # loop over chromosomes
    for(this_chr in chr) {
        if(!quiet) message("- chr ", this_chr)

        # pull out that chromosome
        subcounts <- lapply(counts, function(a) a[a[,1]==this_chr,,drop=FALSE])

        # unique positions
        pos <- sort(unique(unlist(lapply(subcounts, "[[", 2))))

        map[[this_chr]] <- pos/1e6

        # object to contain the counts
        output_counts[[this_chr]] <- array(0, dim=c(length(pos), length(ind), 2))
        dimnames(output_counts[[this_chr]]) <- list(pos, ind, alleles)

        # fill in the counts
        for(i in ind) {
            output_counts[[this_chr]][as.character(subcounts[[i]][,2]) , i, 1] <- subcounts[[i]][,5]
            output_counts[[this_chr]][as.character(subcounts[[i]][,2]) , i, 2] <- subcounts[[i]][,6]
        }

    }

    names(map) <- names(output_counts) <- cchr

    list(map=map, counts=output_counts)
}
